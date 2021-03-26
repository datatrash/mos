mod analysis;
mod config_extractor;
mod config_validator;
mod opcodes;
mod program_counter;

pub use analysis::*;
pub use program_counter::*;

use crate::core::codegen::config_validator::ConfigValidator;
use crate::core::codegen::opcodes::get_opcode_bytes;
use crate::core::parser::code_map::Span;
use crate::core::parser::{
    AddressModifier, AddressingMode, DataSize, Expression, ExpressionFactor, ExpressionFactorFlags,
    Identifier, IdentifierPath, ImportArgs, Located, Mnemonic, ParseTree, SpecificImportArg, Token,
    VariableType,
};
use crate::errors::{MosError, MosResult};
use fs_err as fs;
use itertools::Itertools;
use once_cell::sync::OnceCell;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, Range};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

pub struct CodegenOptions {
    pub pc: ProgramCounter,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            pc: ProgramCounter::new(0xc000),
        }
    }
}

pub struct Segment {
    pc: ProgramCounter,
    data: Vec<u8>,
    range: Range<usize>,
    options: SegmentOptions,
}

pub struct SegmentOptions {
    pub initial_pc: ProgramCounter,
    pub write: bool,
    pub target_address: ProgramCounter,
}

impl Default for SegmentOptions {
    fn default() -> Self {
        Self {
            initial_pc: 0x2000.into(),
            write: true,
            target_address: 0x2000.into(),
        }
    }
}

static EMPTY_DATA: OnceCell<Vec<u8>> = OnceCell::new();

impl Segment {
    fn new(options: SegmentOptions) -> Self {
        let pc = options.initial_pc;
        let range = pc.as_empty_range();

        Self {
            pc,
            data: vec![],
            range,
            options,
        }
    }

    fn reset(&mut self) {
        self.pc = self.options.initial_pc;
        self.range = self.pc.as_empty_range();
        self.data = vec![];
    }

    pub fn target_pc(&self) -> ProgramCounter {
        ((self.pc.as_i64() + self.target_offset()) as usize).into()
    }

    pub fn options(&self) -> &SegmentOptions {
        &self.options
    }

    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    fn target_offset(&self) -> i64 {
        self.options.target_address.as_i64() - self.options.initial_pc.as_i64()
    }

    pub fn range_data(&self) -> &[u8] {
        if self.data.is_empty() {
            EMPTY_DATA.get_or_init(Vec::new)
        } else {
            &self.data[self.range()]
        }
    }

    fn emit(&mut self, bytes: &[u8]) -> bool {
        let start = self.pc;
        let end = self.pc + bytes.len();
        if start.as_usize() > 0xffff || end.as_usize() > 0x10000 {
            return false;
        }

        if start.as_usize() < self.range.start || self.data.is_empty() {
            self.range.start = start.as_usize();
            log::trace!("Extending start of range to: {}", self.range.start);
        }
        if end.as_usize() > self.range.end || self.data.is_empty() {
            self.range.end = end.as_usize();
            log::trace!("Extending end of range to: {}", self.range.end);
        }

        if self.data.is_empty() {
            self.data = [0; 65536].into();
        }

        self.data.splice(*start..*end, bytes.to_vec());
        self.pc = end;

        true
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolData {
    MacroDefinition(MacroDefinition),
    Number(i64),
}

impl From<i64> for SymbolData {
    fn from(val: i64) -> Self {
        Self::Number(val)
    }
}

impl From<MacroDefinition> for SymbolData {
    fn from(val: MacroDefinition) -> Self {
        Self::MacroDefinition(val)
    }
}

impl SymbolData {
    pub fn as_i64(&self) -> i64 {
        match &self {
            SymbolData::Number(val) => *val,
            _ => panic!(),
        }
    }

    pub fn as_macro_definition(&self) -> &MacroDefinition {
        match &self {
            SymbolData::MacroDefinition(m) => m,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolType {
    Label,
    MacroArgument,
    Variable,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub data: SymbolData,
    pub ty: SymbolType,
    pub read_only: bool,
}

impl Symbol {
    fn label<V: Into<SymbolData>>(data: V) -> Self {
        Self {
            data: data.into(),
            ty: SymbolType::Label,
            read_only: false,
        }
    }

    fn constant<V: Into<SymbolData>>(data: V) -> Self {
        Self {
            data: data.into(),
            ty: SymbolType::Variable,
            read_only: true,
        }
    }

    fn macro_arg<V: Into<SymbolData>>(data: V) -> Self {
        Self {
            data: data.into(),
            ty: SymbolType::MacroArgument,
            read_only: true,
        }
    }

    fn variable<V: Into<SymbolData>>(data: V) -> Self {
        Self {
            data: data.into(),
            ty: SymbolType::Variable,
            read_only: false,
        }
    }
}

pub type SymbolTable = HashMap<IdentifierPath, Symbol>;

#[derive(Clone, Debug, PartialEq)]
pub struct MacroDefinition {
    id: Located<Identifier>,
    args: Vec<Located<Identifier>>,
    block: Vec<Token>,
}

pub struct CodegenContext {
    tree: Arc<ParseTree>,

    analysis: Analysis,

    segments: HashMap<Identifier, Segment>,
    current_segment: Option<Identifier>,

    symbols: SymbolTable,
    undefined: Rc<RefCell<HashSet<UndefinedSymbol>>>,
    suppress_undefined_symbol_registration: bool,
    current_scope: IdentifierPath,

    exports: HashMap<IdentifierPath, IdentifierPath>,
    export_filter: Option<Vec<SpecificImportArg>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct UndefinedSymbol {
    scope: IdentifierPath,
    id: IdentifierPath,
    span: Span,
}

impl CodegenContext {
    fn new(tree: Arc<ParseTree>) -> Self {
        let analysis = Analysis::new(tree.clone());

        Self {
            tree,
            analysis,
            segments: HashMap::new(),
            current_segment: None,
            symbols: HashMap::new(),
            undefined: Rc::new(RefCell::new(HashSet::new())),
            suppress_undefined_symbol_registration: false,
            current_scope: IdentifierPath::empty(),
            exports: HashMap::new(),
            export_filter: None,
        }
    }

    pub fn analysis(&self) -> &Analysis {
        &self.analysis
    }

    pub fn segments(&self) -> &HashMap<Identifier, Segment> {
        &self.segments
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    fn after_pass(&mut self) {
        // Register symbols for all segments
        let path: IdentifierPath = "segments".into();
        let segs = self
            .segments
            .iter()
            .map(|(name, segment)| (path.join(name), segment.range()))
            .collect_vec();
        for (path, range) in segs {
            let _ = self.add_symbol(
                None,
                path.join("start"),
                Symbol::variable(range.start as i64),
            );
            let _ = self.add_symbol(None, path.join("end"), Symbol::variable(range.end as i64));
        }
    }

    fn next_pass(&mut self) {
        log::trace!("\n* NEXT PASS *");

        log::trace!("Currently available symbols:");
        for (key, value) in self.symbols.iter().sorted_by_key(|(k, _)| k.to_string()) {
            log::trace!("`--> {}: {:?}", key, value);
        }

        self.segments.values_mut().for_each(|s| s.reset());
        self.undefined.borrow_mut().clear();
    }

    fn try_current_target_pc(&self) -> Option<ProgramCounter> {
        self.try_current_segment().map(|seg| seg.target_pc())
    }

    fn try_current_segment(&self) -> Option<&Segment> {
        self.current_segment
            .as_ref()
            .map(|cs| self.segments.get(cs))
            .flatten()
    }

    fn try_current_segment_mut(&mut self) -> Option<&mut Segment> {
        if let Some(cs) = &self.current_segment {
            self.segments.get_mut(cs)
        } else {
            None
        }
    }

    fn add_symbol<S: Into<Option<Span>>, I: Into<IdentifierPath>>(
        &mut self,
        span: S,
        id: I,
        value: Symbol,
    ) -> MosResult<()> {
        let span = span.into();
        let id = id.into();
        let path = self.current_scope.join(&id);
        log::trace!("Inserting symbol: {} with value '{:?}'", path, value);

        // If the symbol already existed but with a different value,
        // mark it as undefined so we will trigger another pass
        let mut should_mark_as_undefined = false;
        match self.symbols.entry(path) {
            Entry::Occupied(e) => {
                let old_symbol = e.get();

                if old_symbol.ty != value.ty
                    || old_symbol.read_only != value.read_only
                    || (old_symbol != &value && old_symbol.read_only)
                {
                    let span = span.expect("no span provided");
                    return self.error(span, format!("cannot redefine symbol: {}", &id));
                }

                if old_symbol != &value {
                    log::trace!(
                        "`--> Symbol already existed, but has changed. Old value was: {:?}",
                        old_symbol
                    );

                    should_mark_as_undefined = match &value.ty {
                        SymbolType::Label => true,
                        SymbolType::Variable => false,
                        SymbolType::MacroArgument => false,
                    };
                }

                let e = e.into_mut();
                *e = value;
                e
            }
            Entry::Vacant(e) => e.insert(value),
        };

        if should_mark_as_undefined {
            self.mark_undefined(span.expect("expected a span"), id);
        }

        Ok(())
    }

    fn remove_symbol<I: Into<IdentifierPath>>(&mut self, id: I) {
        let id = id.into();
        let path = self.current_scope.join(&id);
        assert_eq!(self.symbols.contains_key(&path), true);
        self.symbols.remove(&path);
    }

    fn get_symbol(
        &self,
        current_scope: &IdentifierPath,
        id: &IdentifierPath,
    ) -> Option<(IdentifierPath, &Symbol)> {
        log::trace!("Trying to get symbol value: {}", id);
        let mut cur_scope = current_scope.clone();
        loop {
            let path = cur_scope.join(id).canonicalize();
            log::trace!("`--> Querying: {}", path);
            if let Some(symbol) = self.symbols.get(&path) {
                log::trace!("`--> Found");
                return Some((path, symbol));
            }

            log::trace!("`--> Querying exports: {}", path);
            if let Some(exported) = self.exports.get(&path) {
                log::trace!("`--> Was exported as: {}", exported);
                return self.get_symbol(current_scope, exported);
            }

            if cur_scope.is_empty() {
                break;
            }

            cur_scope.pop();
        }

        None
    }

    fn get_symbol_data<I: Into<IdentifierPath>>(&self, span: Span, id: I) -> Option<&SymbolData> {
        let id = id.into();
        match self.get_symbol(&self.current_scope, &id) {
            Some((_, s)) => Some(&s.data),
            None => {
                // Undefined symbol!
                if !self.suppress_undefined_symbol_registration {
                    self.mark_undefined(span, id);
                }

                None
            }
        }
    }

    fn mark_undefined<I: Into<IdentifierPath>>(&self, span: Span, id: I) {
        if !self.suppress_undefined_symbol_registration {
            let id = id.into();
            log::trace!(
                "Marking '{}' as undefined within scope: '{}'.",
                &id,
                self.current_scope
            );
            self.undefined.borrow_mut().insert(UndefinedSymbol {
                scope: self.current_scope.clone(),
                id,
                span,
            });
        }
    }

    fn error<T, M: Into<String>>(&self, span: Span, message: M) -> MosResult<T> {
        let location = self.tree.code_map.look_up_span(span);
        Err(MosError::Codegen {
            location,
            message: message.into(),
        })
    }

    fn emit(&mut self, span: Span, bytes: &[u8]) -> MosResult<()> {
        match &self.current_segment {
            Some(name) => {
                let segment = self.segments.get_mut(&name).unwrap();
                log::trace!(
                    "Emitting to segment '{}' {}: {:?}",
                    name,
                    segment.pc,
                    &bytes
                );
                if segment.emit(bytes) {
                    Ok(())
                } else {
                    self.error(span, format!("segment '{}' is out of range", name))
                }
            }
            None => {
                /*log::trace!(
                    "Not emitting, since there is no current segment: {:?}",
                    &bytes
                );*/
                Ok(())
            }
        }
    }

    fn emit_tokens(&mut self, tokens: &[Token]) -> MosResult<()> {
        for token in tokens {
            self.emit_token(token)?;
        }
        Ok(())
    }

    fn emit_token(&mut self, token: &Token) -> MosResult<()> {
        match token {
            Token::Align { value, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    let align = self.evaluate_expression(value)?;
                    let padding = (align - (pc.as_i64() % align)) as usize;
                    let mut bytes = Vec::new();
                    bytes.resize(padding, 0u8);
                    self.emit(value.span, &bytes)?;
                }
            }
            Token::Braces { block, scope } => {
                self.with_scope(scope, |s| s.emit_tokens(&block.inner))?;
            }
            Token::Data { values, size } => {
                let exprs = values.iter().map(|(expr, _comma)| expr).collect_vec();
                for expr in exprs {
                    let value = self.evaluate_expression(expr)?;
                    let bytes = match &size.data {
                        DataSize::Byte => vec![value as u8],
                        DataSize::Word => (value as u16).to_le_bytes().to_vec(),
                        DataSize::Dword => (value as u32).to_le_bytes().to_vec(),
                    };
                    self.emit(expr.span, &bytes)?;
                }
            }
            Token::Definition {
                id,
                value: Some(token),
                ..
            } => {
                if let Token::Config(block) = &token.deref() {
                    let kvps: Vec<(&Located<String>, &Located<Token>)> = block
                        .inner
                        .iter()
                        .filter_map(|token| match token {
                            Token::ConfigPair { key, value, .. } => Some((key, value.deref())),
                            _ => None,
                        })
                        .collect();

                    match id.data.as_str() {
                        "segment" => {
                            let extractor = ConfigValidator::new()
                                .required("name")
                                .required("start")
                                .allowed("pc")
                                .allowed("write")
                                .extract(self.tree.clone(), id.span, &kvps)?;

                            let mut opts = SegmentOptions::default();
                            let name = extractor.get_identifier(self, "name")?;
                            opts.initial_pc = extractor.get_i64(self, "start")?.into();
                            if let Some(write) = extractor.try_get_string("write") {
                                opts.write = write.parse()?;
                            }
                            match extractor.try_get_i64(self, "pc")? {
                                Some(target) => opts.target_address = target.into(),
                                None => opts.target_address = opts.initial_pc,
                            }

                            self.segments.insert(name.clone(), Segment::new(opts));
                            if self.current_segment.is_none() {
                                self.current_segment = Some(name);
                            }
                        }
                        _ => {
                            return self.error(id.span, "Unknown definition type");
                        }
                    }
                }
            }
            Token::Export { id, as_, .. } => {
                let target = match as_ {
                    Some((_, target_id)) => target_id.data.clone(),
                    None => id.data.clone(),
                };
                let id = self.current_scope.join(&id.data);

                // See if we are allowed to export this (because it was requested during import)
                // and, if so, if we need to rename the export target
                let target = self.current_scope.parent().join(&target);
                let target = if let Some(filter) = &self.export_filter {
                    filter.iter().find_map(|f| {
                        if target.has_parent(&f.path.data) {
                            match &f.as_ {
                                Some((_, target_id)) => Some(target_id.data.clone()),
                                None => Some(f.path.data.clone()),
                            }
                        } else {
                            None
                        }
                    })
                } else {
                    Some(target)
                };

                if let Some(target) = target {
                    log::trace!("Exporting {} as: {}", id, target);
                    self.exports.insert(target, id);
                }
            }
            Token::If {
                value, if_, else_, ..
            } => {
                let value = self.evaluate_expression(value)?;
                if value != 0 {
                    self.emit_tokens(&if_.inner)?;
                } else if let Some(e) = else_ {
                    self.emit_tokens(&e.inner)?;
                }
            }
            Token::Import {
                args,
                import_scope,
                filename,
                block,
                resolved_path,
                ..
            } => {
                let imported_file = self.tree.get_file(resolved_path);
                let imported_file_tokens = imported_file.tokens.clone();

                // Make the filename a definition by itself, allowing the user to follow the definition
                let def = self
                    .analysis
                    .get_or_create_definition_mut(DefinitionType::Filename(resolved_path.clone()));
                def.set_location(imported_file.file.span);
                def.add_usage(filename.span);

                let prev_exports = self.exports.clone();
                self.exports.clear();
                self.with_scope(import_scope, |s| {
                    if let Some(block) = block {
                        s.emit_tokens(&block.inner)?;
                    }

                    s.emit_tokens(&imported_file_tokens)
                })?;
                let mut new_exports = self.exports.clone();
                self.exports = prev_exports;

                for i in new_exports.keys() {
                    log::trace!("New export: {}", i);
                }

                let args: Vec<SpecificImportArg> = match args {
                    ImportArgs::All(star) => {
                        // Just take whatever new exports we have and pretend we have manually
                        // selected them to import
                        new_exports
                            .keys()
                            .map(|path| SpecificImportArg {
                                path: star.map(|_| path.clone()),
                                as_: None,
                            })
                            .collect()
                    }
                    ImportArgs::Specific(specific) => {
                        specific.iter().map(|(arg, _)| arg.data.clone()).collect()
                    }
                };

                for arg in &args {
                    let target = match &arg.as_ {
                        Some((_, path)) => path,
                        None => &arg.path,
                    };

                    log::trace!("Trying to remove: {}", &arg.path.data);
                    match new_exports.remove_entry(&arg.path.data) {
                        Some((_original_symbol_name, export)) => {
                            let existing_export = self.exports.get(&target.data);

                            // Don't allow import if there is already a symbol with the same name
                            // _or_ an import that goes somehwere else
                            if self.symbols.contains_key(&target.data)
                                || (existing_export.is_some() && existing_export != Some(&export))
                            {
                                return self.error(
                                    target.span,
                                    format!(
                                        "cannot import an already defined symbol: {}",
                                        target.data
                                    ),
                                );
                            }

                            self.exports.insert(target.data.clone(), export);
                        }
                        None => {
                            return self.error(
                                target.span,
                                format!("undefined export: {}", arg.path.data),
                            );
                        }
                    }
                }
            }
            Token::File { filename, .. } => {
                let span = filename.span;
                let source_file: PathBuf = self.tree.code_map.look_up_span(span).file.name().into();
                let filename = match source_file.parent() {
                    Some(parent) => parent.join(&filename.data),
                    None => PathBuf::from(&filename.data),
                };
                match fs::read(&filename) {
                    Ok(bytes) => {
                        self.emit(span, &bytes)?;
                    }
                    Err(_) => {
                        return self.error(
                            span,
                            &format!("file not found: {}", filename.to_string_lossy()),
                        );
                    }
                }
            }
            Token::Instruction(i) => {
                let (value, am, suffix) = match &i.operand {
                    Some(op) => {
                        let value = self.evaluate_expression(&op.expr)?;
                        let register_suffix = op.suffix.as_ref().map(|s| s.register.data);
                        (value, op.addressing_mode, register_suffix)
                    }
                    None => (0, AddressingMode::Implied, None),
                };

                let value = match &i.mnemonic.data {
                    Mnemonic::Bcc
                    | Mnemonic::Bcs
                    | Mnemonic::Beq
                    | Mnemonic::Bmi
                    | Mnemonic::Bne
                    | Mnemonic::Bpl
                    | Mnemonic::Bvc
                    | Mnemonic::Bvs => {
                        let target_pc = value as i64;
                        // If the current PC cannot be determined we'll just default to the target_pc. This will be fixed up later
                        // when the instruction is re-emitted.
                        let cur_pc = (self
                            .try_current_target_pc()
                            .unwrap_or_else(|| target_pc.into())
                            + 2)
                        .as_i64();
                        let mut offset = target_pc - cur_pc;
                        if offset >= -128 && offset <= 127 {
                            if offset < 0 {
                                offset += 256;
                            }
                            offset as i64
                        } else if target_pc == 0 {
                            // We probably couldn't determine the target_pc, so let's ignore the error for now.
                            // We'll just return a dummy offset. This instruction will be re-emitted in a next pass anyway.
                            0
                        } else {
                            return self.error(i.mnemonic.span, "branch too far");
                        }
                    }
                    _ => value,
                };

                match get_opcode_bytes(i.mnemonic.data, am, suffix, value) {
                    Ok(bytes) => self.emit(i.mnemonic.span, &bytes)?,
                    Err(()) => return self.error(i.mnemonic.span, "operand size mismatch"),
                }
            }
            Token::Label { id, block, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    self.add_symbol(id.span, id.data.clone(), Symbol::label(pc.as_i64()))?;
                    self.update_definition(&id.data, |def| def.set_location(id));
                }

                if let Some(b) = block {
                    self.with_scope(&id.data, |s| s.emit_tokens(&b.inner))?;
                }
            }
            Token::Loop {
                expr,
                loop_scope,
                block,
                ..
            } => {
                let loop_count = self.evaluate_expression(expr)?;
                for index in 0..loop_count {
                    self.with_scope(loop_scope, |s| {
                        s.add_symbol(expr.span, "index", Symbol::constant(index))?;
                        let result = s.emit_tokens(&block.inner);
                        s.remove_symbol("index");
                        result
                    })?;
                }
            }
            Token::MacroDefinition {
                id, args, block, ..
            } => {
                let args = args.iter().map(|(arg, _)| arg.clone()).collect();
                self.add_symbol(
                    id.span,
                    &id.data,
                    Symbol::constant(MacroDefinition {
                        id: id.clone(),
                        args,
                        block: block.inner.clone(),
                    }),
                )?;
            }
            Token::MacroInvocation { id: name, args, .. } => {
                let def = self
                    .get_symbol_data(name.span, &name.data)
                    .map(|d| d.as_macro_definition().clone());

                if let Some(def) = def {
                    self.expect_args(name.span, args.len(), def.args.len())?;
                    self.with_scope(&name.data, |s| {
                        for (idx, arg_name) in def.args.iter().enumerate() {
                            let (expr, _) = args.get(idx).unwrap();
                            let value = s.evaluate_expression(expr)?;
                            s.add_symbol(arg_name.span, &arg_name.data, Symbol::macro_arg(value))?;
                        }

                        s.emit_tokens(&def.block)?;

                        for arg_name in &def.args {
                            s.remove_symbol(&arg_name.data);
                        }

                        Ok(())
                    })?;
                }
            }
            Token::ProgramCounterDefinition { value, .. } => {
                let pc = self.evaluate_expression(value)?.into();
                if let Some(seg) = self.try_current_segment_mut() {
                    seg.pc = pc;
                }
            }
            Token::Segment { id, block, .. } => {
                if !self.segments.contains_key(&id.data) {
                    return self.error(id.span, format!("unknown identifier: {}", id.data));
                }
                match block {
                    Some(block) => {
                        let old_segment =
                            std::mem::replace(&mut self.current_segment, Some(id.data.clone()));
                        self.with_scope(&id.data, |s| s.emit_tokens(&block.inner))?;
                        self.current_segment = old_segment;
                    }
                    None => {
                        self.current_segment = Some(id.data.clone());
                    }
                }
            }
            Token::VariableDefinition { ty, id, value, .. } => {
                let value = self.evaluate_expression(&value)?;
                let mut symbol = Symbol::variable(value);
                symbol.read_only = match &ty.data {
                    VariableType::Constant => true,
                    VariableType::Variable => false,
                };

                self.add_symbol(id.span, id.data.clone(), symbol)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn evaluate_expression(&mut self, expr: &Located<Expression>) -> MosResult<i64> {
        match &expr.data {
            Expression::Factor { factor, flags, .. } => {
                let mut value = self.evaluate_expression_factor(factor)?;
                if flags.contains(ExpressionFactorFlags::NOT) {
                    if value == 0 {
                        value = 1
                    } else {
                        value = 0
                    }
                }
                if flags.contains(ExpressionFactorFlags::NEG) {
                    value = -value;
                }
                Ok(value)
            }
            Expression::BinaryExpression(bin) => {
                let lhs = self.evaluate_expression(&bin.lhs)?;
                let rhs = self.evaluate_expression(&bin.rhs)?;
                Ok(bin.op.data.apply(lhs, rhs))
            }
        }
    }

    fn expect_args(&self, span: Span, actual: usize, expected: usize) -> MosResult<()> {
        if actual != expected {
            self.error(
                span,
                format!("expected {} arguments, got {}", expected, actual),
            )
        } else {
            Ok(())
        }
    }

    fn evaluate_expression_factor(&mut self, factor: &Located<ExpressionFactor>) -> MosResult<i64> {
        match &factor.data {
            ExpressionFactor::CurrentProgramCounter(_) => {
                Ok(self.try_current_target_pc().unwrap_or_default().as_i64())
            }
            ExpressionFactor::ExprParens { inner, .. } => self.evaluate_expression(inner),
            ExpressionFactor::FunctionCall { name, args, .. } => match name.data.as_str() {
                "defined" => {
                    self.expect_args(name.span, args.len(), 1)?;
                    let (expr, _) = args.first().unwrap();
                    let val = self
                        .with_suppressed_undefined_registration(|s| s.evaluate_expression(expr));
                    match val {
                        Ok(result) => {
                            if result != 0 {
                                Ok(1)
                            } else {
                                Ok(0)
                            }
                        }
                        Err(_) => Ok(0),
                    }
                }
                _ => self.error(name.span, format!("unknown function: {}", &name.data)),
            },
            ExpressionFactor::IdentifierValue { path, modifier } => {
                log::trace!(
                    "Adding usage for definition '{}' --> {:?}",
                    &path.data,
                    path.span
                );
                self.update_definition(&path.data, |def| def.add_usage(path));

                let val = self
                    .get_symbol_data(path.span, &path.data)
                    .map(|d| d.as_i64())
                    .unwrap_or_default();
                match modifier.as_ref().map(|m| &m.data) {
                    Some(AddressModifier::HighByte) => Ok((val >> 8) & 255),
                    Some(AddressModifier::LowByte) => Ok(val & 255),
                    _ => Ok(val),
                }
            }
            ExpressionFactor::Number { value: number, .. } => Ok(number.data.value()),
        }
    }

    fn update_definition<IP: Into<IdentifierPath>, F: FnOnce(&mut Definition)>(
        &mut self,
        id: IP,
        f: F,
    ) {
        if let Some((symbol_path, _)) = self.get_symbol(&self.current_scope, &id.into()) {
            let def = self
                .analysis
                .get_or_create_definition_mut(DefinitionType::Symbol(symbol_path));
            f(def);
        }
    }

    fn with_suppressed_undefined_registration<F: FnOnce(&mut Self) -> MosResult<i64>>(
        &mut self,
        f: F,
    ) -> MosResult<i64> {
        self.suppress_undefined_symbol_registration = true;
        let result = f(self);
        self.suppress_undefined_symbol_registration = false;
        result
    }

    fn with_scope<F: FnOnce(&mut Self) -> MosResult<()>>(
        &mut self,
        scope: &Identifier,
        f: F,
    ) -> MosResult<()> {
        self.current_scope.push(scope);
        self.try_current_target_pc()
            .map(|pc| self.add_symbol(None, "-", Symbol::variable(pc.as_i64())));
        log::trace!("Entering scope: {}", self.current_scope);
        let result = f(self);
        log::trace!("Leaving scope: {}", self.current_scope);
        self.try_current_target_pc()
            .map(|pc| self.add_symbol(None, "+", Symbol::variable(pc.as_i64())));
        self.current_scope.pop();
        result
    }
}

pub fn codegen(ast: Arc<ParseTree>, options: CodegenOptions) -> MosResult<CodegenContext> {
    let mut ctx = CodegenContext::new(ast.clone());

    #[cfg(test)]
    const MAX_ITERATIONS: usize = 50;

    #[cfg(not(test))]
    const MAX_ITERATIONS: usize = usize::MAX;

    let mut iterations = MAX_ITERATIONS;
    while iterations != 0 {
        ctx.emit_tokens(&ast.main_file().tokens)?;
        ctx.after_pass();

        // Are there no segments yet? Then create a default one.
        if ctx.segments.is_empty() {
            log::trace!("Creating default segment");
            let seg_opts = SegmentOptions {
                initial_pc: options.pc,
                target_address: options.pc,
                ..Default::default()
            };
            ctx.segments
                .insert("default".into(), Segment::new(seg_opts));
            ctx.current_segment = Some("default".into());
        } else {
            // There were segments, so we have emitted something.

            // Nothing undefined anymore? Then we're done!
            if ctx.undefined.borrow().is_empty() {
                break;
            }

            // Anything that was undefined at some point should be defined by now.
            // If not, it is truly undefined.
            let errors = ctx
                .undefined
                .borrow()
                .iter()
                .sorted_by_key(|k| k.id.to_string())
                .filter_map(|item| {
                    if ctx.get_symbol(&item.scope, &item.id).is_none() {
                        Some(MosError::Codegen {
                            location: ctx.tree.code_map.look_up_span(item.span),
                            message: format!("unknown identifier: {}", item.id),
                        })
                    } else {
                        None
                    }
                })
                .collect_vec();
            if !errors.is_empty() {
                return Err(MosError::Multiple(errors));
            }
        }

        ctx.next_pass();
        iterations -= 1;
    }

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use super::{codegen, CodegenContext, CodegenOptions};
    use crate::core::codegen::{Segment, SegmentOptions};
    use crate::core::parser::source::{InMemoryParsingSource, ParsingSource};
    use crate::core::parser::{parse_or_err, Identifier};
    use crate::errors::MosResult;
    use std::cell::RefCell;
    use std::path::Path;
    use std::sync::Arc;

    impl CodegenContext {
        pub fn get_segment<S: Into<Identifier>>(&self, key: S) -> &Segment {
            self.try_get_segment(key).unwrap()
        }

        pub fn try_get_segment<S: Into<Identifier>>(&self, key: S) -> Option<&Segment> {
            self.segments.get(&key.into())
        }

        pub fn current_segment(&self) -> &Segment {
            self.segments
                .get(&self.current_segment.as_ref().unwrap())
                .unwrap()
        }
    }

    #[test]
    fn basic() -> MosResult<()> {
        let ctx = test_codegen("lda #123\nlda #$40\nlda #%10010")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 123, 0xa9, 64, 0xa9, 18]
        );
        Ok(())
    }

    #[test]
    fn basic_with_comments() -> MosResult<()> {
        let ctx = test_codegen("lda /*hello*/ #123")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn basic_with_braces() -> MosResult<()> {
        let ctx = test_codegen("{ lda #123 }")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn can_detect_operand_size_mismatch() {
        let err = test_codegen("lda (foo,x)\nfoo: nop").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:1: error: operand size mismatch"
        );
    }

    #[test]
    fn expressions() -> MosResult<()> {
        let ctx = test_codegen(
            r"
            lda #1 + 1
            lda #1 - 1
            lda #2 * 4
            lda #8 / 2
            lda #1 + 5 * 4 + 3
            lda #7 == 7
            lda #7 != 7
            lda #7 == 8
            lda #7 != 8
            lda #1 << 2
            lda #4 >> 1
            lda #0 ^ 255
            lda #2 > 2
            lda #2 >= 2
            lda #2 < 2
            lda #2 <= 2
            ",
        )?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![
                0xa9, 2, 0xa9, 0, 0xa9, 8, 0xa9, 4, 0xa9, 24, 0xa9, 1, 0xa9, 0, 0xa9, 0, 0xa9, 1,
                0xa9, 4, 0xa9, 2, 0xa9, 255, 0xa9, 0, 0xa9, 1, 0xa9, 0, 0xa9, 1
            ]
        );
        Ok(())
    }

    #[test]
    fn overflowing_expressions() -> MosResult<()> {
        let ctx = test_codegen(
            r"
        lda #1-2
        lda $ffff + 3
        ",
        )?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 0xff, 0xad, 2, 0]
        );
        Ok(())
    }

    #[test]
    fn can_use_variables() -> MosResult<()> {
        let ctx = test_codegen(".var foo=49152\nlda #>foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0xc0]);
        Ok(())
    }

    #[test]
    fn can_redefine_variables() -> MosResult<()> {
        let ctx = test_codegen(".var foo=49152\n.var foo=foo + 5\nlda #<foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0x05]);
        Ok(())
    }

    #[test]
    fn can_use_scopes() -> MosResult<()> {
        let ctx = test_codegen(
            r"
                .var a = 1
                {
                    .var a = 2
                    lda #a
                    lda #super.a
                }
                lda #a
            ",
        )?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 2, 0xa9, 1, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_use_named_scopes() -> MosResult<()> {
        let ctx = test_codegen(
            r"
                foo: {
                    .var a = 1
                    nop
                    bar: { .var a = 2 }
                }
                lda #foo.a
                lda #foo.bar.a
            ",
        )?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xa9, 1, 0xa9, 2]
        );
        Ok(())
    }

    #[test]
    fn can_define_segments() -> MosResult<()> {
        let ctx =
            test_codegen(".define segment {\nname = foo\nstart = 49152\nwrite = false\n}\nnop")?;
        assert_eq!(ctx.get_segment("foo").range(), 0xc000..0xc001);
        assert_eq!(ctx.get_segment("foo").range_data(), vec![0xea]);
        assert_eq!(ctx.get_segment("foo").options().write, false);
        assert_eq!(ctx.try_get_segment("default").is_none(), true);
        Ok(())
    }

    #[test]
    fn can_use_segments() -> MosResult<()> {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                nop
                .segment b
                rol
                .segment a
                asl
                ",
        )?;
        assert_eq!(ctx.get_segment("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.get_segment("b").range_data(), vec![0x2a]);
        Ok(())
    }

    #[test]
    fn segment_targets() -> MosResult<()> {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 pc = $4000 }
                lda data
                data: nop
                ",
        )?;
        assert_eq!(
            ctx.get_segment("a").range_data(),
            vec![0xad, 0x03, 0x40, 0xea]
        );
        Ok(())
    }

    #[test]
    fn cannot_exceed_range() {
        let err = test_codegen(".define segment { name = a start = $ffff }\nnop\nnop")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:3:1: error: segment 'a' is out of range"
        );
    }

    #[test]
    fn can_use_scoped_segments() -> MosResult<()> {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                nop
                .segment b { rol }
                asl
                ",
        )?;
        assert_eq!(ctx.get_segment("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.get_segment("b").range_data(), vec![0x2a]);
        Ok(())
    }

    #[test]
    fn can_use_forward_references_to_other_segments() -> MosResult<()> {
        // segment b can access 'bar' since it is in a root scope
        // segment a can access 'foo' as 'b.foo' since it is in a segment scope
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                lda b.foo
                .segment b { foo: lda bar }
                bar: nop
                ",
        )?;

        assert_eq!(
            ctx.get_segment("a").range_data(),
            vec![0xad, 0x00, 0x20, 0xea]
        );
        assert_eq!(ctx.get_segment("b").range_data(), vec![0xad, 0x03, 0x10]);
        Ok(())
    }

    #[test]
    fn cannot_use_unknown_segments() {
        let err = test_codegen(".segment foo").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:10: error: unknown identifier: foo"
        );
    }

    #[test]
    fn segment_configuration_is_validated() {
        let err = test_codegen(".define segment { foo = bar }").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:19: error: field not allowed: foo\ntest.asm:1:9: error: missing required fields: name, start"
        );
    }

    #[test]
    fn segments_can_depend_on_each_other() -> MosResult<()> {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = segments.a.end }
                nop
                .segment b { rol }
                asl
                ",
        )?;
        assert_eq!(ctx.get_segment("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.get_segment("b").range_data(), vec![0x2a]);
        assert_eq!(ctx.get_segment("a").range(), 0x1000..0x1002);
        assert_eq!(ctx.get_segment("b").range(), 0x1002..0x1003);
        Ok(())
    }

    #[test]
    fn can_use_constants() -> MosResult<()> {
        let ctx = test_codegen(".const foo=49152\nlda #>foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0xc0]);

        let ctx = test_codegen(".if !defined(foo) { .const foo=49152 }\nlda #>foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0xc0]);

        let ctx =
            test_codegen(".const foo = 32768\n.if !defined(foo) { .const foo=49152 }\nlda #>foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0x80]);

        Ok(())
    }

    #[test]
    fn cannot_redefine_constants() {
        let err = test_codegen(".const foo=49152\n.const foo=foo + 5")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:8: error: cannot redefine symbol: foo"
        );
    }

    #[test]
    fn cannot_redefine_variable_types() {
        let err = test_codegen(".const foo=49152\nfoo: nop").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:1: error: cannot redefine symbol: foo"
        );
    }

    #[test]
    fn if_else() -> MosResult<()> {
        let ctx = test_codegen(
            ".const foo=1\n.if foo { nop } else { rol }\n.if foo > 10 { nop } else { asl }",
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0x0a]);
        Ok(())
    }

    #[test]
    fn ifdef() -> MosResult<()> {
        let ctx = test_codegen(".const foo=1\n.if defined(foo) { nop }\n.if defined(bar) { asl }")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea]);
        Ok(())
    }

    #[test]
    fn ifndef() -> MosResult<()> {
        let ctx =
            test_codegen(".const foo=1\n.if !defined(foo) { nop }\n.if !defined(bar) { asl }")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0x0a]);
        Ok(())
    }

    #[test]
    fn if_or_and() -> MosResult<()> {
        let ctx = test_codegen(
            r"
            .const foo=1
            .const bar=0
            .if foo || bar { nop }
            .if foo && bar { asl }
            .if foo && !bar { rol }
            ",
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0x2a]);
        Ok(())
    }

    #[test]
    fn unary_negative_and_not() -> MosResult<()> {
        let ctx = test_codegen(".var foo = 1\nlda #-foo\nlda #!foo")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 0xff, 0xa9, 0x00]
        );
        Ok(())
    }

    #[test]
    fn align() -> MosResult<()> {
        let ctx = test_codegen("nop\n.align 3\nnop")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0, 0, 0xea]);
        Ok(())
    }

    #[test]
    fn loop_() -> MosResult<()> {
        let ctx = test_codegen(".loop 3 { lda #index }")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 0, 0xa9, 1, 0xa9, 2]
        );
        Ok(())
    }

    #[test]
    fn file() -> MosResult<()> {
        let root = env!("CARGO_MANIFEST_DIR");
        let input = &format!("{}/test/cli/build/include.bin", root);
        let source = format!(".file \"{}\"", input);

        let ctx = test_codegen(&source)?;
        assert_eq!(
            ctx.current_segment().range_data(),
            include_bytes!("../../../test/cli/build/include.bin")
        );
        Ok(())
    }

    #[test]
    fn can_access_current_pc() -> MosResult<()> {
        let ctx = test_codegen("lda * + 3\nlda *")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xad, 0x03, 0xc0, 0xad, 0x03, 0xc0]
        );
        Ok(())
    }

    #[test]
    fn can_set_current_pc() -> MosResult<()> {
        let ctx = test_codegen("* = $1234\nnop")?;
        assert_eq!(ctx.current_segment().range(), 0x1234..0x1235);
        assert_eq!(ctx.current_segment().range_data(), vec![0xea]);
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels() -> MosResult<()> {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x4c, 0x03, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels_within_scope() -> MosResult<()> {
        let ctx = test_codegen("{ jmp my_label\nmy_label: nop }")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x4c, 0x03, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn cannot_access_forward_declared_labels_within_nested_scope() {
        let err = test_codegen("jmp my_label\n{ my_label: nop }")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:5: error: unknown identifier: my_label"
        );
    }

    #[test]
    fn can_access_scope_symbols() -> MosResult<()> {
        let ctx = test_codegen("{\nnop\n{\njmp -\njmp super.-\njmp +\n}\n}")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0x4c, 0x01, 0xc0, 0x4c, 0x00, 0xc0, 0x4c, 0x0a, 0xc0]
        );
        Ok(())
    }

    #[test]
    fn can_modify_addresses() -> MosResult<()> {
        let ctx = test_codegen("lda #<my_label\nlda #>my_label\nmy_label: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 0x04, 0xa9, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn error_unknown_identifiers() {
        let err = test_codegen(".byte foo\n.byte foo2").err().unwrap();
        assert_eq!(format!("{}", err), "test.asm:1:7: error: unknown identifier: foo\ntest.asm:2:7: error: unknown identifier: foo2");
    }

    #[test]
    fn can_store_data() -> MosResult<()> {
        let ctx = test_codegen(".byte 123\n.word 123\n.word $fce2")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![123, 123, 0, 0xe2, 0xfc]
        );
        Ok(())
    }

    #[test]
    fn can_store_current_pc_as_data() -> MosResult<()> {
        let ctx = test_codegen(".word *\n.word foo - *\nfoo: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x00, 0xc0, 0x02, 0x00, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_store_csv_data() -> MosResult<()> {
        let ctx = test_codegen(".word 123, foo, 234\nfoo: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![123, 0, 0x06, 0xc0, 234, 0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_perform_operations_on_labels() -> MosResult<()> {
        // Create two labels, 'foo' and 'bar', separated by three NOPs.
        // 'foo' is a word label (so, 2 bytes), so 'bar - foo' should be 5 (2 bytes + 3 NOPs).
        let ctx = test_codegen("foo: .word bar - foo\nnop\nnop\nnop\nbar: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x05, 0x00, 0xea, 0xea, 0xea, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_perform_branch_calculations() -> MosResult<()> {
        let ctx = test_codegen("foo: nop\nbne foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0xd0, 0xfd]);
        let ctx = test_codegen("bne foo\nfoo: nop")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xd0, 0x00, 0xea]);
        Ok(())
    }

    #[test]
    fn can_use_macros() -> MosResult<()> {
        let ctx = test_codegen(
            ".macro foo(val) { .if val { nop } else { lda bar } }\nfoo(1)\nfoo(0)\nbar: brk",
        )?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xad, 0x04, 0xc0, 0x00]
        );
        Ok(())
    }

    #[test]
    fn should_invoke_macros_with_correct_number_of_arguments() {
        let err = test_codegen(".macro foo(val) { }\nfoo(1, 2)")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:1: error: expected 1 arguments, got 2"
        );
    }

    #[test]
    fn cannot_perform_too_far_branch_calculations() -> MosResult<()> {
        let many_nops = std::iter::repeat("nop\n").take(140).collect::<String>();
        let src = format!("foo: {}bne foo", many_nops);
        let ast = parse_or_err(
            &Path::new("test.asm"),
            InMemoryParsingSource::new().add("test.asm", &src).into(),
        )?;
        let result = codegen(ast, CodegenOptions::default());
        assert_eq!(
            format!("{}", result.err().unwrap()),
            "test.asm:141:1: error: branch too far"
        );
        Ok(())
    }

    #[test]
    fn can_emit_to_segment() {
        let mut seg = Segment::new(SegmentOptions {
            initial_pc: 0xc000.into(),
            target_address: 0xb000.into(),
            ..Default::default()
        });
        seg.emit(&[1, 2, 3]);
        assert_eq!(seg.pc, 0xc003.into());
        assert_eq!(seg.data[0xc000..0xc003], [1, 2, 3]);
        assert_eq!(seg.range_data(), &[1, 2, 3]);
        assert_eq!(seg.range(), 0xc000..0xc003);
        assert_eq!(seg.target_offset(), -0x1000);

        seg.pc = 0x2000.into();
        seg.emit(&[4]);
        assert_eq!(seg.pc, 0x2001.into());
        assert_eq!(seg.data[0xc000..0xc003], [1, 2, 3]);
        assert_eq!(seg.data[0x2000..0x2001], [4]);
        assert_eq!(seg.range(), 0x2000..0xc003);
    }

    #[test]
    fn can_use_wildcard_imports() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import * from \"bar\"\nlda #defined(foo)\nlda #defined(bar)\nlda #defined(baz)",
            )
            .add("bar", "foo: nop\nbar: nop\n.export foo\n.export bar as baz")
            .into();

        let ctx = test_codegen_parsing_source(src)?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xea, 0xa9, 1, 0xa9, 0, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_use_specific_imports() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import biz.baz as boz from \"bar\"\nlda #defined(foo)\nlda #defined(bar)\nlda #defined(biz.baz)\nlda #defined(boz)",
            )
            .add("bar", "foo: nop\nbar: nop\n.export foo\n.export bar as biz.baz")
            .into();

        let ctx = test_codegen_parsing_source(src)?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xea, 0xa9, 0, 0xa9, 0, 0xa9, 0, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_have_imports_with_block() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".import foo from \"bar\" { .const CONST = 1 }")
            .add("bar", "foo: lda #CONST\n.export foo")
            .into();

        let ctx = test_codegen_parsing_source(src)?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 1]);
        Ok(())
    }

    #[test]
    fn cannot_import_unknown_export() {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".import foo from \"bar\"")
            .add("bar", "nop")
            .into();

        let err = test_codegen_parsing_source(src).err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:9: error: undefined export: foo"
        );
    }

    #[test]
    fn cannot_have_duplicate_imports() {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import foo from \"bar\"\n.import foo as foo2 from \"bar\"\n.import foo from \"bar\"",
            )
            .add("bar", "foo: nop\n.export foo")
            .into();

        let err = test_codegen_parsing_source(src).err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:3:9: error: cannot import an already defined symbol: foo"
        );
    }

    #[test]
    fn cannot_have_duplicate_imports_via_wildcard_import() {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".import * from \"bar\"\n.import * from \"bar\"")
            .add("bar", "foo: nop\n.export foo")
            .into();

        let err = test_codegen_parsing_source(src).err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:9: error: cannot import an already defined symbol: foo"
        );
    }

    pub(super) fn test_codegen(code: &str) -> MosResult<CodegenContext> {
        test_codegen_parsing_source(InMemoryParsingSource::new().add("test.asm", &code).into())
    }

    pub(super) fn test_codegen_parsing_source(
        src: Arc<RefCell<dyn ParsingSource>>,
    ) -> MosResult<CodegenContext> {
        let ast = parse_or_err(&Path::new("test.asm"), src)?;
        codegen(ast, CodegenOptions::default())
    }
}
