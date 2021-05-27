mod analysis;
mod config_extractor;
mod config_validator;
mod opcodes;
mod program_counter;
mod source_map;
mod symbols;
mod text_encoding;

pub use analysis::*;
pub use program_counter::*;
pub use source_map::*;
pub use symbols::*;

use crate::codegen::config_validator::ConfigValidator;
use crate::codegen::opcodes::get_opcode_bytes;
use crate::codegen::source_map::SourceMap;
use crate::codegen::text_encoding::encode_text;
use crate::errors::{CoreError, CoreResult};
use crate::parser::code_map::Span;
use crate::parser::{
    AddressModifier, AddressingMode, DataSize, Expression, ExpressionFactor, ExpressionFactorFlags,
    Identifier, IdentifierPath, ImportArgs, Located, Mnemonic, ParseTree, TextEncoding, Token,
    VariableType,
};
use fs_err as fs;
use itertools::Itertools;
use once_cell::sync::OnceCell;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, Range};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct CodegenOptions {
    pub pc: ProgramCounter,
    pub active_test: Option<IdentifierPath>,
    pub predefined_constants: HashMap<String, i64>,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            pc: ProgramCounter::new(0xc000),
            active_test: None,
            predefined_constants: HashMap::new(),
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

    pub fn try_as_i64(&self) -> Option<i64> {
        match &self {
            SymbolData::Number(val) => Some(*val),
            _ => None,
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
    TestCase,
    MacroArgument,
    Variable,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub pass_idx: usize,
    pub data: SymbolData,
    pub ty: SymbolType,
    pub read_only: bool,
}

impl Symbol {
    pub fn label<V: Into<SymbolData>>(pass_idx: usize, data: V) -> Self {
        Self {
            pass_idx,
            data: data.into(),
            ty: SymbolType::Label,
            read_only: true,
        }
    }

    fn test_case<V: Into<SymbolData>>(pass_idx: usize, data: V) -> Self {
        Self {
            pass_idx,
            data: data.into(),
            ty: SymbolType::TestCase,
            read_only: true,
        }
    }

    pub fn constant<V: Into<SymbolData>>(pass_idx: usize, data: V) -> Self {
        Self {
            pass_idx,
            data: data.into(),
            ty: SymbolType::Variable,
            read_only: true,
        }
    }

    fn macro_arg<V: Into<SymbolData>>(pass_idx: usize, data: V) -> Self {
        Self {
            pass_idx,
            data: data.into(),
            ty: SymbolType::MacroArgument,
            read_only: true,
        }
    }

    fn variable<V: Into<SymbolData>>(pass_idx: usize, data: V) -> Self {
        Self {
            pass_idx,
            data: data.into(),
            ty: SymbolType::Variable,
            read_only: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroDefinition {
    id: Located<Identifier>,
    args: Vec<Located<Identifier>>,
    block: Vec<Token>,
}

pub trait FunctionCallback {
    fn expected_args(&self) -> usize;
    fn apply(&self, ctx: &mut CodegenContext, args: &[&Located<Expression>]) -> CoreResult<i64>;
}

pub struct CodegenContext {
    tree: Arc<ParseTree>,
    options: CodegenOptions,

    analysis: Analysis,

    pass_idx: usize,

    segments: HashMap<Identifier, Segment>,
    current_segment: Option<Identifier>,

    functions: HashMap<String, Arc<dyn FunctionCallback + Send + Sync>>,

    symbols: SymbolTable<Symbol>,
    undefined: Arc<Mutex<HashSet<UndefinedSymbol>>>,
    suppress_undefined_symbol_registration: bool,
    current_scope: IdentifierPath,
    current_scope_nx: SymbolIndex,

    assertions: Vec<Assertion>,

    source_map: SourceMap,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct UndefinedSymbol {
    scope_nx: SymbolIndex,
    id: IdentifierPath,
    span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assertion {
    pub pc: ProgramCounter,
    pub expression: Located<Expression>,
    pub failure_message: Option<String>,
}

impl CodegenContext {
    fn new(tree: Arc<ParseTree>, options: CodegenOptions) -> Self {
        let analysis = Analysis::new(tree.clone());

        Self {
            tree,
            options,
            analysis,
            pass_idx: 0,
            segments: HashMap::new(),
            current_segment: None,
            functions: HashMap::new(),
            symbols: SymbolTable::default(),
            undefined: Arc::new(Mutex::new(HashSet::new())),
            suppress_undefined_symbol_registration: false,
            current_scope: IdentifierPath::empty(),
            current_scope_nx: SymbolIndex::new(0),
            assertions: vec![],
            source_map: SourceMap::default(),
        }
    }

    pub fn analysis(&self) -> &Analysis {
        &self.analysis
    }

    pub fn segments(&self) -> &HashMap<Identifier, Segment> {
        &self.segments
    }

    pub fn symbols(&self) -> &SymbolTable<Symbol> {
        &self.symbols
    }

    pub fn symbols_mut(&mut self) -> &mut SymbolTable<Symbol> {
        &mut self.symbols
    }

    pub fn tree(&self) -> &Arc<ParseTree> {
        &self.tree
    }

    pub fn assertions(&self) -> &[Assertion] {
        &self.assertions
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
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
                Symbol::variable(self.pass_idx, range.start as i64),
            );
            let _ = self.add_symbol(
                None,
                path.join("end"),
                Symbol::variable(self.pass_idx, range.end as i64),
            );
        }
    }

    fn next_pass(&mut self) {
        self.pass_idx += 1;

        log::trace!("\n* NEXT PASS ({}) *", self.pass_idx);
        self.segments.values_mut().for_each(|s| s.reset());
        self.undefined.lock().unwrap().clear();
        self.assertions.clear();
        self.source_map.clear();
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
    ) -> CoreResult<SymbolIndex> {
        let span = span.into();
        let id = id.into();
        let path = self.current_scope.join(&id);
        log::trace!(
            "Inserting symbol: '{}' with value '{:?}' (inside scope '{}')",
            &id,
            value,
            &self.current_scope
        );

        let mut should_mark_as_undefined = false;
        let symbol_nx = self.symbols.try_index(self.current_scope_nx, &id);
        log::trace!("Trying to lookup symbol '{}': {:?}", &id, symbol_nx);
        let symbol_nx = match symbol_nx {
            Some(symbol_nx) => {
                match self.symbols.try_get_mut(symbol_nx) {
                    Some(existing) => {
                        if existing.ty != value.ty
                            || existing.read_only != value.read_only
                            || (existing.pass_idx == value.pass_idx
                                && existing.data != value.data
                                && existing.read_only)
                        {
                            let span = span.expect("no span provided");
                            return self.error(span, format!("cannot redefine symbol: {}", &path));
                        }

                        // If the symbol already existed but with a different value,
                        // mark it as undefined so we will trigger another pass
                        if existing.data != value.data {
                            log::trace!(
                                "`--> Symbol already existed, but has changed. Old value was: {:?}",
                                existing
                            );

                            should_mark_as_undefined = match &value.ty {
                                SymbolType::Label => true,
                                SymbolType::TestCase => true,
                                SymbolType::Variable => false,
                                SymbolType::MacroArgument => false,
                            };
                        }

                        log::trace!("Symbol was updated with index: {:?}", symbol_nx);
                        *existing = value;
                    }
                    None => {
                        log::trace!(
                            "Symbol was updated from 'no data' with index: {:?}",
                            symbol_nx
                        );
                        self.symbols.update_data(symbol_nx, value);
                    }
                }

                symbol_nx
            }
            None => {
                let (parent, id) = path.clone().split();
                let parent_nx = self.symbols.ensure_index(self.symbols.root, &parent);
                let nx = self.symbols.insert(parent_nx, id, value);
                log::trace!("Symbol was inserted with index: {:?}", nx);
                nx
            }
        };

        if should_mark_as_undefined {
            self.mark_undefined(span.expect("expected a span"), &id);
        }

        if let Some(span) = span {
            log::trace!(
                "Setting location for definition '{}' ({:?})",
                &path,
                symbol_nx
            );
            let parent_scope = self.current_scope_nx;
            self.symbol_definition(symbol_nx)
                .set_location(DefinitionLocation { parent_scope, span });
        }

        Ok(symbol_nx)
    }

    fn remove_symbol<I: Into<IdentifierPath>>(&mut self, id: I) {
        let id = id.into();
        let path = self.current_scope.join(&id);
        let nx = self.symbols.index(self.symbols.root, path);
        self.symbols.remove(nx);
    }

    pub fn get_symbol(
        &self,
        scope_nx: SymbolIndex,
        id: &IdentifierPath,
    ) -> Option<(SymbolIndex, &Symbol)> {
        log::trace!("Trying to get symbol value: {}", id);
        if let Some(nx) = self.symbols.query(scope_nx, id) {
            return self.symbols.try_get(nx).map(|s| (nx, s));
        }

        None
    }

    fn get_symbol_data<I: Into<IdentifierPath>>(&self, span: Span, id: I) -> Option<&SymbolData> {
        let id = id.into();
        match self.get_symbol(self.current_scope_nx, &id) {
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
            self.undefined.lock().unwrap().insert(UndefinedSymbol {
                scope_nx: self.current_scope_nx,
                id,
                span,
            });
        }
    }

    fn error<T, M: Into<String>>(&self, span: Span, message: M) -> CoreResult<T> {
        let location = self.tree.code_map.look_up_span(span);
        Err(CoreError::Codegen {
            location,
            message: message.into(),
        })
    }

    fn emit(&mut self, span: Span, bytes: &[u8]) -> CoreResult<()> {
        match &self.current_segment {
            Some(name) => {
                let segment = self.segments.get_mut(&name).unwrap();
                log::trace!(
                    "Emitting to segment '{}' {}: {:?}",
                    name,
                    segment.pc,
                    &bytes
                );
                self.source_map
                    .add(self.current_scope_nx, span, segment.pc, bytes.len());
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

    fn emit_tokens(&mut self, tokens: &[Token]) -> CoreResult<()> {
        for token in tokens {
            self.emit_token(token)?;
        }
        Ok(())
    }

    fn emit_token(&mut self, token: &Token) -> CoreResult<()> {
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
            Token::Assert {
                value,
                failure_message,
                ..
            } => {
                if let Some(pc) = self.try_current_target_pc() {
                    self.assertions.push(Assertion {
                        pc,
                        expression: value.clone(),
                        failure_message: failure_message.as_ref().map(|f| f.text.data.clone()),
                    });
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
            Token::File { filename, .. } => {
                let span = filename.text.span;
                let source_file: PathBuf = self.tree.code_map.look_up_span(span).file.name().into();
                let filename = match source_file.parent() {
                    Some(parent) => parent.join(&filename.text.data),
                    None => PathBuf::from(&filename.text.data),
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
                if let Some(imported_file) = self.tree.try_get_file(resolved_path) {
                    let imported_file_tokens = imported_file.tokens.clone();

                    // Make the filename a definition by itself, allowing the user to follow the definition
                    let def = self
                        .analysis
                        .get_or_create_definition_mut(DefinitionType::Filename(
                            resolved_path.clone(),
                        ));
                    def.set_location(DefinitionLocation {
                        parent_scope: self.current_scope_nx,
                        span: imported_file.file.span,
                    });
                    def.add_usage(DefinitionLocation {
                        parent_scope: self.current_scope_nx,
                        span: filename.text.span,
                    });

                    self.with_scope(import_scope, |s| {
                        if let Some(block) = block {
                            s.emit_tokens(&block.inner)?;
                        }

                        s.emit_tokens(&imported_file_tokens)
                    })?;

                    if let Some(import_nx) =
                        self.symbols.try_index(self.current_scope_nx, import_scope)
                    {
                        let mut to_export = vec![];
                        let mut add_symbol_usages = false;

                        match args {
                            ImportArgs::All(star, as_) => {
                                let span = match &as_ {
                                    Some(as_) => as_.path.span,
                                    None => star.span,
                                };

                                let scope_nx = match &as_ {
                                    Some(as_) => {
                                        // Want to import into a new named scope
                                        self.symbols
                                            .ensure_index(self.current_scope_nx, &as_.path.data)
                                    }
                                    None => self.current_scope_nx,
                                };

                                for (child_id, child_nx) in self.symbols.children(import_nx) {
                                    // Do not import special identifiers
                                    if child_id.is_special() {
                                        continue;
                                    }

                                    to_export.push((
                                        child_nx,
                                        scope_nx,
                                        IdentifierPath::from(child_id),
                                        span,
                                    ));
                                }
                            }
                            ImportArgs::Specific(specific) => {
                                add_symbol_usages = true;
                                for (arg, _) in specific {
                                    let original_path = &arg.data.path.data;
                                    let target_path = match &arg.data.as_ {
                                        Some(as_) => &as_.path.data,
                                        None => original_path,
                                    };
                                    match self.symbols.try_index(import_nx, original_path) {
                                        Some(original_nx) => {
                                            to_export.push((
                                                original_nx,
                                                self.current_scope_nx,
                                                target_path.clone(),
                                                arg.span,
                                            ));
                                        }
                                        None => {
                                            self.mark_undefined(arg.span, original_path);
                                        }
                                    }
                                }
                            }
                        };

                        for (to_export_nx, new_parent_nx, new_path, span) in to_export {
                            if self.symbols.export(to_export_nx, new_parent_nx, &new_path) {
                                if add_symbol_usages {
                                    log::trace!(
                                        "Adding usage for import '{}' ({:?})",
                                        &new_path,
                                        to_export_nx
                                    );
                                    self.symbol_definition(to_export_nx).add_usage(
                                        DefinitionLocation {
                                            parent_scope: new_parent_nx,
                                            span,
                                        },
                                    );
                                }
                            } else {
                                return self.error(
                                    span,
                                    format!(
                                        "cannot import an already defined symbol: {}",
                                        new_path
                                    ),
                                );
                            }
                        }
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
                    Ok(bytes) => {
                        let mut span = i.mnemonic.span;
                        if let Some(operand_span) = i.operand.as_ref().map(|o| o.expr.span) {
                            span = operand_span.merge(span);
                        }
                        self.emit(span, &bytes)?
                    }
                    Err(()) => {
                        // Emit 'nothing' so at least the code map gets updated
                        self.emit(i.mnemonic.span, &[])?;
                        return self.error(i.mnemonic.span, "operand size mismatch");
                    }
                }
            }
            Token::Label { id, block, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    self.add_symbol(
                        id.span,
                        id.data.clone(),
                        Symbol::label(self.pass_idx, pc.as_i64()),
                    )?;
                    self.source_map.add(self.current_scope_nx, id.span, pc, 0);
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
                        s.add_symbol(expr.span, "index", Symbol::constant(s.pass_idx, index))?;
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
                    Symbol::constant(
                        self.pass_idx,
                        MacroDefinition {
                            id: id.clone(),
                            args,
                            block: block.inner.clone(),
                        },
                    ),
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
                            s.add_symbol(
                                arg_name.span,
                                &arg_name.data,
                                Symbol::macro_arg(s.pass_idx, value),
                            )?;
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
                        self.emit_tokens(&block.inner)?;
                        self.current_segment = old_segment;
                    }
                    None => {
                        self.current_segment = Some(id.data.clone());
                    }
                }
            }
            Token::Test { id, block, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    let should_add_test_symbol = match &self.options.active_test {
                        Some(active_test) => {
                            if &self.current_scope.join(&id.data) == active_test {
                                self.emit_tokens(&block.inner)?;
                                true
                            } else {
                                false
                            }
                        }
                        None => {
                            // No active test, so enumerate all tests
                            true
                        }
                    };
                    if should_add_test_symbol {
                        self.add_symbol(
                            id.span,
                            &id.data,
                            Symbol::test_case(self.pass_idx, pc.as_i64()),
                        )?;
                    }
                }
            }
            Token::Text { encoding, text, .. } => {
                let encoding = encoding
                    .as_ref()
                    .map(|e| e.data)
                    .unwrap_or(TextEncoding::Ascii);
                self.emit(text.text.span, &encode_text(&text.text.data, encoding))?;
            }
            Token::VariableDefinition { ty, id, value, .. } => {
                let value = self.evaluate_expression(&value)?;
                let mut symbol = Symbol::variable(self.pass_idx, value);
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

    pub fn evaluate_expression(&mut self, expr: &Located<Expression>) -> CoreResult<i64> {
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

    fn expect_args(&self, span: Span, actual: usize, expected: usize) -> CoreResult<()> {
        if actual != expected {
            self.error(
                span,
                format!("expected {} arguments, got {}", expected, actual),
            )
        } else {
            Ok(())
        }
    }

    fn evaluate_expression_factor(
        &mut self,
        factor: &Located<ExpressionFactor>,
    ) -> CoreResult<i64> {
        match &factor.data {
            ExpressionFactor::CurrentProgramCounter(_) => {
                Ok(self.try_current_target_pc().unwrap_or_default().as_i64())
            }
            ExpressionFactor::ExprParens { inner, .. } => self.evaluate_expression(inner),
            ExpressionFactor::FunctionCall { name, args, .. } => {
                match self.functions.get(name.data.as_str()) {
                    Some(callback) => {
                        let callback = callback.clone();
                        self.expect_args(name.span, args.len(), callback.expected_args())?;
                        callback.apply(self, &args.iter().map(|(expr, _)| expr).collect_vec())
                    }
                    None => self.error(name.span, format!("unknown function: {}", &name.data)),
                }
            }
            ExpressionFactor::IdentifierValue { path, modifier } => {
                self.analysis.add_symbol_usage(
                    &self.symbols,
                    self.current_scope_nx,
                    &path.data,
                    path.span,
                );

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

    fn symbol_definition(&mut self, symbol_nx: SymbolIndex) -> &mut Definition {
        self.analysis
            .get_or_create_definition_mut(DefinitionType::Symbol(symbol_nx))
    }

    fn with_suppressed_undefined_registration<F: FnOnce(&mut Self) -> CoreResult<i64>>(
        &mut self,
        f: F,
    ) -> CoreResult<i64> {
        self.suppress_undefined_symbol_registration = true;
        let result = f(self);
        self.suppress_undefined_symbol_registration = false;
        result
    }

    fn with_scope<F: FnOnce(&mut Self) -> CoreResult<()>>(
        &mut self,
        scope: &Identifier,
        f: F,
    ) -> CoreResult<()> {
        let old_scope_nx = self.current_scope_nx;
        self.current_scope.push(scope);
        self.current_scope_nx = self
            .symbols
            .ensure_index(self.symbols.root, &self.current_scope);
        self.try_current_target_pc()
            .map(|pc| self.add_symbol(None, "-", Symbol::variable(self.pass_idx, pc.as_i64())));
        log::trace!(
            "Entering scope: {} ({:?})",
            self.current_scope,
            self.current_scope_nx
        );
        let result = f(self);
        log::trace!(
            "Leaving scope: {} ({:?})",
            self.current_scope,
            self.current_scope_nx
        );
        self.try_current_target_pc()
            .map(|pc| self.add_symbol(None, "+", Symbol::variable(self.pass_idx, pc.as_i64())));
        self.current_scope_nx = old_scope_nx;
        self.current_scope.pop();
        result
    }

    pub fn register_fn<F: 'static + FunctionCallback + Send + Sync>(
        &mut self,
        name: &str,
        function: F,
    ) {
        self.functions.insert(name.into(), Arc::new(function));
    }

    fn register_default_fns(&mut self) {
        struct DefinedFn;
        impl FunctionCallback for DefinedFn {
            fn expected_args(&self) -> usize {
                1
            }

            fn apply(
                &self,
                ctx: &mut CodegenContext,
                args: &[&Located<Expression>],
            ) -> CoreResult<i64> {
                let expr = args.first().unwrap();
                match ctx.with_suppressed_undefined_registration(|s| s.evaluate_expression(expr)) {
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
        }
        self.register_fn("defined", DefinedFn {});
    }
}

pub fn codegen(
    ast: Arc<ParseTree>,
    options: CodegenOptions,
) -> (Option<CodegenContext>, Option<CoreError>) {
    let mut ctx = CodegenContext::new(ast.clone(), options.clone());
    ctx.register_default_fns();
    for (name, val) in &options.predefined_constants {
        ctx.symbols
            .insert(ctx.symbols.root, name.as_str(), Symbol::constant(0, *val));
    }

    #[cfg(test)]
    const MAX_ITERATIONS: usize = 50;

    #[cfg(not(test))]
    const MAX_ITERATIONS: usize = usize::MAX;

    ctx.pass_idx = 0;
    while ctx.pass_idx != MAX_ITERATIONS {
        match ctx.emit_tokens(&ast.main_file().tokens) {
            Ok(()) => (),
            Err(e) => {
                return (Some(ctx), Some(e));
            }
        }
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
            if ctx.undefined.lock().unwrap().is_empty() {
                break;
            }

            // Anything that was undefined at some point should be defined by now.
            // If not, it is truly undefined.
            let errors = ctx
                .undefined
                .lock()
                .unwrap()
                .iter()
                .sorted_by_key(|k| k.id.to_string())
                .filter_map(|item| {
                    if ctx.get_symbol(item.scope_nx, &item.id).is_none() {
                        Some(CoreError::Codegen {
                            location: ctx.tree.code_map.look_up_span(item.span),
                            message: format!("unknown identifier: {}", item.id),
                        })
                    } else {
                        None
                    }
                })
                .collect_vec();
            if !errors.is_empty() {
                return (Some(ctx), Some(CoreError::Multiple(errors)));
            }
        }

        ctx.next_pass();
    }

    (Some(ctx), None)
}

#[cfg(test)]
mod tests {
    use super::{codegen, CodegenContext, CodegenOptions};
    use crate::codegen::{Segment, SegmentOptions};
    use crate::errors::CoreResult;
    use crate::parser::source::{InMemoryParsingSource, ParsingSource};
    use crate::parser::{parse_or_err, Identifier};
    use std::path::Path;
    use std::sync::{Arc, Mutex};

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
    fn basic() -> CoreResult<()> {
        let ctx = test_codegen("lda #123\nlda #$40\nlda #%10010")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 123, 0xa9, 64, 0xa9, 18]
        );
        Ok(())
    }

    #[test]
    fn basic_with_comments() -> CoreResult<()> {
        let ctx = test_codegen("lda /*hello*/ #123")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn basic_with_braces() -> CoreResult<()> {
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
    fn expressions() -> CoreResult<()> {
        let ctx = test_codegen(
            r"
            lda #1 + 1
            lda #1 - 1
            lda #2 * 4
            lda #8 / 2
            lda #7 % 2
            lda #1 + 5 * 4 + 3
            lda #(1 + 5) * (4 + 3)
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
                0xa9, 2, 0xa9, 0, 0xa9, 8, 0xa9, 4, 0xa9, 1, 0xa9, 24, 0xa9, 42, 0xa9, 1, 0xa9, 0,
                0xa9, 0, 0xa9, 1, 0xa9, 4, 0xa9, 2, 0xa9, 255, 0xa9, 0, 0xa9, 1, 0xa9, 0, 0xa9, 1
            ]
        );
        Ok(())
    }

    #[test]
    fn overflowing_expressions() -> CoreResult<()> {
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
    fn can_use_variables() -> CoreResult<()> {
        let ctx = test_codegen(".var foo=49152\nlda #>foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0xc0]);
        Ok(())
    }

    #[test]
    fn can_redefine_variables() -> CoreResult<()> {
        let ctx = test_codegen(".var foo=49152\n.var foo=foo + 5\nlda #<foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 0x05]);
        Ok(())
    }

    #[test]
    fn can_use_scopes() -> CoreResult<()> {
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
    fn can_use_named_scopes() -> CoreResult<()> {
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
    fn can_define_segments() -> CoreResult<()> {
        let ctx =
            test_codegen(".define segment {\nname = foo\nstart = 49152\nwrite = false\n}\nnop")?;
        assert_eq!(ctx.get_segment("foo").range(), 0xc000..0xc001);
        assert_eq!(ctx.get_segment("foo").range_data(), vec![0xea]);
        assert_eq!(ctx.get_segment("foo").options().write, false);
        assert_eq!(ctx.try_get_segment("default").is_none(), true);
        Ok(())
    }

    #[test]
    fn can_use_segments() -> CoreResult<()> {
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
    fn segment_targets() -> CoreResult<()> {
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
    fn can_use_scoped_segments() -> CoreResult<()> {
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
    fn can_use_forward_references_to_other_segments() -> CoreResult<()> {
        // segment b can access 'bar' since it is in a root scope
        // segment a can access 'foo' since it is in a segment scope
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                lda foo
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
            "test.asm:1:9: error: missing required fields: name, start\ntest.asm:1:19: error: field not allowed: foo"
        );
    }

    #[test]
    fn segments_can_depend_on_each_other() -> CoreResult<()> {
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
    fn can_use_constants() -> CoreResult<()> {
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
    fn cannot_redefine_labels() {
        let err = test_codegen("foo: nop\nfoo: nop").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:1: error: cannot redefine symbol: foo"
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
    fn if_else() -> CoreResult<()> {
        let ctx = test_codegen(
            ".const foo=1\n.if foo { nop } else { rol }\n.if foo > 10 { nop } else { asl }",
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0x0a]);
        Ok(())
    }

    #[test]
    fn ifdef() -> CoreResult<()> {
        let ctx = test_codegen(".const foo=1\n.if defined(foo) { nop }\n.if defined(bar) { asl }")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea]);
        Ok(())
    }

    #[test]
    fn ifndef() -> CoreResult<()> {
        let ctx =
            test_codegen(".const foo=1\n.if !defined(foo) { nop }\n.if !defined(bar) { asl }")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0x0a]);
        Ok(())
    }

    #[test]
    fn if_or_and() -> CoreResult<()> {
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
    fn unary_negative_and_not() -> CoreResult<()> {
        let ctx = test_codegen(".var foo = 1\nlda #-foo\nlda #!foo")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 0xff, 0xa9, 0x00]
        );
        Ok(())
    }

    #[test]
    fn align() -> CoreResult<()> {
        let ctx = test_codegen("nop\n.align 3\nnop")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0, 0, 0xea]);
        Ok(())
    }

    #[test]
    fn loop_() -> CoreResult<()> {
        let ctx = test_codegen(".loop 3 { lda #index }")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xa9, 0, 0xa9, 1, 0xa9, 2]
        );
        Ok(())
    }

    #[test]
    fn file() -> CoreResult<()> {
        let root = env!("CARGO_MANIFEST_DIR");
        let input = &format!("{}/test-data/build/include.bin", root);
        let source = format!(".file \"{}\"", input);

        let ctx = test_codegen(&source)?;
        assert_eq!(
            ctx.current_segment().range_data(),
            include_bytes!("../../test-data/build/include.bin")
        );
        Ok(())
    }

    #[test]
    fn can_access_current_pc() -> CoreResult<()> {
        let ctx = test_codegen("lda * + 3\nlda *")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xad, 0x03, 0xc0, 0xad, 0x03, 0xc0]
        );
        Ok(())
    }

    #[test]
    fn can_set_current_pc() -> CoreResult<()> {
        let ctx = test_codegen("* = $1234\nnop")?;
        assert_eq!(ctx.current_segment().range(), 0x1234..0x1235);
        assert_eq!(ctx.current_segment().range_data(), vec![0xea]);
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels() -> CoreResult<()> {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x4c, 0x03, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels_within_scope() -> CoreResult<()> {
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
    fn can_access_scope_symbols() -> CoreResult<()> {
        let ctx = test_codegen("{\nnop\n{\njmp -\njmp super.-\njmp +\n}\n}")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0x4c, 0x01, 0xc0, 0x4c, 0x00, 0xc0, 0x4c, 0x0a, 0xc0]
        );
        Ok(())
    }

    #[test]
    fn can_modify_addresses() -> CoreResult<()> {
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
    fn can_store_data() -> CoreResult<()> {
        let ctx = test_codegen(".byte 123\n.word 123\n.word $fce2")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![123, 123, 0, 0xe2, 0xfc]
        );
        Ok(())
    }

    #[test]
    fn can_store_current_pc_as_data() -> CoreResult<()> {
        let ctx = test_codegen(".word *\n.word foo - *\nfoo: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x00, 0xc0, 0x02, 0x00, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_store_csv_data() -> CoreResult<()> {
        let ctx = test_codegen(".word 123, foo, 234\nfoo: nop")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![123, 0, 0x06, 0xc0, 234, 0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_store_text() -> CoreResult<()> {
        let ctx = test_codegen(".text petscii \"abc\"")?;
        assert_eq!(ctx.current_segment().range_data(), vec![65, 66, 67]);

        Ok(())
    }

    #[test]
    fn can_perform_operations_on_labels() -> CoreResult<()> {
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
    fn can_perform_branch_calculations() -> CoreResult<()> {
        let ctx = test_codegen("foo: nop\nbne foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea, 0xd0, 0xfd]);
        let ctx = test_codegen("bne foo\nfoo: nop")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xd0, 0x00, 0xea]);
        Ok(())
    }

    #[test]
    fn can_use_macros() -> CoreResult<()> {
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
    fn cannot_perform_too_far_branch_calculations() -> CoreResult<()> {
        let many_nops = std::iter::repeat("nop\n").take(140).collect::<String>();
        let src = format!("foo: {}bne foo", many_nops);
        let ast = parse_or_err(
            &Path::new("test.asm"),
            InMemoryParsingSource::new().add("test.asm", &src).into(),
        )?;
        let (_, err) = codegen(ast, CodegenOptions::default());
        assert_eq!(
            format!("{}", err.unwrap()),
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
    fn can_use_wildcard_imports() -> CoreResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import * from \"bar\"\nlda #defined(foo)\nlda #defined(bar)",
            )
            .add("bar", "foo: nop\nbar: nop")
            .into();

        let ctx = test_codegen_parsing_source(src, CodegenOptions::default())?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xea, 0xa9, 1, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_use_wildcard_imports_as() -> CoreResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import * as baz from \"bar\"\nlda #defined(baz.foo)\nlda #defined(baz.bar)",
            )
            .add("bar", "foo: nop\nbar: nop")
            .into();

        let ctx = test_codegen_parsing_source(src, CodegenOptions::default())?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xea, 0xa9, 1, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_use_specific_imports() -> CoreResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import bar from \"bar\"\nlda #defined(foo)\nlda #defined(bar)",
            )
            .add("bar", "foo: nop\nbar: nop")
            .into();

        let ctx = test_codegen_parsing_source(src, CodegenOptions::default())?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xea, 0xea, 0xa9, 0, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_use_specific_imports_as() -> CoreResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                "lda baz\n.import bar as baz from \"bar\"\nlda #defined(foo)\nlda #defined(bar)\nlda #defined(baz)",
            )
            .add("bar", "foo: nop\nbar: nop")
            .into();

        let ctx = test_codegen_parsing_source(src, CodegenOptions::default())?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xad, 4, 0xc0, 0xea, 0xea, 0xa9, 0, 0xa9, 0, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_have_imports_with_block() -> CoreResult<()> {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".import foo from \"bar\" { .const CONST = 1 }")
            .add("bar", "foo: lda #CONST")
            .into();

        let ctx = test_codegen_parsing_source(src, CodegenOptions::default())?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa9, 1]);
        Ok(())
    }

    #[test]
    fn cannot_import_unknown_export() {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".import foo from \"bar\"")
            .add("bar", "nop")
            .into();

        let err = test_codegen_parsing_source(src, CodegenOptions::default())
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:9: error: unknown identifier: foo"
        );
    }

    #[test]
    fn cannot_have_duplicate_imports() {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import foo from \"bar\"\n.import foo as foo2 from \"bar\"\n.import foo from \"bar\"",
            )
            .add("bar", "foo: nop")
            .into();

        let err = test_codegen_parsing_source(src, CodegenOptions::default())
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:3:9: error: cannot import an already defined symbol: foo"
        );
    }

    #[test]
    fn cannot_have_duplicate_imports_via_wildcard_import() {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".import * from \"bar\"\n.import * from \"bar\"")
            .add("bar", "foo: nop")
            .into();

        let err = test_codegen_parsing_source(src, CodegenOptions::default())
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:9: error: cannot import an already defined symbol: foo"
        );
    }

    #[test]
    fn can_perform_source_map_lookups() -> CoreResult<()> {
        let ctx = test_codegen("lda $d020\nsta $d021")?;
        let offset = ctx.source_map().address_to_offset(0xc003).unwrap();
        let sl = ctx.tree.code_map.look_up_span(offset.span);
        assert_eq!(offset.pc, 0xc003..0xc005);
        assert_eq!(sl.file.name(), "test.asm");
        assert_eq!(sl.begin.line, 1);
        assert_eq!(sl.begin.column, 0);
        assert_eq!(sl.end.line, 1);
        assert_eq!(sl.end.column, 9);

        let offsets = ctx
            .source_map
            .line_col_to_offsets(&ctx.tree.code_map, "test.asm", 1, 4);
        let offset = offsets[0];
        let sl = ctx.tree.code_map.look_up_span(offset.span);
        assert_eq!(offset.pc, 0xc003..0xc005);
        assert_eq!(sl.file.name(), "test.asm");
        assert_eq!(sl.begin.line, 1);
        assert_eq!(sl.begin.column, 0);
        assert_eq!(sl.end.line, 1);
        assert_eq!(sl.end.column, 9);

        Ok(())
    }

    #[test]
    fn can_compile_specified_test() -> CoreResult<()> {
        let src = ".test a { nop }\n.test b { asl }";
        let ctx = test_codegen_with_options(
            src,
            CodegenOptions {
                active_test: Some("a".into()),
                ..Default::default()
            },
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xea]);

        let ctx = test_codegen_with_options(
            src,
            CodegenOptions {
                active_test: Some("b".into()),
                ..Default::default()
            },
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0x0a]);
        Ok(())
    }

    #[test]
    fn no_tests_compiled_when_no_active_test_specified() -> CoreResult<()> {
        let ctx = test_codegen_with_options(
            ".test a { nop }\nasl",
            CodegenOptions {
                active_test: None,
                ..Default::default()
            },
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0x0a]);

        Ok(())
    }

    pub(super) fn test_codegen(code: &str) -> CoreResult<CodegenContext> {
        test_codegen_parsing_source(
            InMemoryParsingSource::new().add("test.asm", &code).into(),
            CodegenOptions::default(),
        )
    }

    pub(super) fn test_codegen_with_options(
        code: &str,
        options: CodegenOptions,
    ) -> CoreResult<CodegenContext> {
        test_codegen_parsing_source(
            InMemoryParsingSource::new().add("test.asm", &code).into(),
            options,
        )
    }

    pub(super) fn test_codegen_parsing_source(
        src: Arc<Mutex<dyn ParsingSource>>,
        options: CodegenOptions,
    ) -> CoreResult<CodegenContext> {
        let ast = parse_or_err(&Path::new("test.asm"), src)?;
        let (ctx, err) = codegen(ast, options);
        match err {
            Some(e) => Err(e),
            None => Ok(ctx.unwrap()),
        }
    }
}
