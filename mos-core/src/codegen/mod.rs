mod analysis;
mod config_extractor;
mod config_validator;
mod evaluator;
mod opcodes;
mod program_counter;
mod segment;
mod source_map;
mod symbols;
mod text_encoding;

pub use analysis::*;
pub use evaluator::*;
pub use program_counter::*;
pub use segment::*;
pub use source_map::*;
pub use symbols::*;

use crate::codegen::config_validator::ConfigValidator;
use crate::codegen::opcodes::get_opcode_bytes;
use crate::codegen::source_map::SourceMap;
use crate::codegen::text_encoding::encode_text;
use crate::errors::{CoreResult, Diagnostics};
use crate::io::BankOptions;
use crate::parser::code_map::Span;
use crate::parser::{
    AddressingMode, Block, DataSize, Expression, Identifier, IdentifierPath, ImportArgs, Located,
    Mnemonic, ParseTree, TextEncoding, Token, VariableType,
};
use codespan_reporting::diagnostic::Diagnostic;
use fs_err as fs;
use indexmap::map::IndexMap;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct CodegenOptions {
    pub pc: ProgramCounter,
    pub active_test: Option<IdentifierPath>,
    pub predefined_constants: HashMap<String, i64>,

    /// When invoking a macro, do we want the source map to point to the macro's _invocation_ site, instead of the _definition_?
    /// This is useful for source map listings where every invocation will then be associated with the full macro output.
    ///
    /// However, if we want to be able to set breakpoints in the middle of macro code, then we do NOT want to move the source map offsets.
    pub move_macro_source_map_to_invocation: bool,

    /// Do we want to do more work to improve analysis results?
    /// For instance, when a macro is defined but not invoked, no symbols or other analysis information will be generated.
    /// When this flag is enabled, an uninvoked macro will get invoked once without passing in any parameters.
    pub enable_greedy_analysis: bool,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            pc: ProgramCounter::new(0xc000),
            active_test: None,
            predefined_constants: HashMap::new(),
            move_macro_source_map_to_invocation: false,
            enable_greedy_analysis: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolData {
    MacroDefinition(MacroDefinition),
    Number(i64),
    /// A symbol that was defined to prevent 'unknown identifier' errors, but doesn't hold any data otherwise
    Placeholder,
    String(String),
}

impl ToString for SymbolData {
    fn to_string(&self) -> String {
        match self {
            SymbolData::MacroDefinition(def) => def.id.data.to_string(),
            SymbolData::Number(val) => val.to_string(),
            SymbolData::Placeholder => "<placeholder>".into(),
            SymbolData::String(str) => str.clone(),
        }
    }
}

impl From<MacroDefinition> for SymbolData {
    fn from(val: MacroDefinition) -> Self {
        Self::MacroDefinition(val)
    }
}

impl From<i64> for SymbolData {
    fn from(val: i64) -> Self {
        Self::Number(val)
    }
}

impl From<bool> for SymbolData {
    fn from(val: bool) -> Self {
        Self::Number(if val { 1 } else { 0 })
    }
}

impl From<String> for SymbolData {
    fn from(val: String) -> Self {
        Self::String(val)
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

    pub fn try_as_string(&self) -> Option<String> {
        match &self {
            SymbolData::Number(val) => Some(val.to_string()),
            SymbolData::String(val) => Some(val.clone()),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SymbolType {
    Label,
    TestCase,
    MacroArgument,
    Constant,
    Variable,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub pass_idx: usize,
    pub span: Option<Span>,
    pub segment: Option<Identifier>,
    pub data: SymbolData,
    pub ty: SymbolType,
}

impl Symbol {
    fn read_only(&self) -> bool {
        self.ty != SymbolType::Variable
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroDefinition {
    id: Located<Identifier>,
    args: Vec<Located<Identifier>>,
    block: Vec<Token>,
}

pub struct CodegenContext {
    tree: Arc<ParseTree>,
    options: CodegenOptions,

    analysis: Analysis,

    pass_idx: usize,

    segments: IndexMap<Identifier, Segment>,
    current_segment: Option<Identifier>,
    banks: IndexMap<Identifier, BankOptions>,

    functions: FunctionMap,

    symbols: SymbolTable<Symbol>,
    undefined: HashSet<UndefinedSymbol>,
    current_scope: IdentifierPath,
    current_scope_nx: SymbolIndex,

    next_macro_scope_id: usize,

    test_elements: Vec<TestElement>,

    source_map: SourceMap,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct UndefinedSymbol {
    scope_nx: SymbolIndex,
    id: IdentifierPath,
    span: Option<Span>,
}

pub enum TestElement {
    Assertion(Assertion),
    Trace(Trace),
}

pub struct Assertion {
    pub expr: Located<Expression>,
    pub snapshot: SymbolSnapshot,
    pub failure_message: Option<String>,
}

pub struct Trace {
    pub exprs: Vec<Located<Expression>>,
    pub snapshot: SymbolSnapshot,
}

impl CodegenContext {
    fn new(tree: Arc<ParseTree>, options: CodegenOptions) -> Self {
        let analysis = Analysis::new(tree.clone());

        Self {
            tree,
            options,
            analysis,
            pass_idx: 0,
            segments: IndexMap::new(),
            current_segment: None,
            banks: IndexMap::new(),
            functions: HashMap::new(),
            symbols: SymbolTable::default(),
            undefined: HashSet::new(),
            current_scope: IdentifierPath::empty(),
            current_scope_nx: SymbolIndex::new(0),
            next_macro_scope_id: 0,
            test_elements: vec![],
            source_map: SourceMap::default(),
        }
    }

    pub fn analysis(&self) -> &Analysis {
        &self.analysis
    }

    pub fn segments(&self) -> &IndexMap<Identifier, Segment> {
        &self.segments
    }

    pub fn banks(&self) -> &IndexMap<Identifier, BankOptions> {
        &self.banks
    }

    pub fn functions(&self) -> &FunctionMap {
        &self.functions
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

    pub fn remove_test_elements(&mut self) -> Vec<TestElement> {
        std::mem::take(&mut self.test_elements)
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    fn register_all_segment_symbols(&mut self) -> CoreResult<()> {
        // Register symbols for all segments
        let path: IdentifierPath = "segments".into();

        let segments = std::mem::replace(&mut self.segments, IndexMap::new());
        for (name, segment) in &segments {
            let path = path.join(name);

            self.add_symbol(
                path.join("start"),
                self.symbol(None, segment.range().start as i64, SymbolType::Constant),
            )?;

            self.add_symbol(
                path.join("end"),
                self.symbol(None, segment.range().end as i64, SymbolType::Constant),
            )?;
        }
        self.segments = segments;
        Ok(())
    }

    fn after_pass(&mut self) -> CoreResult<()> {
        self.register_all_segment_symbols()?;
        Ok(())
    }

    pub fn symbol<S: Into<Option<Span>>, D: Into<SymbolData>>(
        &self,
        span: S,
        data: D,
        ty: SymbolType,
    ) -> Symbol {
        Symbol {
            pass_idx: self.pass_idx,
            segment: self.current_segment.clone(),
            span: span.into(),
            data: data.into(),
            ty,
        }
    }

    fn next_pass(&mut self) {
        self.pass_idx += 1;
        self.next_macro_scope_id = 0;

        log::trace!("\n* NEXT PASS ({}) *", self.pass_idx);
        self.segments.values_mut().for_each(|s| s.reset());
        self.test_elements.clear();
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

    fn add_symbol<I: Into<IdentifierPath>>(
        &mut self,
        id: I,
        symbol: Symbol,
    ) -> CoreResult<SymbolIndex> {
        let span = symbol.span;
        let ty = symbol.ty.clone();
        let id = id.into();
        let path = self.current_scope.join(&id);
        log::trace!(
            "Inserting symbol: '{}' with value '{:?}' (inside scope '{}')",
            &id,
            symbol,
            &self.current_scope
        );

        let mut maybe_require_new_pass = false;
        let symbol_nx = self.symbols.try_index(self.current_scope_nx, &id);
        log::trace!("Trying to lookup symbol '{}': {:?}", &id, symbol_nx);
        let symbol_nx = match symbol_nx {
            Some(symbol_nx) => {
                match self.symbols.try_get_mut(symbol_nx) {
                    Some(existing) => {
                        if existing.ty != symbol.ty
                            || existing.read_only() != symbol.read_only()
                            || (existing.pass_idx == symbol.pass_idx
                                && existing.data != symbol.data
                                && existing.read_only())
                        {
                            let span = symbol.span.expect("no span provided");
                            return Err(Diagnostic::error()
                                .with_message(format!("cannot redefine symbol: {}", &path))
                                .with_labels(vec![span.to_label()])
                                .into());
                        }

                        // If the symbol already existed but with a different value,
                        // mark it as undefined so we will trigger another pass
                        if existing.data != symbol.data {
                            log::trace!(
                                "`--> Symbol already existed, but has changed. Old value was: {:?}",
                                existing
                            );

                            maybe_require_new_pass = true;
                        }

                        log::trace!("Symbol was updated with index: {:?}", symbol_nx);
                        *existing = symbol;
                    }
                    None => {
                        log::trace!(
                            "Symbol was updated from 'no data' with index: {:?}",
                            symbol_nx
                        );
                        self.symbols.update_data(symbol_nx, symbol);
                        maybe_require_new_pass = true;
                    }
                }

                symbol_nx
            }
            None => {
                let (parent, id) = path.clone().split();
                let parent_nx = self.symbols.ensure_index(self.symbols.root, &parent);
                let nx = self.symbols.insert(parent_nx, id, symbol);
                log::trace!("Symbol was inserted with index: {:?}", nx);
                nx
            }
        };

        // Variables don't require a new pass, since if they update somewhere in the assembly process
        // they would keep triggering new passes
        if maybe_require_new_pass && ty != SymbolType::Variable {
            self.undefined.insert(UndefinedSymbol {
                scope_nx: self.current_scope_nx,
                id,
                span,
            });
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
        if let Some(nx) = self.symbols.try_index(self.symbols.root, path) {
            self.symbols.remove(nx);
        }
    }

    pub fn get_evaluator(&self) -> Evaluator {
        self.get_evaluator_for_scope(self.current_scope_nx)
    }

    pub fn get_evaluator_for_scope(&self, scope_nx: SymbolIndex) -> Evaluator {
        Evaluator::new(
            scope_nx,
            &self.symbols,
            &self.functions,
            self.try_current_target_pc(),
        )
    }

    fn emit(&mut self, span: Span, bytes: &[u8]) -> CoreResult<()> {
        match &self.current_segment {
            Some(name) => {
                let segment = self.segments.get_mut(name).unwrap();
                log::trace!(
                    "Emitting to segment '{}' {}: {:?}",
                    name,
                    segment.pc(),
                    &bytes
                );
                self.source_map.add(
                    self.current_scope_nx,
                    span,
                    segment.target_pc(),
                    bytes.len(),
                );
                if segment.emit(bytes) {
                    Ok(())
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!("segment '{}' is out of range", name))
                        .with_labels(vec![span.to_label()])
                        .into());
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
        let mut errors = Diagnostics::default();
        for token in tokens {
            if let Err(result) = self.emit_token(token) {
                errors.extend(result);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn emit_token(&mut self, token: &Token) -> CoreResult<()> {
        match token {
            Token::Align { value, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    if let Some(align) = self.evaluate_expression_as_i64(value, true)? {
                        let padding = (align - (pc.as_i64() % align)) as usize;
                        let mut bytes = Vec::new();
                        bytes.resize(padding, 0u8);
                        self.emit(value.span, &bytes)?;
                    }
                }
            }
            Token::Assert {
                value,
                failure_message,
                ..
            } => {
                if self.try_current_target_pc().is_some() && self.options.active_test.is_some() {
                    let extracted_evaluator = self.get_evaluator().snapshot();

                    let interpolated_failure_message;
                    if let Some(fm) = &failure_message {
                        interpolated_failure_message = Some(
                            self.get_evaluator()
                                .interpolate(fm, true)
                                .map_err(|e| self.map_evaluation_error(e))?,
                        );
                    } else {
                        interpolated_failure_message = None;
                    }
                    self.test_elements.push(TestElement::Assertion(Assertion {
                        expr: value.clone(),
                        snapshot: extracted_evaluator,
                        failure_message: interpolated_failure_message,
                    }));
                }
            }
            Token::Braces { block, scope } => {
                self.with_scope(scope, Some(block), |s| s.emit_tokens(&block.inner))?;
            }
            Token::Data { values, size } => {
                let exprs = values.iter().map(|(expr, _comma)| expr).collect_vec();
                for expr in exprs {
                    if let Some(value) = self.evaluate_expression_as_i64(expr, true)? {
                        let bytes = match &size.data {
                            DataSize::Byte => vec![value as u8],
                            DataSize::Word => (value as u16).to_le_bytes().to_vec(),
                            DataSize::Dword => (value as u32).to_le_bytes().to_vec(),
                        };
                        self.emit(expr.span, &bytes)?;
                    } else {
                        self.emit(expr.span, &[])?;
                    }
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
                        "bank" => {
                            let extractor = ConfigValidator::new()
                                .required("name")
                                .allowed("create-segment")
                                .allowed("size")
                                .allowed("fill")
                                .allowed("filename")
                                .extract(id.span, &kvps)?;
                            let name = Identifier::new(extractor.get_string(self, "name")?);

                            let opts = BankOptions {
                                name: name.clone(),
                                size: extractor.try_get_i64(self, "size")?.map(|s| s as usize),
                                fill: extractor.try_get_i64(self, "fill")?.map(|s| s as u8),
                                create_segment: extractor
                                    .try_get_i64(self, "create-segment")?
                                    .map(|s| s != 0)
                                    .unwrap_or_default(),
                                filename: extractor.try_get_string(self, "filename")?,
                            };
                            let create_segment = opts.create_segment;
                            self.banks.insert(name.clone(), opts);

                            if create_segment {
                                let segment_opts = SegmentOptions {
                                    bank: Some(name.clone()),
                                    ..Default::default()
                                };
                                self.segments.insert(name, Segment::new(segment_opts));
                            }
                        }
                        "segment" => {
                            let extractor = ConfigValidator::new()
                                .required("name")
                                .allowed("start")
                                .allowed("pc")
                                .allowed("write")
                                .allowed("bank")
                                .extract(id.span, &kvps)?;

                            let mut opts = SegmentOptions::default();
                            let name = Identifier::new(extractor.get_string(self, "name")?);
                            match extractor.try_get_i64(self, "start") {
                                Ok(Some(val)) => {
                                    log::trace!(
                                        "Segment '{}' was able to evaluate the 'start' to: {}",
                                        name,
                                        val
                                    );
                                    opts.initial_pc = val.into()
                                }
                                Ok(None) => {
                                    log::trace!(
                                        "Segment '{}' has a default value for 'start'",
                                        name,
                                    );
                                    opts.initial_pc = 0.into()
                                }
                                Err(_) => {
                                    // Will be marked as undefined and retried later
                                    log::trace!(
                                        "Segment '{}' was not able to evaluate the 'start'",
                                        name
                                    );
                                    opts.initial_pc = 0.into();
                                }
                            }
                            if let Some(write) = extractor.try_get_i64(self, "write")? {
                                opts.write = write != 0;
                            }
                            opts.bank =
                                extractor.try_get_string(self, "bank")?.map(Identifier::new);
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
                            return Err(Diagnostic::error()
                                .with_message("Unknown definition type")
                                .with_labels(vec![id.span.to_label()])
                                .into());
                        }
                    }
                }
            }
            Token::File { filename, .. } => {
                let span = filename.lquote.span;
                let evaluated_filename = self
                    .get_evaluator()
                    .interpolate(filename, true)
                    .map_err(|e| self.map_evaluation_error(e))?;
                let source_file: PathBuf = self.tree.code_map.look_up_span(span).file.name().into();
                let filename = match source_file.parent() {
                    Some(parent) => parent.join(&evaluated_filename),
                    None => PathBuf::from(&evaluated_filename),
                };
                match fs::read(&filename) {
                    Ok(bytes) => {
                        self.emit(span, &bytes)?;
                    }
                    Err(_) => {
                        return Err(Diagnostic::error()
                            .with_message(format!("file not found: {}", filename.to_string_lossy()))
                            .with_labels(vec![span.to_label()])
                            .into());
                    }
                }
            }
            Token::If {
                value, if_, else_, ..
            } => {
                if let Some(value) = self.evaluate_expression_as_i64(value, true)? {
                    let emit_if = value != 0;
                    if emit_if {
                        self.emit_tokens(&if_.inner)?;
                    } else if self.options.enable_greedy_analysis {
                        self.with_dummy_segment(|s| s.emit_tokens(&if_.inner))?;
                    }

                    if let Some(e) = else_ {
                        if !emit_if {
                            self.emit_tokens(&e.inner)?;
                        } else if self.options.enable_greedy_analysis {
                            self.with_dummy_segment(|s| s.emit_tokens(&e.inner))?;
                        }
                    }
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
                        span: filename.span(),
                    });

                    self.with_scope(import_scope, block.as_ref(), |s| {
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
                                            self.undefined.insert(UndefinedSymbol {
                                                scope_nx: self.current_scope_nx,
                                                id: original_path.clone(),
                                                span: Some(arg.span),
                                            });
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
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "cannot import an already defined symbol: {}",
                                        new_path
                                    ))
                                    .with_labels(vec![span.to_label()])
                                    .into());
                            }
                        }
                    }
                }
            }
            Token::Instruction(i) => {
                let mut full_span = i.mnemonic.span;
                if let Some(operand_span) = i.operand.as_ref().map(|o| o.expr.span) {
                    full_span = operand_span.merge(full_span);
                }

                let data = match &i.operand {
                    Some(op) => self
                        .evaluate_expression_as_i64(&op.expr, true)?
                        .map(|value| {
                            let register_suffix = op.suffix.as_ref().map(|s| s.register.data);
                            (value, op.addressing_mode, register_suffix)
                        }),
                    None => Some((0, AddressingMode::Implied, None)),
                };

                if let Some((value, am, suffix)) = data {
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
                                return Err(Diagnostic::error()
                                    .with_message("branch too far")
                                    .with_labels(vec![i.mnemonic.span.to_label()])
                                    .into());
                            }
                        }
                        _ => value,
                    };

                    match get_opcode_bytes(i.mnemonic.data, am, suffix, value) {
                        Ok(bytes) => self.emit(full_span, &bytes)?,
                        Err(()) => {
                            // Emit 'BRK' so at least the code map gets updated
                            self.emit(full_span, &[0])?;
                            return Err(Diagnostic::error()
                                .with_message("invalid instruction")
                                .with_labels(vec![full_span.to_label()])
                                .into());
                        }
                    }
                } else {
                    // Expression could not be evaluated, still make sure to emit 'nothing' so code map gets updated
                    self.emit(full_span, &[])?;
                }
            }
            Token::Label { id, block, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    self.add_symbol(
                        id.data.clone(),
                        self.symbol(id.span, pc.as_i64(), SymbolType::Label),
                    )?;
                }

                if let Some(b) = block {
                    self.with_scope(&id.data, Some(b), |s| s.emit_tokens(&b.inner))?;
                }
            }
            Token::Loop {
                expr,
                loop_scope,
                block,
                ..
            } => {
                if let Some(loop_count) = self.evaluate_expression_as_i64(expr, true)? {
                    for index in 0..loop_count {
                        self.with_scope(loop_scope, Some(block), |s| {
                            s.add_symbol(
                                "index",
                                s.symbol(expr.span, index, SymbolType::Constant),
                            )?;
                            let result = s.emit_tokens(&block.inner);
                            s.remove_symbol("index");
                            result
                        })?;
                    }
                }
            }
            Token::MacroDefinition {
                id, args, block, ..
            } => {
                let args = args.iter().map(|(arg, _)| arg.clone()).collect();
                self.add_symbol(
                    &id.data,
                    self.symbol(
                        id.span,
                        MacroDefinition {
                            id: id.clone(),
                            args,
                            block: block.inner.clone(),
                        },
                        SymbolType::Constant,
                    ),
                )?;
            }
            Token::MacroInvocation { id: name, args, .. } => {
                let def = self
                    .get_evaluator()
                    .get_symbol_filtered(
                        self.current_scope_nx,
                        &IdentifierPath::from(&name.data),
                        |s| matches!(s.data, SymbolData::MacroDefinition(_)),
                    )
                    .map(|(symbol_nx, symbol)| {
                        (symbol_nx, symbol.data.as_macro_definition().clone())
                    });

                if let Some((macro_nx, def)) = def {
                    let parent_scope = self.current_scope_nx;
                    self.symbol_definition(macro_nx)
                        .add_usage(DefinitionLocation {
                            parent_scope,
                            span: name.span,
                        });

                    self.get_evaluator()
                        .expect_args(name.span, args.len(), def.args.len())
                        .map_err(|e| self.map_evaluation_error(e))?;

                    let macro_scope =
                        Identifier::new(format!("$macro_{}", self.next_macro_scope_id));
                    self.next_macro_scope_id += 1;

                    self.with_scope(&macro_scope, None, |s| {
                        for (idx, arg_name) in def.args.iter().enumerate() {
                            let (expr, _) = args.get(idx).unwrap();

                            // Regardless if evaluation succeeds, we should create the macro argument symbol here, because
                            // it will be undefined otherwise
                            let value = s
                                .evaluate_expression(expr, true)?
                                .unwrap_or(SymbolData::Placeholder);
                            s.add_symbol(
                                &arg_name.data,
                                s.symbol(arg_name.span, value, SymbolType::MacroArgument),
                            )?;
                        }

                        s.emit_tokens(&def.block)?;

                        if s.options.move_macro_source_map_to_invocation {
                            // Move all source map offsets that refer to the macro definition's span to the macro invocation's span
                            // to make sure that the emitted bytes show up at the invocation site when generating a listing file
                            s.source_map
                                .move_offsets(s.current_scope_nx, parent_scope, name.span);
                        }

                        Ok(())
                    })?;
                } else {
                    self.undefined.insert(UndefinedSymbol {
                        scope_nx: self.current_scope_nx,
                        id: name.data.clone().into(),
                        span: Some(name.span),
                    });
                }
            }
            Token::ProgramCounterDefinition { value, .. } => {
                if let Some(pc) = self.evaluate_expression_as_i64(value, true)? {
                    if let Some(seg) = self.try_current_segment_mut() {
                        seg.set_pc(pc);
                    }
                }
            }
            Token::Segment { id, block, .. } => {
                if let Some(segment_id) = self
                    .evaluate_expression_as_string(id, true)?
                    .map(Identifier::new)
                {
                    if !self.segments.contains_key(&segment_id) {
                        return Err(Diagnostic::error()
                            .with_message(format!("unknown identifier: {}", id.data))
                            .with_labels(vec![id.span.to_label()])
                            .into());
                    };

                    match block {
                        Some(block) => {
                            let old_segment =
                                std::mem::replace(&mut self.current_segment, Some(segment_id));
                            self.emit_tokens(&block.inner)?;
                            self.current_segment = old_segment;
                        }
                        None => {
                            self.current_segment = Some(segment_id);
                        }
                    }
                }
            }
            Token::Test { id, block, .. } => {
                if let Some(pc) = self.try_current_target_pc() {
                    if let Some(test_name) = self.evaluate_expression_as_string(&id, true)? {
                        let test_name = IdentifierPath::from(test_name.as_str());
                        let should_add_test_symbol = match &self.options.active_test {
                            Some(active_test) => {
                                if &self.current_scope.join(&test_name) == active_test {
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
                                &test_name,
                                self.symbol(id.span, pc.as_i64(), SymbolType::TestCase),
                            )?;
                        }
                    }
                }
            }
            Token::Text { encoding, text, .. } => {
                let encoding = encoding
                    .as_ref()
                    .map(|e| e.data)
                    .unwrap_or(TextEncoding::Ascii);

                if let Some(eval) = self.evaluate_expression_as_string(text, true)? {
                    self.emit(text.span, &encode_text(&eval, encoding))?;
                }
            }
            Token::Trace { args, .. } => {
                if self.try_current_target_pc().is_some() && self.options.active_test.is_some() {
                    let exprs = args.iter().map(|a| a.0.clone()).collect_vec();
                    let extracted_evaluator = self.get_evaluator().snapshot();

                    self.test_elements.push(TestElement::Trace(Trace {
                        exprs,
                        snapshot: extracted_evaluator,
                    }));
                }
            }
            Token::VariableDefinition { ty, id, value, .. } => {
                if let Some(value) = self.evaluate_expression(&value, true)? {
                    let ty = match &ty.data {
                        VariableType::Constant => SymbolType::Constant,
                        VariableType::Variable => SymbolType::Variable,
                    };
                    self.add_symbol(id.data.clone(), self.symbol(id.span, value, ty))?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn map_evaluation_error(&self, error: EvaluationError) -> Diagnostics {
        Diagnostic::error()
            .with_message(error.message)
            .with_labels(vec![error.span.to_label()])
            .into()
    }

    pub fn evaluate_expression(
        &mut self,
        expr: &Located<Expression>,
        track_usage: bool,
    ) -> CoreResult<Option<SymbolData>> {
        let ctx = self.get_evaluator();
        let result = ctx
            .evaluate_expression(expr, track_usage)
            .map_err(|e| self.map_evaluation_error(e))?;
        if track_usage {
            for usage in ctx.usages() {
                self.analysis.add_symbol_usage(
                    &self.symbols,
                    self.current_scope_nx,
                    &usage.path.data,
                    usage.path.span,
                );

                if usage.symbol_index.is_none() {
                    self.undefined.insert(UndefinedSymbol {
                        scope_nx: self.current_scope_nx,
                        id: usage.path.data,
                        span: Some(usage.path.span),
                    });
                }
            }
        }
        Ok(result)
    }

    pub fn evaluate_expression_as_i64(
        &mut self,
        expr: &Located<Expression>,
        track_usage: bool,
    ) -> CoreResult<Option<i64>> {
        let result = self.evaluate_expression(expr, track_usage)?;
        match result {
            Some(data) => match data {
                SymbolData::Number(n) => Ok(Some(n)),
                _ => Err(Diagnostic::error()
                    .with_message(format!("'{}' does not evaluate to an integer", &expr.data))
                    .with_labels(vec![expr.span.to_label()])
                    .into()),
            },
            None => Ok(None),
        }
    }

    /// Also deals with the interpolation of any symbols delimited with {}
    pub fn evaluate_expression_as_string(
        &mut self,
        expr: &Located<Expression>,
        track_usage: bool,
    ) -> CoreResult<Option<String>> {
        let result = self.evaluate_expression(expr, track_usage)?;
        match result {
            Some(SymbolData::String(data)) => Ok(Some(data)),
            Some(_) => Err(Diagnostic::error()
                .with_message(format!("'{}' does not evaluate to a string", &expr.data))
                .with_labels(vec![expr.span.to_label()])
                .into()),
            None => Ok(None),
        }
    }

    fn symbol_definition(&mut self, symbol_nx: SymbolIndex) -> &mut Definition {
        self.analysis
            .get_or_create_definition_mut(DefinitionType::Symbol(symbol_nx))
    }

    fn with_dummy_segment<F: FnOnce(&mut Self) -> CoreResult<()>>(
        &mut self,
        f: F,
    ) -> CoreResult<()> {
        let prev_segment = self.current_segment.clone();
        self.segments
            .insert("$dummy".into(), Segment::new(SegmentOptions::default()));
        self.current_segment = Some(Identifier::new("$dummy"));
        let result = f(self);
        self.segments.remove(&Identifier::new("$dummy"));
        self.current_segment = prev_segment;
        result
    }

    fn with_scope<F: FnOnce(&mut Self) -> CoreResult<()>>(
        &mut self,
        scope: &Identifier,
        add_symbols_for_block: Option<&Block>,
        f: F,
    ) -> CoreResult<()> {
        let old_scope_nx = self.current_scope_nx;
        self.current_scope.push(scope);
        self.current_scope_nx = self
            .symbols
            .ensure_index(self.symbols.root, &self.current_scope);
        if let Some(span) = add_symbols_for_block.map(|b| b.lparen.span) {
            self.try_current_target_pc().map(|pc| {
                self.add_symbol("-", self.symbol(span, pc.as_i64(), SymbolType::Constant))
            });
        }
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
        if let Some(span) = add_symbols_for_block.map(|b| b.rparen.span) {
            self.try_current_target_pc().map(|pc| {
                self.add_symbol("+", self.symbol(span, pc.as_i64(), SymbolType::Constant))
            });
        }
        self.current_scope_nx = old_scope_nx;
        self.current_scope.pop();
        result
    }

    pub fn register_fn<F: 'static + FunctionCallback + Send + Sync>(
        &mut self,
        name: &str,
        function: F,
    ) {
        self.functions
            .insert(name.into(), Arc::new(Mutex::new(function)));
    }

    fn register_default_fns(&mut self) {
        struct DefinedFn;
        impl FunctionCallback for DefinedFn {
            fn expected_args(&self) -> usize {
                1
            }

            fn apply(
                &mut self,
                ctx: &Evaluator,
                args: &[&Located<Expression>],
            ) -> EvaluationResult<Option<SymbolData>> {
                let expr = args.first().unwrap();
                match ctx.evaluate_expression(expr, false) {
                    Ok(result) => {
                        if result.is_some() {
                            Ok(Some(1.into()))
                        } else {
                            Ok(Some(0.into()))
                        }
                    }
                    Err(_) => Ok(Some(0.into())),
                }
            }
        }
        self.register_fn("defined", DefinedFn {});
    }

    pub fn finalize(&mut self) -> CoreResult<()> {
        // If no banks were defined, define a default one
        if self.banks.is_empty() {
            self.banks
                .insert("default".into(), BankOptions::new("default"));

            // Assign all segments that don't have a bank to this bank
            for segment in self.segments.values_mut() {
                if segment.options().bank.is_none() {
                    segment.options_mut().bank = Some("default".into());
                }
            }
        }

        // If there is only a 'default' segment, assign it to the first bank we have (if not yet assigned)
        if self.segments.len() == 1 {
            let segment = self.segments.values_mut().next().unwrap();
            segment.options_mut().bank = Some(self.banks.keys().next().unwrap().clone());
        }

        // Check if all segments now have a bank
        let mut seg_err = Diagnostics::default();
        for (segment_name, segment) in self.segments.iter() {
            if segment.options().bank.is_none() {
                seg_err.push(Diagnostic::error().with_message(format!(
                    "Segment '{}' is not assigned to any bank",
                    segment_name
                )));
            }
        }
        if !seg_err.is_empty() {
            return Err(seg_err);
        }

        // Do we want to invoke uninvoked macros to generate analysis info?
        if self.options.enable_greedy_analysis {
            // We don't want to actually emit any tokens into an existing segment, so we'll create a dummy one
            self.with_dummy_segment(|s| {
                let mut macro_defs = vec![];
                for (symbol_nx, symbol) in s.symbols.all().values() {
                    if let SymbolData::MacroDefinition(def) = &symbol.data {
                        macro_defs.push((*symbol_nx, def.clone()));
                    }
                }

                for (symbol_nx, def) in macro_defs {
                    if s.symbol_definition(symbol_nx).is_unused() {
                        let _ = s.emit_tokens(&def.block);
                    }
                }

                Ok(())
            })?;
        }

        Ok(())
    }
}

pub fn codegen(
    ast: Arc<ParseTree>,
    options: CodegenOptions,
) -> (Option<CodegenContext>, Diagnostics) {
    let mut ctx = CodegenContext::new(ast.clone(), options.clone());
    ctx.register_default_fns();
    for (name, val) in &options.predefined_constants {
        ctx.symbols.insert(
            ctx.symbols.root,
            name.as_str(),
            ctx.symbol(None, *val, SymbolType::Constant),
        );
    }

    #[cfg(test)]
    const MAX_ITERATIONS: usize = 50;

    #[cfg(not(test))]
    const MAX_ITERATIONS: usize = usize::MAX;

    let mut prev_undefined = HashSet::new();

    let mut errors = Diagnostics::default().with_code_map(&ctx.tree.code_map);
    ctx.pass_idx = 0;
    while ctx.pass_idx != MAX_ITERATIONS {
        match ctx.emit_tokens(&ast.main_file().tokens) {
            Ok(()) => (),
            Err(e) => {
                errors = e.with_code_map(&ctx.tree.code_map);
            }
        }
        ctx.after_pass().expect("Could not finalize pass");

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
            if ctx.undefined.is_empty() {
                break;
            } else {
                // If the same symbols are undefined that were undefined in the previous pass, they are truly undefined.
                if ctx.undefined == prev_undefined {
                    let errors = ctx
                        .undefined
                        .iter()
                        .sorted_by_key(|k| k.id.to_string())
                        .map(|item| {
                            let mut diag = Diagnostic::error()
                                .with_message(format!("unknown identifier: {}", item.id));
                            if let Some(span) = item.span {
                                diag = diag.with_labels(vec![span.to_label()]);
                            }

                            diag
                        })
                        .collect_vec();

                    let e = Diagnostics::from(errors).with_code_map(&ctx.tree.code_map);
                    return (Some(ctx), e);
                }

                prev_undefined = std::mem::take(&mut ctx.undefined);
            }
        }

        ctx.next_pass();
    }

    // We're done!
    if let Err(e) = ctx.finalize() {
        errors.extend(e);
    }

    (Some(ctx), errors)
}

#[cfg(test)]
pub mod tests {
    use super::{codegen, CodegenContext, CodegenOptions};
    use crate::codegen::{DefinitionType, Segment, SymbolIndex};
    use crate::errors::CoreResult;
    use crate::parser::code_map::LineCol;
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
                .get(self.current_segment.as_ref().unwrap())
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
    fn illegal_instructions() -> CoreResult<()> {
        assert_eq!(
            test_codegen("ldx $1234,x").err().unwrap().to_string(),
            "test.asm:1:1: error: invalid instruction"
        );
        assert_eq!(
            test_codegen("inc #$12").err().unwrap().to_string(),
            "test.asm:1:1: error: invalid instruction"
        );
        Ok(())
    }

    #[test]
    fn can_detect_operand_size_mismatch() {
        let err = test_codegen("lda (foo,x)\nfoo: nop").err().unwrap();
        assert_eq!(err.to_string(), "test.asm:1:1: error: invalid instruction");
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
    fn can_define_banks() -> CoreResult<()> {
        let ctx = test_codegen(
            ".define bank {\nname = \"foo\"\nsize = 8192\nfill = 123\nfilename = \"foo.bin\"}\nnop",
        )?;
        let bank = ctx.banks.get(&Identifier::new("foo")).unwrap();
        assert_eq!(bank.size, Some(8192));
        assert_eq!(bank.fill, Some(123));
        assert_eq!(bank.filename, Some("foo.bin".into()));
        assert_eq!(ctx.banks.get(&Identifier::new("default")).is_none(), true);
        Ok(())
    }

    #[test]
    fn can_define_banks_and_segment() -> CoreResult<()> {
        let ctx = test_codegen(".define bank {\nname = \"foo\"\ncreate-segment = true\n}")?;
        assert_eq!(ctx.banks.get(&Identifier::new("foo")).is_some(), true);
        assert_eq!(ctx.segments.get(&Identifier::new("foo")).is_some(), true);
        Ok(())
    }

    #[test]
    fn can_define_banks_with_string_interpolation() -> CoreResult<()> {
        let ctx =
            test_codegen(".macro def(NAME) { .define bank { name = NAME }}\ndef(\"foo\")\nnop")?;
        assert_eq!(ctx.banks.get(&Identifier::new("foo")).is_some(), true);
        assert_eq!(ctx.banks.get(&Identifier::new("default")).is_none(), true);
        Ok(())
    }

    #[test]
    fn can_define_segments() -> CoreResult<()> {
        let ctx = test_codegen(
            ".define segment {\nname = \"foo\"\nstart = 49152\nwrite = false\n}\nnop",
        )?;
        assert_eq!(ctx.get_segment("foo").range(), 0xc000..0xc001);
        assert_eq!(ctx.get_segment("foo").range_data(), vec![0xea]);
        assert_eq!(ctx.get_segment("foo").options().write, false);
        assert_eq!(ctx.try_get_segment("default").is_none(), true);
        Ok(())
    }

    #[test]
    fn can_use_segments() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"
                .define segment { name = "a" start = $1000 }
                .define segment { name = "b" start = $2000 }
                nop
                .segment "b"
                rol
                .segment "a"
                asl
                "#,
        )?;
        assert_eq!(ctx.get_segment("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.get_segment("b").range_data(), vec![0x2a]);
        Ok(())
    }

    #[test]
    fn segment_targets() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"
                .define segment { name = "a" start = $1000 pc = $4000 }
                lda data
                data: nop
                "#,
        )?;
        assert_eq!(
            ctx.get_segment("a").range_data(),
            vec![0xad, 0x03, 0x40, 0xea]
        );
        Ok(())
    }

    #[test]
    fn cannot_exceed_range() {
        let err = test_codegen(".define segment { name = \"a\" start = $ffff }\nnop\nnop")
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
            r#"
                .define segment { name = "a" start = $1000 }
                .define segment { name = "b" start = $2000 }
                nop
                .segment "b" { rol }
                asl
                "#,
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
            r#"
                .define segment { name = "a" start = $1000 }
                .define segment { name = "b" start = $2000 }
                lda foo
                .segment "b" { foo: lda bar }
                bar: nop
                "#,
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
            "test.asm:1:9: error: missing required fields: name\ntest.asm:1:19: error: field not allowed: foo"
        );
    }

    #[test]
    fn segments_can_depend_on_each_other() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"
                .define segment { name = "a" start = $1000 }
                .define segment { name = "b" start = segments.a.end }
                .define segment { name = "c" start = segments.b.end }
                lda data
                nop
                .segment "b" { rol }
                .segment "c" { lsr }
                asl
                data: .byte 1
                "#,
        )?;
        assert_eq!(ctx.get_segment("a").range(), 0x1000..0x1006);
        assert_eq!(ctx.get_segment("b").range(), 0x1006..0x1007);
        assert_eq!(ctx.get_segment("c").range(), 0x1007..0x1008);
        assert_eq!(
            ctx.get_segment("a").range_data(),
            vec![0xad, 0x05, 0x10, 0xea, 0x0a, 0x01]
        );
        assert_eq!(ctx.get_segment("b").range_data(), vec![0x2a]);
        assert_eq!(ctx.get_segment("c").range_data(), vec![0x4a]);
        Ok(())
    }

    #[test]
    fn can_use_numeric_constants() -> CoreResult<()> {
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
    fn strings() -> CoreResult<()> {
        let ctx = test_codegen(".const foo = \"hi\"\n.text foo")?;
        assert_eq!(ctx.current_segment().range_data(), vec![0x68, 0x69]);

        let ctx = test_codegen(".const a = \"hi\"\n.const b = \"ho\"\n.text a + b")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x68, 0x69, 0x68, 0x6f]
        );

        let ctx = test_codegen(".const a = \"hi\"\n.const b = \"{a}ho\"\n.text b")?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0x68, 0x69, 0x68, 0x6f]
        );

        Ok(())
    }

    #[test]
    fn string_interpolation_usage() -> CoreResult<()> {
        let ctx = test_codegen(".const foo = \"hi\"\n.const bar = \"{foo}\"")?;
        let usages = ctx.analysis.find(
            "test.asm",
            LineCol {
                line: 1,
                column: 16, // the 'foo' in {fop}
            },
        );
        assert_eq!(usages.len(), 1);

        if let DefinitionType::Symbol(symbol_nx) = usages.first().unwrap().0 {
            // We should have been referring to 'foo', which is the first symbol defined
            assert_eq!(*symbol_nx, SymbolIndex::new(1));
        } else {
            panic!()
        }

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
        assert_eq!(err.to_string(), "test.asm:1:7: error: unknown identifier: foo\ntest.asm:2:7: error: unknown identifier: foo2");
    }

    #[test]
    fn error_unknown_nested_identifiers() {
        let err = test_codegen("foo: { nop }\nlda foo.blahblah")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:5: error: unknown identifier: foo.blahblah"
        );
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
    fn can_use_forward_declared_values_in_macros() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"
            .macro macro(end) {
                .const end2 = end - 1
                .byte <end2, >end2
            }
            .define bank {
                name = "first"
                create-segment = true
            }
            .segment "first" {
                macro(segments.second.end)
            }                
            
            .define bank {
                name = "second"
                create-segment = true
            }
            .segment "second" { nop }
            "#,
        )?;
        assert_eq!(
            ctx.segments
                .get(&Identifier::new("first"))
                .unwrap()
                .range_data(),
            vec![0x00, 0x20]
        );
        assert_eq!(
            ctx.segments
                .get(&Identifier::new("second"))
                .unwrap()
                .range_data(),
            vec![0xea]
        );
        Ok(())
    }

    #[test]
    fn cannot_use_unknown_macros() -> CoreResult<()> {
        let err = test_codegen("foo(1)").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:1: error: unknown identifier: foo"
        );
        Ok(())
    }

    #[test]
    fn can_invoke_uninvoked_macros() -> CoreResult<()> {
        let ctx = test_codegen_with_options(
            ".macro foo(val) {\n    nop\n.if val { nop }\n}",
            CodegenOptions {
                enable_greedy_analysis: true,
                ..Default::default()
            },
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![]);
        assert_eq!(
            ctx.source_map()
                .line_col_to_offsets(&ctx.tree.code_map, "test.asm", 1, None)
                .is_empty(),
            false
        );
        Ok(())
    }

    #[test]
    fn can_use_nested_macros() -> CoreResult<()> {
        let ctx = test_codegen(
            r"
            .macro a(value_a) {
                ldx #value_a
            }
            .macro b(value_b) {
                a(32)
                ldy #value_b
            }
            b(64)
            ",
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0xa2, 32, 0xa0, 64]);
        Ok(())
    }

    #[test]
    fn can_use_macros_across_files() -> CoreResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".import * from \"bar\"\nmy_macro(foo)\nfoo: nop",
            )
            .add("bar", ".macro my_macro(arg) {\n   lda arg\n}")
            .into();

        let ctx = test_codegen_parsing_source(src, CodegenOptions::default())?;
        assert_eq!(
            ctx.current_segment().range_data(),
            vec![0xad, 0x03, 0xc0, 0xea]
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
        let (_, err) = codegen(ast.clone(), CodegenOptions::default());
        assert_eq!(err.to_string(), "test.asm:141:1: error: branch too far");
        Ok(())
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
        let ctx = test_codegen(
            r#".define segment {
    name = "relocated"
    start = $c000
    pc = $8000
}

lda $d020
sta $d021"#,
        )?;
        let offset = ctx.source_map().address_to_offset(0x8003).unwrap();
        let sl = ctx.tree.code_map.look_up_span(offset.span);
        assert_eq!(offset.pc, 0x8003..0x8006);
        assert_eq!(sl.file.name(), "test.asm");
        assert_eq!(sl.begin.line, 7);
        assert_eq!(sl.begin.column, 0);
        assert_eq!(sl.end.line, 7);
        assert_eq!(sl.end.column, 9);

        let offsets = ctx
            .source_map
            .line_col_to_offsets(&ctx.tree.code_map, "test.asm", 7, 4);
        let offset = offsets[0];
        let sl = ctx.tree.code_map.look_up_span(offset.span);
        assert_eq!(offset.pc, 0x8003..0x8006);
        assert_eq!(sl.file.name(), "test.asm");
        assert_eq!(sl.begin.line, 7);
        assert_eq!(sl.begin.column, 0);
        assert_eq!(sl.end.line, 7);
        assert_eq!(sl.end.column, 9);

        Ok(())
    }

    #[test]
    fn can_compile_specified_test() -> CoreResult<()> {
        let src = ".test \"a\" { nop }\n.test \"b\" { asl }";
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
        let mut ctx = test_codegen_with_options(
            r#"
            .test "a" {
                nop
                .trace
                .assert foo
            }
            asl"#,
            CodegenOptions {
                active_test: None,
                ..Default::default()
            },
        )?;
        assert_eq!(ctx.current_segment().range_data(), vec![0x0a]);
        assert_eq!(ctx.remove_test_elements().len(), 0);

        Ok(())
    }

    pub fn test_codegen(code: &str) -> CoreResult<CodegenContext> {
        test_codegen_parsing_source(
            InMemoryParsingSource::new().add("test.asm", &code).into(),
            CodegenOptions::default(),
        )
    }

    pub fn test_codegen_with_options(
        code: &str,
        options: CodegenOptions,
    ) -> CoreResult<CodegenContext> {
        test_codegen_parsing_source(
            InMemoryParsingSource::new().add("test.asm", &code).into(),
            options,
        )
    }

    pub fn test_codegen_parsing_source(
        src: Arc<Mutex<dyn ParsingSource>>,
        options: CodegenOptions,
    ) -> CoreResult<CodegenContext> {
        let ast = parse_or_err(&Path::new("test.asm"), src)?;
        let (ctx, err) = codegen(ast, options);
        if err.is_empty() {
            Ok(ctx.unwrap())
        } else {
            Err(err)
        }
    }
}
