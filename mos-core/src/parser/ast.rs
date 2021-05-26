use crate::errors::{CoreError, CoreResult};
use crate::parser::code_map::{CodeMap, File, Span};
use crate::parser::mnemonic::Mnemonic;
use crate::parser::source::ParsingSource;
use crate::parser::{Identifier, IdentifierPath};
use itertools::Itertools;
use path_dedot::ParseDot;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Binary, Debug, Display, Formatter, LowerHex};
use std::path::PathBuf;
use std::sync::{Arc, Mutex, MutexGuard};

/// A span containing a fragment of the source text and the location of this fragment within the source
pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, ParserInstance>;

/// A convenience wrapper around nom's own [nom::IResult] to make use of our own [LocatedSpan] type
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

/// An item in a comma-separated list
pub type ArgItem<T> = (Located<T>, Option<Located<char>>);

/// The result of parsing
pub struct ParseTree {
    pub code_map: CodeMap,
    pub main_file: PathBuf,
    pub files: HashMap<PathBuf, ParsedFile>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParsedFile {
    pub tokens: Arc<Vec<Token>>,
    pub file: Arc<File>,
}

impl Debug for ParseTree {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "[ParseTree]")
    }
}

impl ParseTree {
    pub fn new(code_map: CodeMap, main_file: PathBuf, files: HashMap<PathBuf, ParsedFile>) -> Self {
        Self {
            code_map,
            main_file,
            files,
        }
    }

    pub fn main_file(&self) -> &ParsedFile {
        self.try_get_file(&self.main_file).unwrap()
    }

    pub fn try_get_file<P: Into<PathBuf>>(&self, path: P) -> Option<&ParsedFile> {
        self.files.get(path.into().parse_dot().unwrap().as_ref())
    }
}

/// The state of a single parser
#[derive(Clone)]
pub struct ParserInstance {
    shared_state: Arc<Mutex<State>>,
    pub current_file: Arc<File>,
    pub to_import: Arc<RefCell<HashMap<PathBuf, Span>>>,
}

impl ParserInstance {
    pub fn new(state: Arc<Mutex<State>>, current_file: Arc<File>) -> Self {
        Self {
            shared_state: state,
            current_file,
            to_import: Arc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn shared_state(&self) -> MutexGuard<State> {
        self.shared_state.lock().unwrap()
    }
}

/// The shared state of all parsers
pub struct State {
    /// The parsing source
    pub source: Arc<Mutex<dyn ParsingSource>>,

    /// Codemap
    pub code_map: CodeMap,

    /// Which errors did we encounter?
    pub errors: Vec<CoreError>,

    /// Should the next error be ignored?
    ignore_next_error: bool,

    /// Current anonymous scope index
    anonymous_scope_index: usize,
}

impl State {
    pub fn new(source: Arc<Mutex<dyn ParsingSource>>) -> Self {
        Self {
            source,
            code_map: CodeMap::new(),
            errors: vec![],
            ignore_next_error: false,
            anonymous_scope_index: 0,
        }
    }

    pub fn add_file<P: Into<PathBuf>>(&mut self, path: P) -> CoreResult<Arc<File>> {
        let path = path.into();
        let src = self.source.lock().unwrap().get_contents(&path)?;
        Ok(self.code_map.add_file(path.to_str().unwrap().into(), src))
    }

    /// Mark the next reported error to be ignored (since it may be redundant or something)
    pub fn ignore_next_error(&mut self) {
        log::trace!("Ignoring next error");
        self.ignore_next_error = true;
    }

    /// When there is an error during parsing we don't want to fail. Instead, we continue but log the error via this method
    pub fn report_error<E: Into<CoreError>>(&mut self, error: E) {
        let error = error.into();
        if self.ignore_next_error {
            log::trace!("Ignoring error: {:?}", error);
            self.ignore_next_error = false;
        } else {
            log::trace!("Pushing error: {:?}", error);
            self.errors.push(error);
        }
    }

    pub fn new_anonymous_scope(&mut self) -> Identifier {
        self.anonymous_scope_index += 1;
        Identifier::anonymous(self.anonymous_scope_index)
    }
}

/// Any trivia we may encounter during parsing
#[derive(Clone, Debug, PartialEq)]
pub enum Trivia {
    /// One or more spaces or tabs
    Whitespace(String),
    /// A newline
    NewLine,
    /// A C-style comment: `/* foo */`
    CStyle(String),
    /// A C++-style comment: `// foo`
    CppStyle(String),
}

/// Registers used for indexing
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IndexRegister {
    X,
    Y,
}

impl Display for IndexRegister {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::X => write!(f, "X"),
            Self::Y => write!(f, "Y"),
        }
    }
}

/// A 6502 instruction
#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    pub mnemonic: Located<Mnemonic>,
    /// The operand is optional because some instructions (e.g. `NOP`) don't have an operand.
    pub operand: Option<Operand>,
}

/// The addressing mode for the instruction
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressingMode {
    /// Absolute or Zero-Page addressing (e.g. `LDA $34`)
    AbsoluteOrZp,
    /// Immediate addressing (e.g. `LDA #123`)
    Immediate,
    /// Implied (e.g. `NOP`)
    Implied,
    /// Indirect (e.g. `LDA ($12,X)`
    Indirect,
    /// Outer indirection (e.g. `LDA ($12),Y`)
    OuterIndirect,
}

/// The operand of an instruction
#[derive(Clone, Debug, PartialEq)]
pub struct Operand {
    pub expr: Located<Expression>,
    pub lchar: Option<Located<char>>,
    pub rchar: Option<Located<char>>,
    pub addressing_mode: AddressingMode,
    pub suffix: Option<RegisterSuffix>,
}

/// The optional register suffix of an operand
#[derive(Clone, Debug, PartialEq)]
pub struct RegisterSuffix {
    pub comma: Located<char>,
    pub register: Located<IndexRegister>,
}

/// A number, which can be hexadecimal (`$23AB`), decimal (`123`) or binary (`%11011`)
#[derive(Debug, Clone, PartialEq)]
pub enum NumberType {
    Hex,
    Dec,
    Bin,
}

impl Display for NumberType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            NumberType::Hex => write!(f, "$"),
            NumberType::Dec => Ok(()),
            NumberType::Bin => write!(f, "%"),
        }
    }
}

/// An address modifier which can be used to get the low (`<`) or high (`>`) byte of an address
#[derive(Debug, Clone, PartialEq)]
pub enum AddressModifier {
    HighByte,
    LowByte,
}

impl Display for AddressModifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::LowByte => write!(f, "<"),
            Self::HighByte => write!(f, ">"),
        }
    }
}

/// Any kind of binary operation that can be found in an expression
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Modulo
    Mod,
    /// Shift left
    Shl,
    /// Shift right
    Shr,
    /// Exclusive or
    Xor,
    /// Equals
    Eq,
    /// Not equals
    Ne,
    /// Greater than
    Gt,
    /// Greater than or equal
    GtEq,
    /// Less than
    Lt,
    /// Less than or equal
    LtEq,
    /// And
    And,
    /// Or
    Or,
}

impl BinaryOp {
    pub fn apply(&self, lhs: i64, rhs: i64) -> i64 {
        match self {
            BinaryOp::Add => lhs + rhs,
            BinaryOp::Sub => lhs - rhs,
            BinaryOp::Mul => lhs * rhs,
            BinaryOp::Div => match rhs {
                0 => 0,
                _ => lhs / rhs,
            },
            BinaryOp::Mod => match rhs {
                0 => 0,
                _ => lhs % rhs,
            },
            BinaryOp::Shl => lhs << rhs,
            BinaryOp::Shr => lhs >> rhs,
            BinaryOp::Xor => lhs ^ rhs,
            BinaryOp::Eq => (lhs == rhs) as i64,
            BinaryOp::Ne => (lhs != rhs) as i64,
            BinaryOp::Gt => (lhs > rhs) as i64,
            BinaryOp::GtEq => (lhs >= rhs) as i64,
            BinaryOp::Lt => (lhs < rhs) as i64,
            BinaryOp::LtEq => (lhs <= rhs) as i64,
            BinaryOp::And => (lhs != 0 && rhs != 0) as i64,
            BinaryOp::Or => (lhs != 0 || rhs != 0) as i64,
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match &self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
            BinaryOp::Xor => write!(f, "^"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Ne => write!(f, "!="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::GtEq => write!(f, ">="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::LtEq => write!(f, "<="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
        }
    }
}

/// A binary expression of the form `lhs op rhs`
#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub op: Located<BinaryOp>,
    pub lhs: Box<Located<Expression>>,
    pub rhs: Box<Located<Expression>>,
}

/// A factor that can be used on either side of an expression operation
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionFactor {
    CurrentProgramCounter(Located<char>),
    ExprParens {
        lparen: Located<char>,
        inner: Box<Located<Expression>>,
        rparen: Located<char>,
    },
    FunctionCall {
        name: Located<Identifier>,
        lparen: Located<char>,
        args: Vec<ArgItem<Expression>>,
        rparen: Located<char>,
    },
    IdentifierValue {
        path: Located<IdentifierPath>,
        modifier: Option<Located<AddressModifier>>,
    },
    Number {
        ty: Located<NumberType>,
        value: Located<Number>,
    },
}

/// A wrapper that stores the original number string and its radix, so that any zero-prefixes are kept
#[derive(Clone, Debug, PartialEq)]
pub struct Number {
    radix: u32,
    data: String,
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl Number {
    pub fn value(&self) -> i64 {
        i64::from_str_radix(&self.data, self.radix).ok().unwrap()
    }

    pub fn from_type<S: AsRef<str>>(ty: NumberType, data: S) -> Self {
        let radix = match ty {
            NumberType::Hex => 16,
            NumberType::Dec => 10,
            NumberType::Bin => 2,
        };
        Self {
            radix,
            data: data.as_ref().into(),
        }
    }
}

bitflags::bitflags! {
    /// Flags that can be used to modify the [ExpressionFactor] the flags belong to
    pub struct ExpressionFactorFlags: u8 {
        const NOT = 0b00000001;
        const NEG = 0b00000010;
    }
}

impl Display for ExpressionFactorFlags {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let not = if self.contains(ExpressionFactorFlags::NOT) {
            "!"
        } else {
            ""
        };
        let neg = if self.contains(ExpressionFactorFlags::NEG) {
            "-"
        } else {
            ""
        };
        write!(f, "{}{}", not, neg)
    }
}

/// An expression consists of one or more factors, possibly contained within a binary (sub)expression
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinaryExpression(BinaryExpression),
    Factor {
        factor: Box<Located<ExpressionFactor>>,
        flags: ExpressionFactorFlags,
        tag_not: Option<Located<char>>,
        tag_neg: Option<Located<char>>,
    },
}

impl Expression {
    /// Grab the trivia that comes before the first item in the token (e.g. stuff that could be to the right of the previous token)
    pub fn trivia(&self) -> Option<&Vec<Trivia>> {
        let t = match self {
            Expression::BinaryExpression(expr) => &expr.lhs.trivia,
            Expression::Factor { factor, .. } => &factor.trivia,
        };

        t.as_ref().map(|t| &t.data)
    }
}

/// User-defined variables
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VariableType {
    Constant,
    Variable,
}

impl Display for VariableType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            VariableType::Constant => write!(f, ".const"),
            VariableType::Variable => write!(f, ".var"),
        }
    }
}

/// The size of a data directive (e.g. `.byte 1, 2, 3`)
#[derive(Debug, Clone, PartialEq)]
pub enum DataSize {
    Byte,
    Word,
    Dword,
}

impl Display for DataSize {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, ".byte"),
            Self::Word => write!(f, ".word"),
            Self::Dword => write!(f, ".dword"),
        }
    }
}

/// A block of tokens
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub lparen: Located<char>,
    pub inner: Vec<Token>,
    pub rparen: Located<char>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportAs {
    pub tag: Located<String>,
    pub path: Located<IdentifierPath>,
}

impl Display for ImportAs {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.tag.map(|t| t.to_uppercase()), self.path)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TextEncoding {
    Ascii,
    Petscii,
    Petscreen,
    Unspecified,
}

impl Display for TextEncoding {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            TextEncoding::Ascii => write!(f, "ascii"),
            TextEncoding::Petscii => write!(f, "petscii"),
            TextEncoding::Petscreen => write!(f, "petscreen"),
            TextEncoding::Unspecified => write!(f, ""),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SpecificImportArg {
    pub path: Located<IdentifierPath>,
    pub as_: Option<ImportAs>,
}

impl Display for SpecificImportArg {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let as_ = match &self.as_ {
            Some(a) => {
                format!("{}", a)
            }
            None => "".to_string(),
        };
        write!(f, "{}{}", self.path, as_)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportArgs {
    All(Located<char>, Option<ImportAs>),
    Specific(Vec<ArgItem<SpecificImportArg>>),
}

/// Tokens that, together, make up all possible source text
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Align {
        tag: Located<String>,
        value: Located<Expression>,
    },
    Assert {
        tag: Located<String>,
        value: Located<Expression>,
        failure_message: Option<QuotedString>,
    },
    Braces {
        block: Block,
        scope: Identifier,
    },
    Config(Block),
    ConfigPair {
        key: Located<String>,
        eq: Located<char>,
        value: Box<Located<Token>>,
    },
    Data {
        values: Vec<ArgItem<Expression>>,
        size: Located<DataSize>,
    },
    Definition {
        tag: Located<String>,
        id: Located<Identifier>,
        value: Option<Box<Token>>,
    },
    Eof(Located<()>),
    Error(Located<String>),
    Expression(Expression),
    If {
        tag_if: Located<String>,
        value: Located<Expression>,
        if_: Block,
        tag_else: Option<Located<String>>,
        else_: Option<Block>,
    },
    Import {
        tag: Located<String>,
        args: ImportArgs,
        from: Located<String>,
        filename: QuotedString,
        block: Option<Block>,
        import_scope: Identifier,
        resolved_path: PathBuf,
    },
    File {
        tag: Located<String>,
        filename: QuotedString,
    },
    Instruction(Instruction),
    Label {
        id: Located<Identifier>,
        colon: Located<char>,
        block: Option<Block>,
    },
    Loop {
        tag: Located<String>,
        loop_scope: Box<Identifier>,
        expr: Located<Expression>,
        block: Block,
    },
    MacroDefinition {
        tag: Located<String>,
        id: Located<Identifier>,
        lparen: Located<char>,
        args: Vec<ArgItem<Identifier>>,
        rparen: Located<char>,
        block: Block,
    },
    MacroInvocation {
        id: Located<Identifier>,
        lparen: Located<char>,
        args: Vec<ArgItem<Expression>>,
        rparen: Located<char>,
    },
    ProgramCounterDefinition {
        star: Located<char>,
        eq: Located<char>,
        value: Located<Expression>,
    },
    Segment {
        tag: Located<String>,
        id: Located<Identifier>,
        block: Option<Block>,
    },
    Test {
        tag: Located<String>,
        id: Located<Identifier>,
        block: Block,
    },
    Text {
        tag: Located<String>,
        encoding: Option<Located<TextEncoding>>,
        text: QuotedString,
    },
    VariableDefinition {
        ty: Located<VariableType>,
        id: Located<Identifier>,
        eq: Located<char>,
        value: Located<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct QuotedString {
    pub lquote: Located<char>,
    pub text: Located<String>,
}

impl Display for QuotedString {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{}\"", self.lquote, self.text)
    }
}

impl Token {
    /// Grab the trivia that comes before the first item in the token (e.g. stuff that could be to the right of the previous token)
    pub fn trivia(&self) -> Option<&Vec<Trivia>> {
        let t = match self {
            Token::Align { tag, .. } => &tag.trivia,
            Token::Assert { tag, .. } => &tag.trivia,
            Token::Braces { block, .. } => &block.lparen.trivia,
            Token::Config(block) => &block.lparen.trivia,
            Token::ConfigPair { key, .. } => &key.trivia,
            Token::Data { size, .. } => &size.trivia,
            Token::Definition { tag, .. } => &tag.trivia,
            Token::Eof(empty) => &empty.trivia,
            Token::Error(invalid) => &invalid.trivia,
            Token::Expression(expr) => {
                return expr.trivia();
            }
            Token::If { tag_if, .. } => &tag_if.trivia,
            Token::Instruction(i) => &i.mnemonic.trivia,
            Token::Import { tag, .. } => &tag.trivia,
            Token::File { tag, .. } => &tag.trivia,
            Token::Label { id, .. } => &id.trivia,
            Token::Loop { tag, .. } => &tag.trivia,
            Token::MacroDefinition { tag, .. } => &tag.trivia,
            Token::MacroInvocation { id: name, .. } => &name.trivia,
            Token::ProgramCounterDefinition { star, .. } => &star.trivia,
            Token::Segment { tag, .. } => &tag.trivia,
            Token::Test { tag, .. } => &tag.trivia,
            Token::Text { tag, .. } => &tag.trivia,
            Token::VariableDefinition { ty, .. } => &ty.trivia,
        };

        t.as_ref().map(|t| &t.data)
    }
}

/// A wrapper around any kind of data which adds span information and any trivia.
///
/// During parsing the trivia that is included with tokens is any trivia that is located to the left of the token.
#[derive(Debug, Clone, PartialEq)]
pub struct Located<T> {
    pub span: Span,
    pub data: T,
    pub trivia: Option<Box<Located<Vec<Trivia>>>>,
}

impl<T> From<Located<T>> for Span {
    fn from(l: Located<T>) -> Self {
        l.span
    }
}

impl<T> From<&Located<T>> for Span {
    fn from(l: &Located<T>) -> Self {
        l.span
    }
}

impl<T> AsRef<Located<T>> for Located<T> {
    fn as_ref(&self) -> &Located<T> {
        &self
    }
}

impl<T> Located<Located<T>> {
    /// Remove one layer of nesting for nested located types
    pub fn flatten(self) -> Located<T> {
        Located {
            span: self.data.span,
            data: self.data.data,
            trivia: self.trivia,
        }
    }
}

impl<T> Located<T> {
    /// Borrow the [Located] and map the data to something else
    pub fn map<U, F: Fn(&T) -> U>(&self, map_fn: F) -> Located<U> {
        Located::new_with_trivia(self.span, map_fn(&self.data), self.trivia.clone())
    }

    /// Take ownership of the [Located] and map the data into something else
    pub fn map_into<U, F: FnOnce(T) -> U>(self, map_fn: F) -> Located<U> {
        Located::new_with_trivia(self.span, map_fn(self.data), self.trivia)
    }

    pub fn without_trivia(self) -> Located<T> {
        Located::new_with_trivia(self.span, self.data, None)
    }

    pub fn new(span: Span, data: T) -> Self {
        Self {
            span,
            data,
            trivia: None,
        }
    }

    pub fn new_with_trivia(span: Span, data: T, trivia: Option<Box<Located<Vec<Trivia>>>>) -> Self {
        Self { span, data, trivia }
    }
}

impl Display for Mnemonic {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let m = format!("{:?}", self).to_uppercase();
        write!(f, "{}", m)
    }
}

impl Display for Trivia {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Trivia::Whitespace(str) => write!(f, "{}", str),
            Trivia::NewLine => writeln!(f),
            Trivia::CStyle(str) => write!(f, "{}", str),
            Trivia::CppStyle(str) => write!(f, "{}", str),
        }
    }
}

impl Display for ExpressionFactor {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::CurrentProgramCounter(star) => write!(f, "{}", star),
            Self::ExprParens {
                lparen,
                inner,
                rparen,
            } => {
                write!(f, "{}{}{}", lparen, inner, rparen)
            }
            Self::FunctionCall {
                name,
                lparen,
                args,
                rparen,
            } => {
                write!(f, "{}{}{}{}", name, lparen, format_arglist(args), rparen)
            }
            Self::IdentifierValue { path, modifier } => {
                let modifier = match modifier {
                    Some(m) => m.to_string(),
                    None => "".to_string(),
                };
                write!(f, "{}{}", modifier, path)
            }
            Self::Number { ty, value } => write!(f, "{}{}", ty, value),
        }
    }
}

/// Formats a list of [ArgItem], using its contained trivia to add the separating commas, etc
fn format_arglist<T: Display>(args: &[ArgItem<T>]) -> String {
    args.iter()
        .map(|(arg, comma)| {
            let comma = match comma {
                Some(c) => format!("{}", c),
                None => "".to_string(),
            };
            format!("{}{}", arg, comma)
        })
        .join("")
}

/// Formats all the [Trivia] enclosed in a [Located]
pub fn format_trivia(trivia: &Option<Box<Located<Vec<Trivia>>>>) -> String {
    trivia
        .as_ref()
        .map(|t| t.data.iter().map(|c| format!("{}", c)).join(""))
        .unwrap_or_else(|| "".to_string())
}

impl<'a, T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{}", format_trivia(&self.trivia), &self.data)
    }
}

impl<'a, T: Display + LowerHex> LowerHex for Located<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{:x}", format_trivia(&self.trivia), &self.data)
    }
}

impl<'a, T: Display + Binary> Binary for Located<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{:b}", format_trivia(&self.trivia), &self.data)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::BinaryExpression(expr) => {
                write!(f, "{}{}{}", expr.lhs, expr.op, expr.rhs)
            }
            Self::Factor {
                factor,
                tag_not,
                tag_neg,
                ..
            } => {
                let not = match tag_not {
                    Some(n) => format!("{}", n),
                    None => "".to_string(),
                };
                let neg = match tag_neg {
                    Some(n) => format!("{}", n),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", not, neg, factor)
            }
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let inner = self
            .inner
            .iter()
            .map(|t| format!("{}", t))
            .collect_vec()
            .join("");
        write!(f, "{}{}{}", self.lparen, inner, self.rparen)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Align { tag, value } => {
                write!(f, "{}{}", format!("{}", tag).to_uppercase(), value)
            }
            Token::Assert {
                tag,
                value,
                failure_message,
            } => {
                let failure_message = failure_message
                    .as_ref()
                    .map(|c| format!("{}", c))
                    .unwrap_or_else(|| "".to_string());
                write!(
                    f,
                    "{}{}{}",
                    format!("{}", tag).to_uppercase(),
                    value,
                    failure_message
                )
            }
            Token::Braces { block, .. } | Token::Config(block) => {
                write!(f, "{}", block)
            }
            Token::ConfigPair { key, eq, value } => {
                write!(f, "{}{}{}", key, eq, value)
            }
            Token::Data { values, size } => {
                write!(
                    f,
                    "{}{}",
                    format!("{}", size).to_uppercase(),
                    format_arglist(values)
                )
            }
            Token::Definition { tag, id, value } => {
                let value = value
                    .as_ref()
                    .map(|c| format!("{}", c))
                    .unwrap_or_else(|| "".to_string());
                write!(f, "{}{}{}", format!("{}", tag).to_uppercase(), id, value)
            }
            Token::Eof(triv) => {
                write!(f, "{}", format_trivia(&triv.trivia))
            }
            Token::Error(str) => {
                write!(f, "{}", str)
            }
            Token::Expression(e) => write!(f, "{}", e),
            Token::File { tag, filename } => {
                write!(f, "{}{}", tag.map(|t| t.to_uppercase()), filename,)
            }
            Token::If {
                tag_if,
                value,
                if_,
                tag_else,
                else_,
            } => {
                let else_ = match (tag_else, else_) {
                    (Some(tag), Some(e)) => format!("{}{}", format!("{}", tag).to_uppercase(), e),
                    _ => "".to_string(),
                };
                write!(
                    f,
                    "{}{}{}{}",
                    format!("{}", tag_if).to_uppercase(),
                    value,
                    if_,
                    else_
                )
            }
            Token::Import {
                tag,
                args,
                from,
                filename,
                block,
                ..
            } => {
                let block = match block {
                    Some(block) => format!("{}", block),
                    None => "".to_string(),
                };
                let args = match args {
                    ImportArgs::All(c, as_) => {
                        let as_ = match as_ {
                            Some(a) => format!("{}", a),
                            None => "".to_string(),
                        };
                        format!("{}{}", c, as_)
                    }
                    ImportArgs::Specific(args) => format_arglist(args),
                };
                write!(
                    f,
                    "{}{}{}{}{}",
                    tag.map(|t| t.to_uppercase()),
                    args,
                    from.map(|t| t.to_uppercase()),
                    filename,
                    block
                )
            }
            Token::Instruction(i) => match &i.operand {
                Some(o) => {
                    let suffix = match &o.suffix {
                        Some(s) => format!("{}{}", s.comma, s.register.to_string().to_uppercase()),
                        None => "".to_string(),
                    };

                    let operand = match &o.addressing_mode {
                        AddressingMode::AbsoluteOrZp => {
                            format!("{}{}", o.expr, suffix)
                        }
                        AddressingMode::Immediate => {
                            format!("{}{}", o.lchar.as_ref().unwrap(), o.expr)
                        }
                        AddressingMode::Implied => "".to_string(),
                        AddressingMode::OuterIndirect => {
                            format!(
                                "{}{}{}{}",
                                o.lchar.as_ref().unwrap(),
                                o.expr,
                                o.rchar.as_ref().unwrap(),
                                suffix
                            )
                        }
                        AddressingMode::Indirect => {
                            format!(
                                "{}{}{}{}",
                                o.lchar.as_ref().unwrap(),
                                o.expr,
                                suffix,
                                o.rchar.as_ref().unwrap()
                            )
                        }
                    };

                    write!(f, "{}{}", i.mnemonic, operand)
                }
                None => write!(f, "{}", i.mnemonic),
            },
            Token::Label { id, colon, block } => {
                let block = match block {
                    Some(s) => format!("{}", s),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", id, colon, block)
            }
            Token::Loop {
                tag,
                loop_scope: _,
                expr,
                block,
            } => {
                write!(f, "{}{}{}", format!("{}", tag).to_uppercase(), expr, block)
            }
            Token::MacroDefinition {
                tag,
                id,
                lparen,
                args,
                rparen,
                block,
            } => {
                write!(
                    f,
                    "{}{}{}{}{}{}",
                    format!("{}", tag).to_uppercase(),
                    id,
                    lparen,
                    format_arglist(args),
                    rparen,
                    block
                )
            }
            Token::MacroInvocation {
                id: name,
                lparen,
                args,
                rparen,
            } => {
                write!(f, "{}{}{}{}", name, lparen, format_arglist(args), rparen)
            }
            Token::ProgramCounterDefinition { star, eq, value } => {
                write!(f, "{}{}{}", star, eq, value)
            }
            Token::Segment { tag, id, block } => {
                let block = match block {
                    Some(i) => format!("{}", i),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", format!("{}", tag).to_uppercase(), id, block)
            }
            Token::Test { tag, id, block } => {
                write!(f, "{}{}{}", tag.map(|t| t.to_uppercase()), id, block)
            }
            Token::Text {
                tag,
                encoding,
                text,
            } => {
                write!(
                    f,
                    "{}{}{}",
                    tag.map(|t| t.to_uppercase()),
                    encoding
                        .as_ref()
                        .map(|t| format!("{}", t).to_uppercase())
                        .unwrap_or_default(),
                    text,
                )
            }
            Token::VariableDefinition { ty, id, eq, value } => {
                write!(
                    f,
                    "{}{}{}{}",
                    format!("{}", ty).to_uppercase(),
                    id,
                    eq,
                    value
                )
            }
        }
    }
}
