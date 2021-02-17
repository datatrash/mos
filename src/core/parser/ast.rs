use std::cell::RefCell;
use std::fmt::{Binary, Display, Formatter, LowerHex};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use itertools::Itertools;

use crate::core::parser::config_map::ConfigMap;
use crate::core::parser::mnemonic::Mnemonic;
use crate::core::parser::ParseError;

/// A span containing a fragment of the source text and the location of this fragment within the source
pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;

/// A convenience wrapper around nom's own [nom::IResult] to make use of our own [LocatedSpan] type
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

/// An item in a comma-separated list
pub type ArgItem<'a> = (Located<'a, Expression<'a>>, Option<Located<'a, char>>);

/// The state of the parser
#[derive(Clone, Debug)]
pub struct State<'a> {
    /// Which file are we parsing?
    pub path: &'a Path,

    /// Which errors did we encounter?
    pub errors: Rc<RefCell<Vec<ParseError<'a>>>>,
}

impl<'a> State<'a> {
    pub fn new(path: &'a Path) -> Self {
        Self {
            path,
            errors: Rc::new(RefCell::new(Vec::new())),
        }
    }
}

impl<'a> State<'a> {
    /// When there is an error during parsing we don't want to fail. Instead, we continue but log the error via this method
    pub fn report_error(&self, error: ParseError<'a>) {
        self.errors.borrow_mut().push(error);
    }
}

/// Any trivia we may encounter during parsing
#[derive(Debug, Clone, PartialEq)]
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

/// A Rust-style identifier that can be used to, well, identify things
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier<'a>(pub &'a str);

/// Same as [Identifier] but with an owned [String] instead.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OwnedIdentifier(pub String);

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for OwnedIdentifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> From<Identifier<'a>> for OwnedIdentifier {
    fn from(id: Identifier<'a>) -> Self {
        Self(id.0.into())
    }
}

impl<'a> From<&Identifier<'a>> for OwnedIdentifier {
    fn from(id: &Identifier<'a>) -> Self {
        Self(id.0.into())
    }
}

impl<'a> From<Identifier<'a>> for &'a str {
    fn from(id: Identifier<'a>) -> Self {
        id.0
    }
}

impl<'a> From<&Identifier<'a>> for &'a str {
    fn from(id: &Identifier<'a>) -> Self {
        id.0
    }
}

/// A location inside of a source file
#[derive(Clone, Debug, PartialEq)]
pub struct Location<'a> {
    pub path: &'a Path,
    pub line: u32,
    pub column: u32,
}

impl<'a> Location<'a> {
    #[cfg(test)]
    pub fn empty() -> Self {
        Self {
            path: "test.asm".as_ref(),
            line: 0,
            column: 0,
        }
    }
}

/// A version of [Location] that owns its data
#[derive(Clone, Debug, PartialEq)]
pub struct OwnedLocation {
    pub path: PathBuf,
    pub line: u32,
    pub column: u32,
}

impl<'a> From<Location<'a>> for OwnedLocation {
    fn from(l: Location<'a>) -> Self {
        Self {
            path: l.path.to_path_buf(),
            line: l.line,
            column: l.column,
        }
    }
}

impl<'a> From<&'a Location<'a>> for OwnedLocation {
    fn from(l: &'a Location<'a>) -> Self {
        Self {
            path: l.path.to_path_buf(),
            line: l.line,
            column: l.column,
        }
    }
}

impl<'a> From<&LocatedSpan<'a>> for Location<'a> {
    fn from(span: &LocatedSpan<'a>) -> Self {
        Self {
            path: span.extra.path,
            line: span.location_line(),
            column: span.get_column() as u32,
        }
    }
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
#[derive(Debug, Clone, PartialEq)]
pub struct Instruction<'a> {
    pub mnemonic: Located<'a, Mnemonic>,
    /// The operand is optional because some instructions (e.g. `NOP`) don't have an operand.
    pub operand: Option<Box<Located<'a, Token<'a>>>>,
}

/// The addressing mode for the instruction
#[derive(Debug, Clone, PartialEq)]
pub enum AddressingMode {
    /// Absolute or Zero-Page addressing (e.g. `LDA $34`)
    AbsoluteOrZP,
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
#[derive(Debug, Clone, PartialEq)]
pub struct Operand<'a> {
    pub expr: Box<Located<'a, Expression<'a>>>,
    pub lchar: Option<Located<'a, char>>,
    pub rchar: Option<Located<'a, char>>,
    pub addressing_mode: AddressingMode,
    pub suffix: Option<Box<Located<'a, Token<'a>>>>,
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

/// A path of multiple identifiers, usually written as being separated by dots (e.g. `foo.bar.baz`)
#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierPath<'a>(Vec<Identifier<'a>>);

/// Same as [IdentifierPath] but containing [OwnedIdentifier]s.
#[derive(Clone, Debug, PartialEq)]
pub struct OwnedIdentifierPath(Vec<OwnedIdentifier>);

impl<'a> Display for IdentifierPath<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|i| i.0).collect_vec().join("."))
    }
}

impl Display for OwnedIdentifierPath {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|i| i.0.as_str()).collect_vec().join(".")
        )
    }
}

impl<'a> From<IdentifierPath<'a>> for OwnedIdentifierPath {
    fn from(src: IdentifierPath<'a>) -> Self {
        Self(
            src.0
                .into_iter()
                .map(|id| OwnedIdentifier(id.0.into()))
                .collect(),
        )
    }
}

impl<'a> IdentifierPath<'a> {
    pub fn new(ids: &[Identifier<'a>]) -> Self {
        Self(ids.to_vec())
    }

    pub fn to_str_vec(&self) -> Vec<&str> {
        self.0.iter().map(|i| i.0).collect()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn single(&self) -> &Identifier {
        assert_eq!(self.len(), 1);
        self.0.first().unwrap()
    }
}

impl OwnedIdentifierPath {
    pub fn new(ids: &[OwnedIdentifier]) -> Self {
        Self(ids.to_vec())
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match &self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
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
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression<'a> {
    pub op: Located<'a, BinaryOp>,
    pub lhs: Box<Located<'a, Expression<'a>>>,
    pub rhs: Box<Located<'a, Expression<'a>>>,
}

/// A factor that can be used on either side of an expression operation
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionFactor<'a> {
    CurrentProgramCounter(Located<'a, char>),
    ExprParens {
        lparen: Located<'a, char>,
        inner: Box<Located<'a, Expression<'a>>>,
        rparen: Located<'a, char>,
    },
    FunctionCall {
        name: Box<Located<'a, Token<'a>>>,
        lparen: Located<'a, char>,
        args: Vec<ArgItem<'a>>,
        rparen: Located<'a, char>,
    },
    IdentifierValue {
        path: Located<'a, IdentifierPath<'a>>,
        modifier: Option<Located<'a, AddressModifier>>,
    },
    Number {
        ty: Located<'a, NumberType>,
        value: Located<'a, i64>,
    },
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
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    BinaryExpression(BinaryExpression<'a>),
    Factor {
        factor: Box<Located<'a, ExpressionFactor<'a>>>,
        flags: ExpressionFactorFlags,
        tag_not: Option<Located<'a, char>>,
        tag_neg: Option<Located<'a, char>>,
    },
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

impl DataSize {
    pub fn byte_len(&self) -> usize {
        match self {
            DataSize::Byte => 1,
            DataSize::Word => 2,
            DataSize::Dword => 4,
        }
    }
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

/// An empty struct that is just there to provide an empty [std::fmt::Display] implementation
#[derive(Debug, Clone, PartialEq)]
pub struct EmptyDisplay;

impl Display for EmptyDisplay {
    fn fmt(&self, _: &mut Formatter) -> std::fmt::Result {
        Ok(())
    }
}

/// Tokens that, together, make up all possible source text
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Align {
        tag: Located<'a, &'a str>,
        value: Located<'a, Expression<'a>>,
    },
    Braces {
        lparen: Located<'a, char>,
        inner: Located<'a, Vec<Located<'a, Token<'a>>>>,
        rparen: Located<'a, char>,
    },
    Config {
        lparen: Located<'a, char>,
        inner: Located<'a, Vec<Located<'a, Token<'a>>>>,
        rparen: Located<'a, char>,
    },
    ConfigPair {
        key: Box<Located<'a, Token<'a>>>,
        eq: Located<'a, char>,
        value: Box<Located<'a, Token<'a>>>,
    },
    Data {
        values: Vec<ArgItem<'a>>,
        size: Located<'a, DataSize>,
    },
    Definition {
        tag: Located<'a, &'a str>,
        id: Box<Located<'a, Token<'a>>>,
        value: Option<Box<Located<'a, Token<'a>>>>,
    },
    /// Since during parsing the trivia that is attached to the tokens is the trivia to the left side of the token. If there is any trivia
    /// to the right-hand side, until the end of the line, then this token is additionally emitted just to store this additional trivia.
    EolTrivia(Located<'a, EmptyDisplay>),
    Eof,
    Error,
    Expression(Expression<'a>),
    IdentifierName(Identifier<'a>),
    If {
        tag_if: Located<'a, &'a str>,
        value: Located<'a, Expression<'a>>,
        if_: Box<Located<'a, Token<'a>>>,
        tag_else: Option<Located<'a, &'a str>>,
        else_: Option<Box<Located<'a, Token<'a>>>>,
    },
    Include {
        tag: Located<'a, &'a str>,
        lquote: Located<'a, char>,
        filename: Located<'a, &'a str>,
    },
    Instruction(Instruction<'a>),
    Label {
        id: Located<'a, Identifier<'a>>,
        colon: Option<Located<'a, char>>,
        braces: Option<Box<Located<'a, Token<'a>>>>,
    },
    Operand(Operand<'a>),
    ProgramCounterDefinition {
        star: Located<'a, char>,
        eq: Located<'a, char>,
        value: Located<'a, Expression<'a>>,
    },
    RegisterSuffix {
        comma: Located<'a, char>,
        register: Located<'a, IndexRegister>,
    },
    Segment {
        tag: Located<'a, &'a str>,
        id: Box<Located<'a, Token<'a>>>,
        inner: Option<Box<Located<'a, Token<'a>>>>,
    },
    VariableDefinition {
        ty: Located<'a, VariableType>,
        id: Located<'a, Identifier<'a>>,
        eq: Located<'a, char>,
        value: Box<Located<'a, Expression<'a>>>,
    },
}

impl<'a> Token<'a> {
    pub(crate) fn as_identifier(&self) -> &Identifier<'a> {
        match self {
            Token::IdentifierName(id) => id,
            _ => panic!(),
        }
    }

    pub(crate) fn as_expression(&self) -> &Expression<'a> {
        match self {
            Token::Expression(expr) => &expr,
            _ => panic!(),
        }
    }

    pub(crate) fn as_factor(&self) -> &ExpressionFactor<'a> {
        match self {
            Token::Expression(Expression::Factor { factor, .. }) => &factor.data,
            _ => panic!(),
        }
    }

    pub(crate) fn try_as_factor(&self) -> Option<&ExpressionFactor<'a>> {
        match self {
            Token::Expression(Expression::Factor { factor, .. }) => Some(&factor.data),
            _ => None,
        }
    }

    pub(crate) fn into_identifier(self) -> Identifier<'a> {
        match self {
            Token::IdentifierName(id) => id,
            _ => panic!(),
        }
    }

    pub(crate) fn into_config_map(self) -> ConfigMap<'a> {
        match self {
            Token::Config { inner, .. } => ConfigMap::new(inner.data),
            _ => panic!(),
        }
    }

    pub(crate) fn into_braces(self) -> Vec<Located<'a, Token<'a>>> {
        match self {
            Token::Braces { inner, .. } => inner.data,
            _ => panic!(),
        }
    }
}

/// A wrapper around any kind of data which adds location information and any trivia.
///
/// During parsing the trivia that is included with tokens is any trivia that is located to the left of the token.
#[derive(Debug, Clone, PartialEq)]
pub struct Located<'a, T> {
    pub location: Location<'a>,
    pub data: T,
    pub trivia: Option<Box<Located<'a, Vec<Trivia>>>>,
}

impl<'a, T> Located<'a, Located<'a, T>> {
    /// Remove one layer of nesting for nested located types
    pub fn flatten(self) -> Located<'a, T> {
        Located {
            location: self.data.location.clone(),
            data: self.data.data,
            trivia: self.trivia,
        }
    }
}

impl<'a, T> Located<'a, T> {
    /// Borrow the [Located] and map the data to something else
    pub fn map<U, F: Fn(&T) -> U>(&self, map_fn: F) -> Located<'a, U> {
        Located::new_with_trivia(
            self.location.clone(),
            map_fn(&self.data),
            self.trivia.clone(),
        )
    }

    /// Borrow the [Located] and map the data to something else using an FnOnce
    pub fn map_once<U, F: FnOnce(&T) -> U>(&self, map_fn: F) -> Located<'a, U> {
        Located::new_with_trivia(
            self.location.clone(),
            map_fn(&self.data),
            self.trivia.clone(),
        )
    }

    /// Take ownership of the [Located] and map the data into something else
    pub fn map_into<U, F: FnOnce(T) -> U>(self, map_fn: F) -> Located<'a, U> {
        Located::new_with_trivia(self.location, map_fn(self.data), self.trivia)
    }

    pub fn new<L: Into<Location<'a>>>(location: L, data: T) -> Self {
        Self {
            location: location.into(),
            data,
            trivia: None,
        }
    }

    pub fn new_with_trivia<L: Into<Location<'a>>>(
        location: L,
        data: T,
        trivia: Option<Box<Located<'a, Vec<Trivia>>>>,
    ) -> Self {
        Self {
            location: location.into(),
            data,
            trivia,
        }
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

impl<'a> Display for ExpressionFactor<'a> {
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
            Self::Number { ty, value } => match ty.data {
                NumberType::Hex => write!(f, "{}{:x}", ty, value),
                NumberType::Bin => write!(f, "{}{:b}", ty, value),
                NumberType::Dec => write!(f, "{}{}", ty, value),
            },
        }
    }
}

/// Formats a list of [ArgItem], using its contained trivia to add the separating commas, etc
fn format_arglist(args: &[ArgItem]) -> String {
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
fn format_trivia(trivia: &Option<Box<Located<Vec<Trivia>>>>) -> String {
    trivia
        .as_ref()
        .map(|t| t.data.iter().map(|c| format!("{}", c)).join(""))
        .unwrap_or_else(|| "".to_string())
}

impl<'a, T: Display> Display for Located<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{}", format_trivia(&self.trivia), &self.data)
    }
}

impl<'a, T: Display + LowerHex> LowerHex for Located<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{:x}", format_trivia(&self.trivia), &self.data)
    }
}

impl<'a, T: Display + Binary> Binary for Located<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{:b}", format_trivia(&self.trivia), &self.data)
    }
}

impl<'a> Display for Expression<'a> {
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

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Align { tag, value } => {
                write!(f, "{}{}", format!("{}", tag).to_uppercase(), value)
            }
            Token::Braces {
                lparen: left_paren,
                inner,
                rparen: right_paren,
            }
            | Token::Config {
                lparen: left_paren,
                inner,
                rparen: right_paren,
            } => {
                let inner = inner
                    .data
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect_vec()
                    .join("");
                write!(f, "{}{}{}", left_paren, inner, right_paren)
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
            Token::Eof => Ok(()),
            Token::EolTrivia(triv) => {
                writeln!(f, "{}", triv)
            }
            Token::Error => write!(f, "Error"),
            Token::Expression(e) => write!(f, "{}", e),
            Token::IdentifierName(id) => {
                write!(f, "{}", id.0)
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
            Token::Include {
                tag,
                lquote,
                filename,
            } => {
                write!(
                    f,
                    "{}{}{}\"",
                    tag.map(|t| t.to_uppercase()),
                    lquote,
                    filename,
                )
            }
            Token::Instruction(i) => match &i.operand {
                Some(o) => {
                    write!(f, "{}{}", i.mnemonic, o)
                }
                None => write!(f, "{}", i.mnemonic),
            },
            Token::Label { id, colon, braces } => {
                let braces = match braces {
                    Some(s) => format!("{}", s),
                    None => "".to_string(),
                };
                let colon = match colon {
                    Some(c) => format!("{}", c),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", id, colon, braces)
            }
            Token::Operand(o) => {
                let suffix = match &o.suffix {
                    Some(s) => format!("{}", s),
                    None => "".to_string(),
                };

                match &o.addressing_mode {
                    AddressingMode::AbsoluteOrZP => {
                        write!(f, "{}{}", o.expr, suffix)
                    }
                    AddressingMode::Immediate => {
                        write!(f, "{}{}", o.lchar.as_ref().unwrap(), o.expr)
                    }
                    AddressingMode::Implied => write!(f, ""),
                    AddressingMode::OuterIndirect => {
                        write!(
                            f,
                            "{}{}{}{}",
                            o.lchar.as_ref().unwrap(),
                            o.expr,
                            o.rchar.as_ref().unwrap(),
                            suffix
                        )
                    }
                    AddressingMode::Indirect => {
                        write!(
                            f,
                            "{}{}{}{}",
                            o.lchar.as_ref().unwrap(),
                            o.expr,
                            suffix,
                            o.rchar.as_ref().unwrap()
                        )
                    }
                }
            }
            Token::ProgramCounterDefinition { star, eq, value } => {
                write!(f, "{}{}{}", star, eq, value)
            }
            Token::RegisterSuffix { comma, register } => {
                write!(f, "{}{}", comma, format!("{}", register).to_uppercase())
            }
            Token::Segment { tag, id, inner } => {
                let inner = match inner {
                    Some(i) => format!("{}", i),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", format!("{}", tag).to_uppercase(), id, inner)
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
