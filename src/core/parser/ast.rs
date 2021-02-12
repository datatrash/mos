use std::cell::RefCell;
use std::fmt::{Binary, Display, Formatter, LowerHex};
use std::rc::Rc;

use itertools::Itertools;

use crate::core::parser::config_map::ConfigMap;
use crate::core::parser::mnemonic::Mnemonic;
use crate::core::parser::ParseError;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;
// An item in a comma-separated list
pub type ArgItem<'a> = (Located<'a, Expression<'a>>, Option<Located<'a, char>>);

#[derive(Clone, Debug)]
pub struct State<'a> {
    pub filename: &'a str,
    pub errors: Rc<RefCell<Vec<ParseError<'a>>>>,
}

impl<'a> State<'a> {
    pub fn new(filename: &'a str) -> Self {
        Self {
            filename,
            errors: Rc::new(RefCell::new(Vec::new())),
        }
    }
}

impl<'a> State<'a> {
    pub fn report_error(&self, error: ParseError<'a>) {
        self.errors.borrow_mut().push(error);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comment {
    Whitespace(String),
    NewLine,
    CStyle(String),
    CppStyle(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier<'a>(pub &'a str);

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

#[derive(Clone, Debug, PartialEq)]
pub struct Location<'a> {
    pub path: &'a str,
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OwnedLocation {
    pub path: String,
    pub line: u32,
    pub column: u32,
}

impl<'a> From<Location<'a>> for OwnedLocation {
    fn from(l: Location<'a>) -> Self {
        Self {
            path: String::from(l.path),
            line: l.line,
            column: l.column,
        }
    }
}

impl<'a> From<&'a Location<'a>> for OwnedLocation {
    fn from(l: &'a Location<'a>) -> Self {
        Self {
            path: String::from(l.path),
            line: l.line,
            column: l.column,
        }
    }
}

impl<'a> From<&LocatedSpan<'a>> for Location<'a> {
    fn from(span: &LocatedSpan<'a>) -> Self {
        Self {
            path: span.extra.filename,
            line: span.location_line(),
            column: span.get_column() as u32,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    X,
    Y,
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::X => write!(f, "X"),
            Self::Y => write!(f, "Y"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction<'a> {
    pub mnemonic: Located<'a, Mnemonic>,
    pub operand: Option<Box<Located<'a, Token<'a>>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AddressingMode {
    AbsoluteOrZP,
    Immediate,
    Implied,
    Indirect,
    OuterIndirect,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operand<'a> {
    pub expr: Box<Located<'a, Expression<'a>>>,
    pub left_paren: Option<Located<'a, char>>,
    pub right_paren: Option<Located<'a, char>>,
    pub addressing_mode: AddressingMode,
    pub suffix: Option<Box<Located<'a, Token<'a>>>>,
}

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

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierPath<'a>(Vec<Identifier<'a>>);

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

    pub fn single(&self) -> &Identifier {
        assert_eq!(self.len(), 1);
        self.0.first().unwrap()
    }
}

impl<'a> Display for IdentifierPath<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|i| i.0).collect_vec().join("."))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Shl,
    Shr,
    Xor,
    Eq,
    Ne,
    Gt,
    GtEq,
    Lt,
    LtEq,
    And,
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

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression<'a> {
    pub op: Located<'a, BinaryOp>,
    pub lhs: Box<Located<'a, Expression<'a>>>,
    pub rhs: Box<Located<'a, Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionFactor<'a> {
    /// The full identifier path, which modifier it uses
    IdentifierValue(
        Located<'a, IdentifierPath<'a>>,
        Option<Located<'a, AddressModifier>>,
    ),
    Number(Located<'a, NumberType>, Located<'a, i64>),
    ExprParens(
        Located<'a, char>,
        Box<Located<'a, Expression<'a>>>,
        Located<'a, char>,
    ),
    CurrentProgramCounter(Located<'a, char>),
    FunctionCall(
        Box<Located<'a, Token<'a>>>,
        Located<'a, char>,
        Vec<ArgItem<'a>>,
        Located<'a, char>,
    ),
}

bitflags::bitflags! {
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Factor(
        Located<'a, ExpressionFactor<'a>>,
        ExpressionFactorFlags,
        Option<Located<'a, char>>,
        Option<Located<'a, char>>,
    ),
    BinaryExpression(BinaryExpression<'a>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VariableType {
    Variable,
    Constant,
}

impl Display for VariableType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            VariableType::Variable => write!(f, ".var"),
            VariableType::Constant => write!(f, ".const"),
        }
    }
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Braces(
        Located<'a, char>,
        Located<'a, Vec<Located<'a, Token<'a>>>>,
        Located<'a, char>,
    ),
    Label(Located<'a, Identifier<'a>>, Option<Located<'a, char>>),
    Instruction(Instruction<'a>),
    IdentifierName(Identifier<'a>),
    VariableDefinition(
        Located<'a, VariableType>,
        Located<'a, Identifier<'a>>,
        Located<'a, char>,
        Box<Located<'a, Expression<'a>>>,
    ),
    ProgramCounterDefinition(
        Located<'a, char>,
        Located<'a, char>,
        Located<'a, Expression<'a>>,
    ),
    Operand(Operand<'a>),
    RegisterSuffix(Located<'a, char>, Located<'a, Register>),
    Data(Vec<ArgItem<'a>>, Located<'a, DataSize>),
    Expression(Expression<'a>),
    Definition(
        Located<'a, &'a str>,
        Box<Located<'a, Token<'a>>>,
        Option<Box<Located<'a, Token<'a>>>>,
    ),
    Config(
        Located<'a, char>,
        Located<'a, Vec<Located<'a, Token<'a>>>>,
        Located<'a, char>,
    ),
    ConfigPair(
        Box<Located<'a, Token<'a>>>,
        Located<'a, char>,
        Box<Located<'a, Token<'a>>>,
    ),
    Segment(
        Located<'a, &'a str>,
        Box<Located<'a, Token<'a>>>,
        Option<Box<Located<'a, Token<'a>>>>,
    ),
    If(
        Located<'a, &'a str>,
        Located<'a, Expression<'a>>,
        Box<Located<'a, Token<'a>>>,
        Option<Located<'a, &'a str>>,
        Option<Box<Located<'a, Token<'a>>>>,
    ),
    Align(Located<'a, &'a str>, Located<'a, Expression<'a>>),
    Error,
    EolTrivia(Located<'a, char>),
    Eof,
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
            Token::Expression(Expression::Factor(expr, _, _, _)) => &expr.data,
            _ => panic!(),
        }
    }

    pub(crate) fn try_as_factor(&self) -> Option<&ExpressionFactor<'a>> {
        match self {
            Token::Expression(Expression::Factor(expr, _, _, _)) => Some(&expr.data),
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
            Token::Config(_, kvp, _) => ConfigMap::new(kvp.data),
            _ => panic!(),
        }
    }

    pub(crate) fn into_braces(self) -> Vec<Located<'a, Token<'a>>> {
        match self {
            Token::Braces(_, inner, _) => inner.data,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Located<'a, T> {
    pub location: Location<'a>,
    pub data: T,
    pub trivia: Option<Trivia<'a>>,
}

impl<'a, T> Located<'a, Located<'a, T>> {
    pub fn flatten(self) -> Located<'a, T> {
        Located {
            location: self.data.location.clone(),
            data: self.data.data,
            trivia: self.trivia,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trivia<'a> {
    pub location: Location<'a>,
    pub comments: Vec<Comment>,
}

impl<'a> Display for Trivia<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let triv = self.comments.iter().map(|t| format!("{}", t)).join("");
        write!(f, "{}", triv)
    }
}

impl<'a, T> Located<'a, T> {
    pub fn map<U, F: Fn(&T) -> U>(&self, map_fn: F) -> Located<'a, U> {
        Located::from_trivia(
            self.location.clone(),
            map_fn(&self.data),
            self.trivia.clone(),
        )
    }

    pub fn map_into<U, F: FnOnce(T) -> U>(self, map_fn: F) -> Located<'a, U> {
        Located::from_trivia(self.location, map_fn(self.data), self.trivia)
    }

    pub fn from<L: Into<Location<'a>>>(location: L, data: T) -> Self {
        Self {
            location: location.into(),
            data,
            trivia: None,
        }
    }

    pub fn from_trivia<L: Into<Location<'a>>>(
        location: L,
        data: T,
        trivia: Option<Trivia<'a>>,
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

impl Display for Comment {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Comment::Whitespace(str) => write!(f, "{}", str),
            Comment::NewLine => writeln!(f),
            Comment::CStyle(str) => write!(f, "{}", str),
            Comment::CppStyle(str) => write!(f, "{}", str),
        }
    }
}

impl<'a> Display for ExpressionFactor<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::IdentifierValue(path, modifier) => {
                let modifier = match modifier {
                    Some(m) => m.to_string(),
                    None => "".to_string(),
                };
                write!(f, "{}{}", modifier, path)
            }
            Self::Number(ty, val) => match ty.data {
                NumberType::Hex => write!(f, "{}{:x}", ty, val),
                NumberType::Bin => write!(f, "{}{:b}", ty, val),
                NumberType::Dec => write!(f, "{}{}", ty, val),
            },
            Self::CurrentProgramCounter(star) => write!(f, "{}", star),
            Self::FunctionCall(name, lhs, args, rhs) => {
                write!(f, "{}{}{}{}", name, lhs, format_arglist(args), rhs)
            }
            Self::ExprParens(lbracket, inner, rbracket) => {
                write!(f, "{}{}{}", lbracket, inner, rbracket)
            }
        }
    }
}

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

fn format_trivia(trivia: &Option<Trivia>) -> String {
    trivia
        .as_ref()
        .map(|t| format!("{}", t))
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
            Self::Factor(factor, _flags, not, neg) => {
                let not = match not {
                    Some(n) => format!("{}", n),
                    None => "".to_string(),
                };
                let neg = match neg {
                    Some(n) => format!("{}", n),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", not, neg, factor)
            }
            Self::BinaryExpression(expr) => {
                write!(f, "{}{}{}", expr.lhs, expr.op, expr.rhs)
            }
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Braces(lhs, tokens, rhs) | Token::Config(lhs, tokens, rhs) => {
                let tokens = tokens
                    .data
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect_vec()
                    .join("");
                write!(f, "{}{}{}", lhs, tokens, rhs)
            }
            Token::Label(id, colon) => {
                let colon = match colon {
                    Some(c) => format!("{}", c),
                    None => "".to_string(),
                };
                write!(f, "{}{}", id, colon)
            }
            Token::Instruction(i) => match &i.operand {
                Some(o) => {
                    write!(f, "{}{}", i.mnemonic, o)
                }
                None => write!(f, "{}", i.mnemonic),
            },
            Token::IdentifierName(id) => {
                write!(f, "{}", id.0)
            }
            Token::VariableDefinition(ty, id, eq, val) => {
                write!(f, "{}{}{}{}", format!("{}", ty).to_uppercase(), id, eq, val)
            }
            Token::ProgramCounterDefinition(star, eq, val) => {
                write!(f, "{}{}{}", star, eq, val)
            }
            Token::Expression(e) => write!(f, "{}", e),
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
                        write!(f, "{}{}", o.left_paren.as_ref().unwrap(), o.expr)
                    }
                    AddressingMode::Implied => write!(f, ""),
                    AddressingMode::OuterIndirect => {
                        write!(
                            f,
                            "{}{}{}{}",
                            o.left_paren.as_ref().unwrap(),
                            o.expr,
                            o.right_paren.as_ref().unwrap(),
                            suffix
                        )
                    }
                    AddressingMode::Indirect => {
                        write!(
                            f,
                            "{}{}{}{}",
                            o.left_paren.as_ref().unwrap(),
                            o.expr,
                            suffix,
                            o.right_paren.as_ref().unwrap()
                        )
                    }
                }
            }
            Token::RegisterSuffix(comma, reg) => {
                write!(f, "{}{}", comma, format!("{}", reg).to_uppercase())
            }
            Token::Data(args, sz) => {
                write!(
                    f,
                    "{}{}",
                    format!("{}", sz).to_uppercase(),
                    format_arglist(args)
                )
            }
            Token::Definition(tag, id, cfg) => {
                let cfg = cfg
                    .as_ref()
                    .map(|c| format!("{}", c))
                    .unwrap_or_else(|| "".to_string());
                write!(f, "{}{}{}", format!("{}", tag).to_uppercase(), id, cfg)
            }
            Token::Segment(tag, id, inner) => {
                let inner = match inner {
                    Some(i) => format!("{}", i),
                    None => "".to_string(),
                };
                write!(f, "{}{}{}", format!("{}", tag).to_uppercase(), id, inner)
            }
            Token::ConfigPair(k, eq, v) => {
                write!(f, "{}{}{}", k, eq, v)
            }
            Token::If(tag_if, expr, if_, tag_else, else_) => {
                let else_ = match (tag_else, else_) {
                    (Some(tag), Some(e)) => format!("{}{}", format!("{}", tag).to_uppercase(), e),
                    _ => "".to_string(),
                };
                write!(
                    f,
                    "{}{}{}{}",
                    format!("{}", tag_if).to_uppercase(),
                    expr,
                    if_,
                    else_
                )
            }
            Token::Align(tag, expr) => {
                write!(f, "{}{}", format!("{}", tag).to_uppercase(), expr)
            }
            Token::Error => write!(f, "Error"),
            Token::EolTrivia(triv) => {
                write!(f, "{}", triv)
            }
            Token::Eof => Ok(()),
        }
    }
}
