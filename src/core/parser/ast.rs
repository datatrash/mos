use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use itertools::Itertools;

use crate::core::parser::config_map::ConfigMap;
use crate::core::parser::mnemonic::Mnemonic;
use crate::core::parser::ParseError;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction<'a> {
    pub mnemonic: Mnemonic,
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
    pub addressing_mode: AddressingMode,
    pub suffix: Option<Box<Located<'a, Token<'a>>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberType {
    Hex,
    Dec,
    Bin,
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
    pub op: BinaryOp,
    pub lhs: Box<Located<'a, Expression<'a>>>,
    pub rhs: Box<Located<'a, Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionFactor<'a> {
    /// The full identifier path, which modifier it uses
    IdentifierValue(IdentifierPath<'a>, Option<AddressModifier>),
    Number(i64, NumberType),
    ExprParens(Box<Located<'a, Expression<'a>>>),
    CurrentProgramCounter,
    FunctionCall(
        Box<Located<'a, Token<'a>>>,
        Vec<Located<'a, Expression<'a>>>,
    ),
    Ws(
        Vec<Comment>,
        Box<Located<'a, ExpressionFactor<'a>>>,
        Vec<Comment>,
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
    Factor(Located<'a, ExpressionFactor<'a>>, ExpressionFactorFlags),
    BinaryExpression(BinaryExpression<'a>),
    Ws(Vec<Comment>, Box<Located<'a, Expression<'a>>>, Vec<Comment>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Variable,
    Constant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Braces(Vec<Located<'a, Token<'a>>>),
    Label(Identifier<'a>),
    Instruction(Instruction<'a>),
    IdentifierName(Identifier<'a>),
    VariableDefinition(Identifier<'a>, Located<'a, Expression<'a>>, VariableType),
    ProgramCounterDefinition(Located<'a, Expression<'a>>),
    Operand(Operand<'a>),
    RegisterSuffix(Register),
    Ws(Vec<Comment>, Box<Located<'a, Token<'a>>>, Vec<Comment>),
    Data(Vec<Located<'a, Expression<'a>>>, usize),
    Expression(Expression<'a>),
    Definition(
        Box<Located<'a, Token<'a>>>,
        Option<Box<Located<'a, Token<'a>>>>,
    ),
    Config(ConfigMap<'a>),
    ConfigPair(Box<Located<'a, Token<'a>>>, Box<Located<'a, Token<'a>>>),
    Segment(
        Box<Located<'a, Token<'a>>>,
        Option<Box<Located<'a, Token<'a>>>>,
    ),
    If(
        Located<'a, Expression<'a>>,
        Box<Located<'a, Token<'a>>>,
        Option<Box<Located<'a, Token<'a>>>>,
    ),
    Error,
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
            Token::Expression(Expression::Factor(expr, _)) => &expr.data,
            _ => panic!(),
        }
    }

    pub(crate) fn try_as_factor(&self) -> Option<&ExpressionFactor<'a>> {
        match self {
            Token::Expression(Expression::Factor(expr, _)) => Some(&expr.data),
            _ => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn as_config_map(&self) -> &ConfigMap<'a> {
        match self {
            Token::Config(cfg) => cfg,
            _ => panic!(),
        }
    }

    pub(crate) fn into_config_map(self) -> ConfigMap<'a> {
        match self {
            Token::Config(cfg) => cfg,
            _ => panic!(),
        }
    }

    pub(crate) fn into_braces(self) -> Vec<Located<'a, Token<'a>>> {
        match self {
            Token::Braces(inner) => inner,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Located<'a, T> {
    pub location: Location<'a>,
    pub data: T,
}

pub trait CanWrapWhitespace<'a> {
    fn strip_whitespace(self) -> Self;
    fn wrap_inner(lhs: Vec<Comment>, inner: Box<Located<'a, Self>>, rhs: Vec<Comment>) -> Self
    where
        Self: Sized;
}

impl<'a, T> Located<'a, T> {
    pub fn map<U, F: Fn(&T) -> U>(&self, map_fn: F) -> Located<'a, U> {
        Located::from(self.location.clone(), map_fn(&self.data))
    }

    pub fn from<L: Into<Location<'a>>>(location: L, data: T) -> Self {
        Self {
            location: location.into(),
            data,
        }
    }

    pub fn strip_whitespace(self) -> Self
    where
        T: CanWrapWhitespace<'a>,
    {
        Self {
            data: self.data.strip_whitespace(),
            ..self
        }
    }
}

fn sob<'a, T: CanWrapWhitespace<'a>>(
    token: Option<Box<Located<'a, T>>>,
) -> Option<Box<Located<'a, T>>> {
    token.map(|t| Box::new(t.strip_whitespace()))
}

#[allow(clippy::boxed_local)]
fn sb<'a, T: CanWrapWhitespace<'a>>(t: Box<Located<'a, T>>) -> Box<Located<'a, T>> {
    Box::new(t.strip_whitespace())
}

impl<'a> CanWrapWhitespace<'a> for Token<'a> {
    fn strip_whitespace(self) -> Self {
        match self {
            Token::Instruction(i) => Token::Instruction(Instruction {
                operand: sob(i.operand),
                ..i
            }),
            Token::Operand(o) => Token::Operand(Operand {
                expr: sb(o.expr),
                suffix: sob(o.suffix),
                ..o
            }),
            Token::Data(inner, size) => {
                let inner = inner.into_iter().map(|v| v.strip_whitespace()).collect();
                Token::Data(inner, size)
            }
            Token::Braces(inner) => {
                let inner = inner.into_iter().map(|v| v.strip_whitespace()).collect();
                Token::Braces(inner)
            }
            Token::If(expr, if_, else_) => Token::If(expr.strip_whitespace(), sb(if_), sob(else_)),
            Token::Ws(_, inner, _) => inner.data,
            _ => self,
        }
    }

    fn wrap_inner(lhs: Vec<Comment>, inner: Box<Located<'a, Self>>, rhs: Vec<Comment>) -> Self {
        Token::Ws(lhs, inner, rhs)
    }
}

impl<'a> CanWrapWhitespace<'a> for ExpressionFactor<'a> {
    fn strip_whitespace(self) -> Self {
        match self {
            ExpressionFactor::FunctionCall(name, args) => ExpressionFactor::FunctionCall(
                sb(name),
                args.into_iter().map(|a| a.strip_whitespace()).collect(),
            ),
            ExpressionFactor::ExprParens(inner) => ExpressionFactor::ExprParens(sb(inner)),
            ExpressionFactor::Ws(_, inner, _) => inner.data,
            _ => self,
        }
    }

    fn wrap_inner(lhs: Vec<Comment>, inner: Box<Located<'a, Self>>, rhs: Vec<Comment>) -> Self {
        ExpressionFactor::Ws(lhs, inner, rhs)
    }
}

impl<'a> CanWrapWhitespace<'a> for Expression<'a> {
    fn strip_whitespace(self) -> Self {
        match self {
            Expression::Factor(factor, flags) => {
                Expression::Factor(factor.strip_whitespace(), flags)
            }
            Expression::BinaryExpression(expr) => {
                let e = BinaryExpression {
                    op: expr.op,
                    lhs: sb(expr.lhs),
                    rhs: sb(expr.rhs),
                };
                Expression::BinaryExpression(e)
            }
            Expression::Ws(_, inner, _) => inner.data,
        }
    }

    fn wrap_inner(lhs: Vec<Comment>, inner: Box<Located<'a, Self>>, rhs: Vec<Comment>) -> Self {
        Expression::Ws(lhs, inner, rhs)
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
            Self::Number(val, ty) => match ty {
                NumberType::Hex => write!(f, "${:x}", val),
                NumberType::Bin => write!(f, "%{:b}", val),
                NumberType::Dec => write!(f, "{}", val),
            },
            Self::CurrentProgramCounter => write!(f, "*"),
            Self::FunctionCall(name, args) => {
                let args = args.iter().map(|arg| format!("{}", arg.data)).join(",");
                write!(f, "{}[{}]", name.data, args)
            }
            Self::ExprParens(inner) => {
                write!(f, "[{}]", inner.data)
            }
            Self::Ws(l, inner, r) => format_ws(f, l, inner, r),
        }
    }
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Factor(factor, flags) => {
                write!(f, "{}{}", flags, factor.data)
            }
            Self::BinaryExpression(expr) => {
                write!(f, "{} {} {}", expr.lhs.data, expr.op, expr.rhs.data)
            }
            Self::Ws(l, inner, r) => format_ws(f, l, inner, r),
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Braces(tokens) => {
                let tokens = tokens
                    .iter()
                    .map(|t| format!("{}", t.data))
                    .collect_vec()
                    .join(" ");
                write!(f, "{{ {} }}", tokens)
            }
            Token::Label(id) => {
                write!(f, "{}:", id.0)
            }
            Token::Instruction(i) => match &i.operand {
                Some(o) => {
                    write!(f, "{}{}", i.mnemonic, o.data)
                }
                None => write!(f, "{}", i.mnemonic),
            },
            Token::IdentifierName(id) => {
                write!(f, "{}", id.0)
            }
            Token::VariableDefinition(id, val, ty) => {
                let ty = match ty {
                    VariableType::Variable => ".VAR",
                    VariableType::Constant => ".CONST",
                };
                write!(f, "{} {} = {}", ty, id, val.data)
            }
            Token::ProgramCounterDefinition(val) => {
                write!(f, "* = {}", val.data)
            }
            Token::Expression(e) => write!(f, "{}", e),
            Token::Operand(o) => {
                let suffix = match &o.suffix {
                    Some(s) => format!("{}", s.data),
                    None => "".to_string(),
                };

                match &o.addressing_mode {
                    AddressingMode::Immediate => write!(f, " #{}", o.expr.data),
                    AddressingMode::Implied => write!(f, ""),
                    AddressingMode::AbsoluteOrZP => {
                        write!(f, " {}{}", o.expr.data, suffix)
                    }
                    AddressingMode::OuterIndirect => {
                        write!(f, " ({}){}", o.expr.data, suffix)
                    }
                    AddressingMode::Indirect => {
                        write!(f, " ({}{})", o.expr.data, suffix)
                    }
                }
            }
            Token::RegisterSuffix(reg) => match reg {
                Register::X => write!(f, ", x"),
                Register::Y => write!(f, ", y"),
            },
            Token::Ws(l, inner, r) => format_ws(f, l, inner, r),
            Token::Data(tok, sz) => {
                let label = match sz {
                    1 => ".byte",
                    2 => ".word",
                    4 => ".dword",
                    _ => panic!(),
                };
                let data = tok
                    .iter()
                    .map(|t| format!("{}", t.data))
                    .collect_vec()
                    .join(", ");
                write!(f, "{} {}", label, data)
            }
            Token::Definition(id, cfg) => {
                let cfg = cfg
                    .as_ref()
                    .map(|c| format!("{}", c.data))
                    .unwrap_or_else(|| "".to_string());
                write!(f, ".DEFINE {}{}", id.data, cfg)
            }
            Token::Segment(id, inner) => {
                let inner = match inner {
                    Some(i) => format!(" {}", i.data),
                    None => "".to_string(),
                };
                write!(f, ".SEGMENT {}{}", id.data, inner)
            }
            Token::Config(cfg) => {
                let items = cfg
                    .keys()
                    .iter()
                    .sorted()
                    .map(|key| format!("{}={}", key, cfg.value(key).data))
                    .collect_vec()
                    .join(",")
                    .trim_end()
                    .to_string();
                write!(f, "[{}]", items)
            }
            Token::ConfigPair(_k, _v) => unimplemented!(),
            Token::If(expr, if_, else_) => {
                let else_ = match else_ {
                    Some(e) => format!(" ELSE {}", e.data),
                    None => "".to_string(),
                };
                write!(f, ".IF [{}] {}{}", expr.data, if_.data, else_)
            }
            Token::Error => write!(f, "Error"),
        }
    }
}

fn format_ws<'a, T: CanWrapWhitespace<'a> + Display>(
    f: &mut Formatter,
    l: &[Comment],
    inner: &Located<'a, T>,
    r: &[Comment],
) -> std::fmt::Result {
    for w in l {
        let _ = write!(f, "{}", w);
    }
    let _ = write!(f, "{}", inner.data);
    for w in r {
        let _ = write!(f, "{}", w);
    }
    Ok(())
}
