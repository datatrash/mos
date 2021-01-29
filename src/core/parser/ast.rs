use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

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
}

impl<'a> Display for IdentifierPath<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|i| i.0).collect::<Vec<_>>().join(".")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    /// The full identifier path, which modifier it uses
    IdentifierValue(IdentifierPath<'a>, Option<AddressModifier>),
    Number(usize, NumberType),
    ExprParens(Box<Located<'a, Expression<'a>>>),
    CurrentProgramCounter,
    BinaryAdd(
        Box<Located<'a, Expression<'a>>>,
        Box<Located<'a, Expression<'a>>>,
    ),
    BinarySub(
        Box<Located<'a, Expression<'a>>>,
        Box<Located<'a, Expression<'a>>>,
    ),
    BinaryMul(
        Box<Located<'a, Expression<'a>>>,
        Box<Located<'a, Expression<'a>>>,
    ),
    BinaryDiv(
        Box<Located<'a, Expression<'a>>>,
        Box<Located<'a, Expression<'a>>>,
    ),
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
    Error,
}

impl<'a> Token<'a> {
    pub(crate) fn as_identifier(&self) -> &Identifier<'a> {
        match self {
            Token::IdentifierName(id) => id,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Located<'a, T: CanWrapWhitespace<'a>> {
    pub location: Location<'a>,
    pub data: T,
}

pub trait CanWrapWhitespace<'a> {
    fn strip_whitespace(self) -> Self;
    fn wrap_inner(lhs: Vec<Comment>, inner: Box<Located<'a, Self>>, rhs: Vec<Comment>) -> Self
    where
        Self: Sized;
}

impl<'a, T: CanWrapWhitespace<'a>> Located<'a, T> {
    pub fn from<L: Into<Location<'a>>>(location: L, data: T) -> Self {
        Self {
            location: location.into(),
            data,
        }
    }

    pub fn strip_whitespace(self) -> Self {
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
            Token::Ws(_, inner, _) => inner.data,
            _ => self,
        }
    }

    fn wrap_inner(lhs: Vec<Comment>, inner: Box<Located<'a, Self>>, rhs: Vec<Comment>) -> Self {
        Token::Ws(lhs, inner, rhs)
    }
}

impl<'a> CanWrapWhitespace<'a> for Expression<'a> {
    fn strip_whitespace(self) -> Self {
        match self {
            Expression::BinaryAdd(lhs, rhs) => Expression::BinaryAdd(sb(lhs), sb(rhs)),
            Expression::BinarySub(lhs, rhs) => Expression::BinarySub(sb(lhs), sb(rhs)),
            Expression::BinaryMul(lhs, rhs) => Expression::BinaryMul(sb(lhs), sb(rhs)),
            Expression::BinaryDiv(lhs, rhs) => Expression::BinaryDiv(sb(lhs), sb(rhs)),
            Expression::ExprParens(inner) => Expression::ExprParens(sb(inner)),
            Expression::Ws(_, inner, _) => inner.data,
            _ => self,
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

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Expression::IdentifierValue(path, modifier) => {
                let modifier = match modifier {
                    Some(m) => m.to_string(),
                    None => "".to_string(),
                };
                write!(f, "{}{}", modifier, path)
            }
            Expression::Number(val, ty) => match ty {
                NumberType::Hex => write!(f, "${:x}", val),
                NumberType::Bin => write!(f, "%{:b}", val),
                NumberType::Dec => write!(f, "{}", val),
            },
            Expression::CurrentProgramCounter => write!(f, "*"),
            Expression::ExprParens(inner) => {
                write!(f, "[{}]", inner.data)
            }
            Expression::BinaryAdd(lhs, rhs) => {
                write!(f, "{} + {}", lhs.data, rhs.data)
            }
            Expression::BinarySub(lhs, rhs) => {
                write!(f, "{} - {}", lhs.data, rhs.data)
            }
            Expression::BinaryMul(lhs, rhs) => {
                write!(f, "{} * {}", lhs.data, rhs.data)
            }
            Expression::BinaryDiv(lhs, rhs) => {
                write!(f, "{} / {}", lhs.data, rhs.data)
            }
            Expression::Ws(l, inner, r) => format_ws(f, l, inner, r),
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Braces(tokens) => {
                let mut tokens = tokens
                    .iter()
                    .map(|t| format!("{}", t.data))
                    .collect::<Vec<_>>()
                    .join("\n");
                if !tokens.is_empty() {
                    tokens = format!("\n{}\n", tokens);
                }
                write!(f, "{{{}}}", tokens)
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
                    .collect::<Vec<_>>()
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
            Token::Config(cfg) => {
                let items = cfg
                    .items()
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, v.data))
                    .collect::<Vec<_>>()
                    .join(",")
                    .trim_end()
                    .to_string();
                write!(f, "[{}]", items)
            }
            Token::ConfigPair(_k, _v) => unimplemented!(),
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
