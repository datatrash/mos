use crate::core::parser::mnemonic::Mnemonic;
use crate::errors::MosError;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

#[derive(Clone, Debug)]
pub struct State<'a> {
    pub filename: &'a str,
    pub errors: Rc<RefCell<Vec<MosError<'a>>>>,
}

impl<'a> State<'a> {
    pub fn report_error(&self, error: MosError<'a>) {
        self.errors.borrow_mut().push(error);
    }
}

#[derive(Debug)]
pub enum Comment {
    CStyle(String),
    CppStyle(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

impl Display for Identifier {
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

impl<'a> From<&LocatedSpan<'a>> for Location<'a> {
    fn from(span: &LocatedSpan<'a>) -> Self {
        Self {
            path: span.extra.filename,
            line: span.location_line(),
            column: span.get_column() as u32,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Register {
    X,
    Y,
}

#[derive(Debug)]
pub struct Instruction<'a> {
    pub mnemonic: Mnemonic,
    pub operand: Option<Box<Located<'a, Token<'a>>>>,
}

#[derive(Debug)]
pub enum AddressingMode {
    AbsoluteOrZP,
    Immediate,
    Implied,
    Indirect,
    OuterIndirect,
}

#[derive(Debug)]
pub struct Operand<'a> {
    pub expr: Box<Located<'a, Expression<'a>>>,
    pub addressing_mode: AddressingMode,
    pub suffix: Option<Box<Located<'a, Token<'a>>>>,
}

#[derive(Debug)]
pub enum NumberType {
    Hex,
    Dec,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Identifier(Identifier),
    Number(usize, NumberType),
    ExprParens(Box<Located<'a, Expression<'a>>>),
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

#[derive(Debug)]
pub enum Token<'a> {
    Label(Identifier),
    Instruction(Instruction<'a>),
    Operand(Operand<'a>),
    RegisterSuffix(Register),
    Ws(Vec<Comment>, Box<Located<'a, Token<'a>>>, Vec<Comment>),
    Data(Option<Box<Located<'a, Expression<'a>>>>, usize),
    Error,
}

#[derive(Debug)]
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
            Token::Data(inner, size) => Token::Data(sob(inner), size),
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
            Expression::Identifier(id) => {
                write!(f, "{}", id.0)
            }
            Expression::Number(val, ty) => match ty {
                NumberType::Hex => write!(f, "${:x}", val),
                NumberType::Dec => write!(f, "{}", val),
            },
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
            Token::Label(id) => {
                write!(f, "{}:", id.0)
            }
            Token::Instruction(i) => match &i.operand {
                Some(o) => {
                    write!(f, "{}{}", i.mnemonic, o.data)
                }
                None => write!(f, "{}", i.mnemonic),
            },
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
                match tok {
                    Some(t) => write!(f, "{} {}", label, t.data),
                    None => write!(f, "{}", label),
                }
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
