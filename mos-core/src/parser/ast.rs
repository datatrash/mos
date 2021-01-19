use crate::errors::AsmError;
use crate::parser::mnemonic::Mnemonic;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

#[derive(Clone, Debug)]
pub struct State<'a> {
    pub errors: &'a RefCell<Vec<AsmError>>,
}

impl<'a> State<'a> {
    pub fn report_error(&self, error: AsmError) {
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl<'a> From<&LocatedSpan<'a>> for Location {
    fn from(span: &LocatedSpan) -> Self {
        Self {
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
pub struct Instruction {
    pub location: Location,
    pub mnemonic: Mnemonic,
    pub operand: Option<Box<Token>>,
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
pub struct Operand {
    pub expr: Box<Token>,
    pub addressing_mode: AddressingMode,
    pub suffix: Option<Box<Token>>,
}

#[derive(Debug)]
pub enum NumberType {
    Hex,
    Dec
}

#[derive(Debug)]
pub enum Token {
    Identifier(Identifier),
    Label(Identifier),
    Mnemonic(Mnemonic),
    Number(usize, NumberType),
    Instruction(Instruction),
    Operand(Operand),
    RegisterSuffix(Register),
    Ws((Vec<Comment>, Box<Token>, Vec<Comment>)),
    ExprParens(Box<Token>),
    BinaryAdd(Box<Token>, Box<Token>),
    BinarySub(Box<Token>, Box<Token>),
    BinaryMul(Box<Token>, Box<Token>),
    BinaryDiv(Box<Token>, Box<Token>),
    Data(Option<Box<Token>>, usize),
    Error,
}

impl Token {
    pub fn strip_whitespace(self) -> Self {
        let sob = |token: Option<Box<Token>>| {
            token.map(|t| Box::new(t.strip_whitespace()))
        };

        let sb = |token: Box<Token>| {
            Box::new(token.strip_whitespace())
        };

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
            Token::Data(token, size) => Token::Data(sob(token), size),
            Token::BinaryAdd(lhs, rhs) => Token::BinaryAdd(sb(lhs), sb(rhs)),
            Token::BinarySub(lhs, rhs) => Token::BinarySub(sb(lhs), sb(rhs)),
            Token::BinaryMul(lhs, rhs) => Token::BinaryMul(sb(lhs), sb(rhs)),
            Token::BinaryDiv(lhs, rhs) => Token::BinaryDiv(sb(lhs), sb(rhs)),
            Token::ExprParens(token) => Token::ExprParens(sb(token)),
            Token::Ws((_, token, _)) => *token,
            _ => self,
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
            Comment::CStyle(str) => write!(f, "{}", str),
            Comment::CppStyle(str) => write!(f, "{}", str),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Label(id) => {
                write!(f, "{}:", id.0)
            }
            Token::Instruction(i) => match &i.operand {
                Some(o) => {
                    write!(f, "{}{}", i.mnemonic, o)
                }
                None => write!(f, "{}", i.mnemonic),
            },
            Token::Operand(o) => {
                let suffix = match &o.suffix {
                    Some(s) => format!("{}", s),
                    None => "".to_string(),
                };

                match &o.addressing_mode {
                    AddressingMode::Immediate => write!(f, " #{}", o.expr),
                    AddressingMode::Implied => write!(f, ""),
                    AddressingMode::AbsoluteOrZP => {
                        write!(f, " {}{}", o.expr, suffix)
                    }
                    AddressingMode::OuterIndirect => {
                        write!(f, " ({}){}", o.expr, suffix)
                    }
                    AddressingMode::Indirect => {
                        write!(f, " ({}{})", o.expr, suffix)
                    }
                }
            }
            Token::RegisterSuffix(reg) => match reg {
                Register::X => write!(f, ", x"),
                Register::Y => write!(f, ", y"),
            },
            Token::Identifier(id) => {
                write!(f, "{}", id.0)
            }
            Token::Ws((l, inner, r)) => {
                for w in l {
                    let _ = write!(f, "{}", w);
                }
                let _ = write!(f, "{}", inner);
                for w in r {
                    let _ = write!(f, "{}", w);
                }
                Ok(())
            }
            Token::Number(val, ty) => {
                match ty {
                    NumberType::Hex => write!(f, "${:x}", val),
                    NumberType::Dec => write!(f, "{}", val),
                }
            }
            Token::ExprParens(inner) => {
                write!(f, "[{}]", inner)
            }
            Token::BinaryAdd(lhs, rhs) => {
                write!(f, "{} + {}", lhs, rhs)
            }
            Token::BinarySub(lhs, rhs) => {
                write!(f, "{} - {}", lhs, rhs)
            }
            Token::BinaryMul(lhs, rhs) => {
                write!(f, "{} * {}", lhs, rhs)
            }
            Token::BinaryDiv(lhs, rhs) => {
                write!(f, "{} / {}", lhs, rhs)
            }
            Token::Data(tok, sz) => {
                let label = match sz {
                    1 => ".byte",
                    2 => ".word",
                    4 => ".dword",
                    _ => panic!(),
                };
                match tok {
                    Some(t) => write!(f, "{} {}", label, t),
                    None => write!(f, "{}", label),
                }
            }
            _ => Ok(()),
        }
    }
}
