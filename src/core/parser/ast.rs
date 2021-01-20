use crate::core::parser::mnemonic::Mnemonic;
use crate::errors::MosError;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

#[derive(Clone, Debug)]
pub struct State<'a> {
    pub filename: &'a str,
    pub errors: &'a RefCell<Vec<MosError>>,
}

impl<'a> State<'a> {
    pub fn report_error(&self, error: MosError) {
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
pub struct Location {
    pub path: String,
    pub line: u32,
    pub column: u32,
}

impl Location {
    pub fn unknown() -> Self {
        Self {
            path: "unknown".to_string(),
            line: 0,
            column: 0,
        }
    }
}

impl<'a> From<&LocatedSpan<'a>> for Location {
    fn from(span: &LocatedSpan) -> Self {
        Self {
            path: span.extra.filename.to_string(),
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
    pub mnemonic: Mnemonic,
    pub operand: Option<Box<LocatedToken>>,
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
    pub expr: Box<LocatedToken>,
    pub addressing_mode: AddressingMode,
    pub suffix: Option<Box<LocatedToken>>,
}

#[derive(Debug)]
pub enum NumberType {
    Hex,
    Dec,
}

#[derive(Debug)]
pub enum Token {
    Identifier(Identifier),
    Label(Identifier),
    Number(usize, NumberType),
    Instruction(Instruction),
    Operand(Operand),
    RegisterSuffix(Register),
    Ws((Vec<Comment>, Box<LocatedToken>, Vec<Comment>)),
    ExprParens(Box<LocatedToken>),
    BinaryAdd(Box<LocatedToken>, Box<LocatedToken>),
    BinarySub(Box<LocatedToken>, Box<LocatedToken>),
    BinaryMul(Box<LocatedToken>, Box<LocatedToken>),
    BinaryDiv(Box<LocatedToken>, Box<LocatedToken>),
    Data(Option<Box<LocatedToken>>, usize),
    Error,
}

#[derive(Debug)]
pub struct LocatedToken {
    pub location: Location,
    pub token: Token,
}

impl LocatedToken {
    pub fn from<L: Into<Location>>(location: L, token: Token) -> Self {
        Self {
            location: location.into(),
            token,
        }
    }
}

impl LocatedToken {
    pub fn strip_whitespace(self) -> Self {
        let sob = |token: Option<Box<LocatedToken>>| token.map(|t| Box::new(t.strip_whitespace()));

        let sb = |token: Box<LocatedToken>| Box::new(token.strip_whitespace());

        match self.token {
            Token::Instruction(i) => LocatedToken {
                token: Token::Instruction(Instruction {
                    operand: sob(i.operand),
                    ..i
                }),
                ..self
            },
            Token::Operand(o) => LocatedToken {
                token: Token::Operand(Operand {
                    expr: sb(o.expr),
                    suffix: sob(o.suffix),
                    ..o
                }),
                ..self
            },
            Token::Data(token, size) => LocatedToken {
                token: Token::Data(sob(token), size),
                ..self
            },
            Token::BinaryAdd(lhs, rhs) => LocatedToken {
                token: Token::BinaryAdd(sb(lhs), sb(rhs)),
                ..self
            },
            Token::BinarySub(lhs, rhs) => LocatedToken {
                token: Token::BinarySub(sb(lhs), sb(rhs)),
                ..self
            },
            Token::BinaryMul(lhs, rhs) => LocatedToken {
                token: Token::BinaryMul(sb(lhs), sb(rhs)),
                ..self
            },
            Token::BinaryDiv(lhs, rhs) => LocatedToken {
                token: Token::BinaryDiv(sb(lhs), sb(rhs)),
                ..self
            },
            Token::ExprParens(token) => LocatedToken {
                token: Token::ExprParens(sb(token)),
                ..self
            },
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
                    write!(f, "{}{}", i.mnemonic, o.token)
                }
                None => write!(f, "{}", i.mnemonic),
            },
            Token::Operand(o) => {
                let suffix = match &o.suffix {
                    Some(s) => format!("{}", s.token),
                    None => "".to_string(),
                };

                match &o.addressing_mode {
                    AddressingMode::Immediate => write!(f, " #{}", o.expr.token),
                    AddressingMode::Implied => write!(f, ""),
                    AddressingMode::AbsoluteOrZP => {
                        write!(f, " {}{}", o.expr.token, suffix)
                    }
                    AddressingMode::OuterIndirect => {
                        write!(f, " ({}){}", o.expr.token, suffix)
                    }
                    AddressingMode::Indirect => {
                        write!(f, " ({}{})", o.expr.token, suffix)
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
                let _ = write!(f, "{}", inner.token);
                for w in r {
                    let _ = write!(f, "{}", w);
                }
                Ok(())
            }
            Token::Number(val, ty) => match ty {
                NumberType::Hex => write!(f, "${:x}", val),
                NumberType::Dec => write!(f, "{}", val),
            },
            Token::ExprParens(inner) => {
                write!(f, "[{}]", inner.token)
            }
            Token::BinaryAdd(lhs, rhs) => {
                write!(f, "{} + {}", lhs.token, rhs.token)
            }
            Token::BinarySub(lhs, rhs) => {
                write!(f, "{} - {}", lhs.token, rhs.token)
            }
            Token::BinaryMul(lhs, rhs) => {
                write!(f, "{} * {}", lhs.token, rhs.token)
            }
            Token::BinaryDiv(lhs, rhs) => {
                write!(f, "{} / {}", lhs.token, rhs.token)
            }
            Token::Data(tok, sz) => {
                let label = match sz {
                    1 => ".byte",
                    2 => ".word",
                    4 => ".dword",
                    _ => panic!(),
                };
                match tok {
                    Some(t) => write!(f, "{} {}", label, t.token),
                    None => write!(f, "{}", label),
                }
            }
            _ => Ok(()),
        }
    }
}
