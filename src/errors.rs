use crate::parser::Span;
use nom::error::{ErrorKind, ParseError};

pub type AsmResult<T> = Result<T, AsmError>;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum AsmError {
    #[error("parser error")]
    Parser(#[from] nom::Err<nom::error::Error<Span<'static>>>),
    #[error("unknown error")]
    Unknown,
}

impl<I> ParseError<I> for AsmError {
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self {
        AsmError::Unknown
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}
