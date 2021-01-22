use crate::parser::Location;

pub type MosResult<'a, T> = Result<T, MosError<'a>>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum MosError<'a> {
    Parser {
        location: Location<'a>,
        message: String,
    },
    Codegen {
        location: Location<'a>,
        message: String,
    },
    Unknown,
}

impl<'a> std::fmt::Display for MosError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            MosError::Parser { location, message } => {
                write!(
                    f,
                    "{}:{}:{}: error: {}",
                    location.path, location.line, location.column, message
                )
            }
            MosError::Codegen { location, message } => {
                write!(
                    f,
                    "{}:{}:{}: error: {}",
                    location.path, location.line, location.column, message
                )
            }
            MosError::Unknown => write!(f, "Unknown error"),
        }
    }
}
