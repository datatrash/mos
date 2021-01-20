use crate::parser::Location;

pub type MosResult<T> = Result<T, MosError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum MosError {
    Parser { location: Location, message: String },
    Codegen { location: Location, message: String },
    Unknown,
}

impl std::fmt::Display for MosError {
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
