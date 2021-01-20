use crate::parser::Location;

pub type MosResult<T> = Result<T, MosError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum MosError {
    #[error("parser error")]
    Parser { location: Location, message: String },
    #[error("codegen error")]
    Codegen { location: Location, message: String },
    #[error("unknown error")]
    Unknown,
}
