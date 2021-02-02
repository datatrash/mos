use itertools::Itertools;

use crate::core::parser::OwnedLocation;

pub type MosResult<T> = Result<T, MosError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug)]
pub enum MosError {
    Parser {
        location: OwnedLocation,
        message: String,
    },
    Codegen {
        location: OwnedLocation,
        message: String,
    },
    Io(#[from] std::io::Error),
    Clap(#[from] clap::Error),
    Unknown,
    Multiple(Vec<MosError>),
}

impl<T: Into<MosError>> From<Vec<T>> for MosError {
    fn from(errors: Vec<T>) -> Self {
        Self::Multiple(errors.into_iter().map(|e| e.into()).collect())
    }
}

impl std::fmt::Display for MosError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format(false))
    }
}

impl MosError {
    pub fn format(&self, use_color: bool) -> String {
        use ansi_term::Colour::Red;

        match self {
            MosError::Parser { location, message } | MosError::Codegen { location, message } => {
                let err = if use_color {
                    Red.paint("error:")
                } else {
                    "error:".into()
                };
                format!(
                    "{}:{}:{}: {} {}",
                    location.path, location.line, location.column, err, message
                )
            }
            MosError::Io(err) => format!("{}", err),
            MosError::Clap(err) => format!("{}", err),
            MosError::Unknown => "unknown error".to_string(),
            MosError::Multiple(errors) => errors
                .iter()
                .map(|e| e.format(use_color))
                .collect_vec()
                .join("\n"),
        }
    }
}
