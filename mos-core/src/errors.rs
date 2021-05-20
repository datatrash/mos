use crate::parser::code_map::SpanLoc;
use itertools::Itertools;
use pathdiff::diff_paths;
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;
use std::path::PathBuf;
use std::str::ParseBoolError;

pub type CoreResult<T> = Result<T, CoreError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug)]
pub enum CoreError {
    BuildError(String),
    Codegen { location: SpanLoc, message: String },
    Io(#[from] std::io::Error),
    Parser { location: SpanLoc, message: String },
    Multiple(Vec<CoreError>),
    Json(#[from] serde_json::Error),
    Toml(#[from] toml::de::Error),
    ParseBoolError(#[from] ParseBoolError),
    ParseIntError(#[from] ParseIntError),
    Unknown,
}

impl Display for CoreError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format_error(self, &ErrorFormattingOptions::default())
        )
    }
}

impl From<()> for CoreError {
    fn from(_: ()) -> Self {
        CoreError::Unknown
    }
}

impl PartialEq for CoreError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                CoreError::Parser {
                    location: lloc,
                    message: lmsg,
                    ..
                },
                CoreError::Parser {
                    location: rloc,
                    message: rmsg,
                    ..
                },
            ) => lloc == rloc && lmsg == rmsg,
            (
                CoreError::Codegen {
                    location: lloc,
                    message: lmsg,
                    ..
                },
                CoreError::Codegen {
                    location: rloc,
                    message: rmsg,
                    ..
                },
            ) => lloc == rloc && lmsg == rmsg,
            (CoreError::Multiple(lhs), CoreError::Multiple(rhs)) => lhs == rhs,
            (CoreError::BuildError(lhs), CoreError::BuildError(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<T: Into<CoreError>> From<Vec<T>> for CoreError {
    fn from(errors: Vec<T>) -> Self {
        Self::Multiple(errors.into_iter().map(|e| e.into()).collect())
    }
}

pub struct ErrorFormattingOptions {
    pub use_color: bool,
    pub paths_relative_from: Option<PathBuf>,
    pub use_prefix: bool,
}

impl Default for ErrorFormattingOptions {
    fn default() -> Self {
        Self {
            use_color: false,
            paths_relative_from: None,
            use_prefix: true,
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct ErrorMessageLine {
    location: Option<SpanLoc>,
    message: String,
}

pub struct ErrorMessage {
    pub lines: Vec<ErrorMessageLine>,
}

impl From<String> for ErrorMessage {
    fn from(message: String) -> Self {
        ErrorMessage {
            lines: vec![ErrorMessageLine {
                location: None,
                message,
            }],
        }
    }
}

impl From<CoreError> for ErrorMessage {
    fn from(e: CoreError) -> Self {
        e.message()
    }
}

impl From<&CoreError> for ErrorMessage {
    fn from(e: &CoreError) -> Self {
        e.message()
    }
}

impl CoreError {
    fn message(&self) -> ErrorMessage {
        match self {
            CoreError::Codegen { location, message } | CoreError::Parser { location, message } => {
                ErrorMessage {
                    lines: vec![ErrorMessageLine {
                        location: Some(location.clone()),
                        message: message.clone(),
                    }],
                }
            }
            CoreError::Io(err) => err.to_string().into(),
            CoreError::Json(err) => err.to_string().into(),
            CoreError::Toml(err) => err.to_string().into(),
            CoreError::BuildError(message) => message.to_string().into(),
            CoreError::ParseBoolError(err) => err.to_string().into(),
            CoreError::ParseIntError(err) => err.to_string().into(),
            CoreError::Unknown => "unknown error".to_string().into(),
            CoreError::Multiple(errors) => {
                let lines = errors
                    .iter()
                    .map(|e| e.message().lines)
                    .sorted()
                    .flatten()
                    .collect_vec();
                ErrorMessage { lines }
            }
        }
    }
}

pub fn format_error<M: Into<ErrorMessage>>(message: M, options: &ErrorFormattingOptions) -> String {
    let message = message.into();

    message
        .lines
        .into_iter()
        .map(|line| {
            let location = line.location.map(|location| {
                let mut filename: PathBuf = location.file.name().into();
                if let Some(relative_from) = &options.paths_relative_from {
                    filename = diff_paths(filename, relative_from).unwrap();
                }
                format!(
                    "{}:{}:{}: ",
                    filename.to_string_lossy(),
                    location.begin.line + 1,
                    location.begin.column + 1
                )
            });

            use ansi_term::Colour::Red;
            let err = if options.use_prefix {
                if options.use_color {
                    Red.paint("error: ")
                } else {
                    "error: ".into()
                }
            } else {
                "".into()
            };
            format!("{}{}{}", location.unwrap_or_default(), err, line.message)
        })
        .join("\n")
}
