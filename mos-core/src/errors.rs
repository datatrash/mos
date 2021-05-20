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
        write!(f, "{}", self.format(&ErrorFormattingOptions::default()))
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

impl CoreError {
    pub fn format(&self, options: &ErrorFormattingOptions) -> String {
        let use_color = options.use_color;
        let use_prefix = options.use_prefix;

        match self {
            CoreError::Codegen { location, message } | CoreError::Parser { location, message } => {
                let mut filename: PathBuf = location.file.name().into();
                if let Some(relative_from) = &options.paths_relative_from {
                    filename = diff_paths(filename, relative_from).unwrap();
                }
                let location = format!(
                    "{}:{}:{}: ",
                    filename.to_string_lossy(),
                    location.begin.line + 1,
                    location.begin.column + 1
                );
                format!(
                    "{}{}",
                    location,
                    format_error(use_color, use_prefix, message)
                )
            }
            CoreError::Io(err) => format_error(use_color, use_prefix, err),
            CoreError::Json(err) => format_error(use_color, use_prefix, err),
            CoreError::Toml(err) => format_error(use_color, use_prefix, err),
            CoreError::BuildError(message) => format_error(use_color, use_prefix, message),
            CoreError::ParseBoolError(err) => format_error(use_color, use_prefix, err),
            CoreError::ParseIntError(err) => format_error(use_color, use_prefix, err),
            CoreError::Unknown => format_error(use_color, use_prefix, "unknown error"),
            CoreError::Multiple(errors) => errors
                .iter()
                .map(|e| e.format(&options))
                .sorted()
                .collect_vec()
                .join("\n"),
        }
    }
}

pub fn format_error<M: ToString>(use_color: bool, use_prefix: bool, message: M) -> String {
    use ansi_term::Colour::Red;
    let err = if use_prefix {
        if use_color {
            Red.paint("error: ")
        } else {
            "error: ".into()
        }
    } else {
        "".into()
    };
    format!("{}{}", err, message.to_string())
}
