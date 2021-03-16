use crate::core::parser::ParseTree;
use crossbeam_channel::SendError;
use itertools::Itertools;
use lsp_server::{Message, ProtocolError};
use pathdiff::diff_paths;
use std::path::PathBuf;
use std::str::ParseBoolError;
use std::sync::Arc;

pub type MosResult<T> = Result<T, MosError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug)]
pub enum MosError {
    BuildError(String),
    Clap(#[from] clap::Error),
    Cli(String),
    Codegen {
        tree: Arc<ParseTree>,
        span: codemap::Span,
        message: String,
    },
    Io(#[from] std::io::Error),
    Parser {
        tree: Arc<ParseTree>,
        span: codemap::Span,
        message: String,
    },
    Multiple(Vec<MosError>),
    Toml(#[from] toml::de::Error),
    Protocol(#[from] ProtocolError),
    Crossbeam(#[from] SendError<Message>),
    ParseBoolError(#[from] ParseBoolError),
    Unknown,
}

impl PartialEq for MosError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                MosError::Parser {
                    span: lloc,
                    message: lmsg,
                    ..
                },
                MosError::Parser {
                    span: rloc,
                    message: rmsg,
                    ..
                },
            ) => lloc == rloc && lmsg == rmsg,
            (
                MosError::Codegen {
                    span: lloc,
                    message: lmsg,
                    ..
                },
                MosError::Codegen {
                    span: rloc,
                    message: rmsg,
                    ..
                },
            ) => lloc == rloc && lmsg == rmsg,
            (MosError::Multiple(lhs), MosError::Multiple(rhs)) => lhs == rhs,
            (MosError::BuildError(lhs), MosError::BuildError(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<T: Into<MosError>> From<Vec<T>> for MosError {
    fn from(errors: Vec<T>) -> Self {
        Self::Multiple(errors.into_iter().map(|e| e.into()).collect())
    }
}

impl std::fmt::Display for MosError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format(&MosErrorOptions::default()))
    }
}

#[derive(Default)]
pub struct MosErrorOptions {
    pub use_color: bool,
    pub paths_relative_from: Option<PathBuf>,
}

impl MosError {
    pub fn format(&self, options: &MosErrorOptions) -> String {
        let use_color = options.use_color;

        fn format_error<M: ToString>(use_color: bool, message: M) -> String {
            use ansi_term::Colour::Red;
            let err = if use_color {
                Red.paint("error:")
            } else {
                "error:".into()
            };
            format!("{} {}", err, message.to_string())
        }

        match self {
            MosError::Codegen {
                tree,
                span,
                message,
            }
            | MosError::Parser {
                tree,
                span,
                message,
            } => {
                let location = tree.code_map().look_up_span(*span);
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
                format!("{}{}", location, format_error(use_color, message))
            }
            MosError::Io(err) => format_error(use_color, err),
            MosError::Clap(err) => format_error(use_color, err),
            MosError::Cli(err) => format_error(use_color, err),
            MosError::Toml(err) => format_error(use_color, err),
            MosError::Protocol(err) => format_error(use_color, err),
            MosError::Crossbeam(err) => format_error(use_color, err),
            MosError::BuildError(message) => format_error(use_color, message),
            MosError::ParseBoolError(err) => format_error(use_color, err),
            MosError::Unknown => format_error(use_color, "unknown error"),
            MosError::Multiple(errors) => errors
                .iter()
                .map(|e| e.format(&options))
                .collect_vec()
                .join("\n"),
        }
    }
}
