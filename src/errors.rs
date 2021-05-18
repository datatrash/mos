use crate::core::parser::code_map::SpanLoc;
use crate::debugger::adapters::vice::protocol::ViceRequest;
use crate::debugger::adapters::MachineEvent;
use crate::debugger::protocol::ProtocolMessage;
use crossbeam_channel::{RecvError, SendError};
use itertools::Itertools;
use lsp_server::{Message, ProtocolError};
use pathdiff::diff_paths;
use std::num::ParseIntError;
use std::path::PathBuf;
use std::str::ParseBoolError;

pub type MosResult<T> = Result<T, MosError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug)]
pub enum MosError {
    BuildError(String),
    Clap(#[from] clap::Error),
    Cli(String),
    Codegen { location: SpanLoc, message: String },
    Io(#[from] std::io::Error),
    Parser { location: SpanLoc, message: String },
    Multiple(Vec<MosError>),
    Json(#[from] serde_json::Error),
    Toml(#[from] toml::de::Error),
    Protocol(#[from] ProtocolError),
    CrossbeamLsp(#[from] SendError<Message>),
    CrossbeamDap(#[from] SendError<ProtocolMessage>),
    CrossbeamViceRequest(#[from] SendError<ViceRequest>),
    CrossbeamRecv(#[from] RecvError),
    CrossbeamSend(#[from] SendError<()>),
    CrossbeamSendMachineEvent(#[from] SendError<MachineEvent>),
    ParseBoolError(#[from] ParseBoolError),
    ParseIntError(#[from] ParseIntError),
    DebugAdapter(String),
    Vice(String),
    Unknown,
}

impl From<()> for MosError {
    fn from(_: ()) -> Self {
        MosError::Unknown
    }
}

impl PartialEq for MosError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                MosError::Parser {
                    location: lloc,
                    message: lmsg,
                    ..
                },
                MosError::Parser {
                    location: rloc,
                    message: rmsg,
                    ..
                },
            ) => lloc == rloc && lmsg == rmsg,
            (
                MosError::Codegen {
                    location: lloc,
                    message: lmsg,
                    ..
                },
                MosError::Codegen {
                    location: rloc,
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

pub struct MosErrorOptions {
    pub use_color: bool,
    pub paths_relative_from: Option<PathBuf>,
    pub use_prefix: bool,
}

impl Default for MosErrorOptions {
    fn default() -> Self {
        Self {
            use_color: false,
            paths_relative_from: None,
            use_prefix: true,
        }
    }
}

impl MosError {
    pub fn format(&self, options: &MosErrorOptions) -> String {
        let use_color = options.use_color;
        let use_prefix = options.use_prefix;

        fn format_error<M: ToString>(use_color: bool, use_prefix: bool, message: M) -> String {
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

        match self {
            MosError::Codegen { location, message } | MosError::Parser { location, message } => {
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
            MosError::Io(err) => format_error(use_color, use_prefix, err),
            MosError::Clap(err) => format_error(use_color, use_prefix, err),
            MosError::Cli(err) => format_error(use_color, use_prefix, err),
            MosError::Json(err) => format_error(use_color, use_prefix, err),
            MosError::Toml(err) => format_error(use_color, use_prefix, err),
            MosError::Protocol(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamLsp(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamDap(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamViceRequest(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamRecv(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamSend(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamSendMachineEvent(err) => format_error(use_color, use_prefix, err),
            MosError::BuildError(message) => format_error(use_color, use_prefix, message),
            MosError::ParseBoolError(err) => format_error(use_color, use_prefix, err),
            MosError::ParseIntError(err) => format_error(use_color, use_prefix, err),
            MosError::DebugAdapter(message) => format_error(use_color, use_prefix, message),
            MosError::Vice(message) => format_error(use_color, use_prefix, message),
            MosError::Unknown => format_error(use_color, use_prefix, "unknown error"),
            MosError::Multiple(errors) => errors
                .iter()
                .map(|e| e.format(&options))
                .sorted()
                .collect_vec()
                .join("\n"),
        }
    }
}
