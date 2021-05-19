use crate::debugger::adapters::vice::protocol::ViceRequest;
use crate::debugger::adapters::MachineEvent;
use crate::debugger::protocol::ProtocolMessage;
use crossbeam_channel::{RecvError, SendError};
use itertools::Itertools;
use lsp_server::{Message, ProtocolError};
use mos_core::errors::{CoreError, ErrorFormattingOptions};
use std::num::ParseIntError;
use std::str::ParseBoolError;

pub type MosResult<T> = Result<T, MosError>;

#[allow(dead_code)]
#[derive(thiserror::Error, Debug)]
pub enum MosError {
    Clap(#[from] clap::Error),
    Cli(String),
    Core(#[from] CoreError),
    Io(#[from] std::io::Error),
    Json(#[from] serde_json::Error),
    Toml(#[from] toml::de::Error),
    Protocol(#[from] ProtocolError),
    CrossbeamLsp(#[from] SendError<Message>),
    CrossbeamDap(#[from] SendError<ProtocolMessage>),
    CrossbeamViceRequest(#[from] SendError<ViceRequest>),
    CrossbeamRecv(#[from] RecvError),
    CrossbeamSend(#[from] SendError<()>),
    CrossbeamSendMachineEvent(#[from] SendError<MachineEvent>),
    DebugAdapter(String),
    Vice(String),
    ParseBoolError(#[from] ParseBoolError),
    ParseIntError(#[from] ParseIntError),
    Multiple(Vec<MosError>),
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
            (MosError::Core(lhs), MosError::Core(rhs)) => lhs == rhs,
            (MosError::Multiple(lhs), MosError::Multiple(rhs)) => lhs == rhs,
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
        write!(f, "{}", self.format(&ErrorFormattingOptions::default()))
    }
}

impl MosError {
    pub fn format(&self, options: &ErrorFormattingOptions) -> String {
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
            MosError::Clap(err) => format_error(use_color, use_prefix, err),
            MosError::Cli(err) => format_error(use_color, use_prefix, err),
            MosError::Core(err) => format_error(use_color, use_prefix, err),
            MosError::Io(err) => format_error(use_color, use_prefix, err),
            MosError::Json(err) => format_error(use_color, use_prefix, err),
            MosError::Toml(err) => format_error(use_color, use_prefix, err),
            MosError::Protocol(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamLsp(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamDap(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamViceRequest(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamRecv(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamSend(err) => format_error(use_color, use_prefix, err),
            MosError::CrossbeamSendMachineEvent(err) => format_error(use_color, use_prefix, err),
            MosError::DebugAdapter(message) => format_error(use_color, use_prefix, message),
            MosError::Vice(message) => format_error(use_color, use_prefix, message),
            MosError::ParseBoolError(err) => format_error(use_color, use_prefix, err),
            MosError::ParseIntError(err) => format_error(use_color, use_prefix, err),
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
