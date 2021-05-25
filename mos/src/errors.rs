use crate::debugger::adapters::vice::protocol::ViceRequest;
use crate::debugger::adapters::MachineEvent;
use crate::debugger::protocol::ProtocolMessage;
use crossbeam_channel::{RecvError, SendError};
use itertools::Itertools;
use lsp_server::{Message, ProtocolError};
use mos_core::errors::{format_error, CoreError, ErrorFormattingOptions, ErrorMessage};
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
    UnitTest(String),
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
        write!(
            f,
            "{}",
            format_error(self, &ErrorFormattingOptions::default())
        )
    }
}

impl From<MosError> for ErrorMessage {
    fn from(e: MosError) -> Self {
        e.message()
    }
}

impl From<&MosError> for ErrorMessage {
    fn from(e: &MosError) -> Self {
        e.message()
    }
}

impl MosError {
    fn message(&self) -> ErrorMessage {
        match self {
            MosError::Clap(err) => err.to_string().into(),
            MosError::Cli(err) => err.to_string().into(),
            MosError::Core(err) => err.to_string().into(),
            MosError::Io(err) => err.to_string().into(),
            MosError::Json(err) => err.to_string().into(),
            MosError::Toml(err) => err.to_string().into(),
            MosError::Protocol(err) => err.to_string().into(),
            MosError::CrossbeamLsp(err) => err.to_string().into(),
            MosError::CrossbeamDap(err) => err.to_string().into(),
            MosError::CrossbeamViceRequest(err) => err.to_string().into(),
            MosError::CrossbeamRecv(err) => err.to_string().into(),
            MosError::CrossbeamSend(err) => err.to_string().into(),
            MosError::CrossbeamSendMachineEvent(err) => err.to_string().into(),
            MosError::DebugAdapter(message) => message.to_string().into(),
            MosError::Vice(message) => message.to_string().into(),
            MosError::ParseBoolError(err) => err.to_string().into(),
            MosError::ParseIntError(err) => err.to_string().into(),
            MosError::UnitTest(message) => message.to_string().into(),
            MosError::Unknown => "unknown error".to_string().into(),
            MosError::Multiple(errors) => {
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
