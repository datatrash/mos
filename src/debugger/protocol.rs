use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ProtocolMessage {
    Request(RequestMessage),
    Event(EventMessage),
    Response(ResponseMessage),
}

impl ProtocolMessage {
    pub fn seq(&self) -> usize {
        match self {
            Self::Request(req) => req.seq,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct RequestMessage {
    pub seq: usize,
    pub command: String,
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    pub arguments: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EventMessage {
    pub seq: usize,
    pub event: String,
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    pub body: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ResponseMessage {
    pub seq: usize,
    #[serde(rename = "request_seq")] // wtf
    pub request_seq: usize,
    pub command: String,
    pub success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    pub body: serde_json::Value,
}

pub trait Request {
    type Arguments: DeserializeOwned + Serialize;
    type Response: DeserializeOwned + Serialize;
    const COMMAND: &'static str;
}

pub trait Event {
    type Body: DeserializeOwned + Serialize;
    const EVENT: &'static str;
}

pub trait Response {
    type Body: DeserializeOwned + Serialize;
    const COMMAND: &'static str;
}
