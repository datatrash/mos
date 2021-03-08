use crate::errors::MosResult;
use crate::lsp::analysis::to_file_uri;
use crate::lsp::LspServer;
use lsp_types::notification::{DidOpenTextDocument, Notification};
use lsp_types::request::{Rename, Request};
use lsp_types::{
    DidOpenTextDocumentParams, Position, RenameParams, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams,
};

impl LspServer {
    pub fn did_open_text_document(&mut self, file: &str, source: &str) -> MosResult<()> {
        self.handle_message(notification::<DidOpenTextDocument>(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: to_file_uri(file),
                    language_id: "".to_string(),
                    version: 0,
                    text: source.to_string(),
                },
            },
        ))
    }

    pub fn rename(&mut self, file: &str, position: Position, new_name: &str) -> MosResult<()> {
        self.handle_message(request::<Rename>(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: to_file_uri(file),
                },
                position,
            },
            new_name: new_name.to_string(),
            work_done_progress_params: Default::default(),
        }))
    }
}

fn request<T: Request>(params: T::Params) -> lsp_server::Message {
    let params = serde_json::to_value(&params).unwrap();
    lsp_server::Request {
        id: 1.into(),
        method: T::METHOD.to_string(),
        params,
    }
    .into()
}

pub fn response<T: Request>(result: T::Result) -> lsp_server::Response {
    let result = serde_json::to_value(&result).unwrap();
    lsp_server::Response {
        id: 1.into(),
        result: Some(result),
        error: None,
    }
}

fn notification<T: Notification>(params: T::Params) -> lsp_server::Message {
    let params = serde_json::to_value(&params).unwrap();
    lsp_server::Notification {
        method: T::METHOD.to_string(),
        params,
    }
    .into()
}
