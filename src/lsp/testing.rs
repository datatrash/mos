use crate::errors::MosResult;
use crate::lsp::{path_to_uri, LspContext, LspServer};
use lsp_types::notification::{DidOpenTextDocument, Notification};
use lsp_types::request::{GotoDefinition, References, Rename, Request};
use lsp_types::{
    DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, Position,
    ReferenceContext, ReferenceParams, RenameParams, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams,
};
use serde::de::DeserializeOwned;
use std::path::PathBuf;

impl LspContext {
    fn pop_response<T: DeserializeOwned>(&self) -> T {
        let response = self.responses.borrow_mut().pop().unwrap().result.unwrap();
        serde_json::from_value(response).unwrap()
    }
}

impl LspServer {
    pub fn did_open_text_document<P: Into<PathBuf>>(
        &mut self,
        path: P,
        source: &str,
    ) -> MosResult<()> {
        self.handle_message(notification::<DidOpenTextDocument>(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: path_to_uri(path),
                    language_id: "".to_string(),
                    version: 0,
                    text: source.to_string(),
                },
            },
        ))
    }

    pub fn rename<P: Into<PathBuf>>(
        &mut self,
        path: P,
        position: Position,
        new_name: &str,
    ) -> MosResult<()> {
        self.handle_message(request::<Rename>(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: path_to_uri(path),
                },
                position,
            },
            new_name: new_name.to_string(),
            work_done_progress_params: Default::default(),
        }))
    }

    pub fn go_to_definition<P: Into<PathBuf>>(
        &mut self,
        path: P,
        position: Position,
    ) -> MosResult<GotoDefinitionResponse> {
        self.handle_message(request::<GotoDefinition>(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: path_to_uri(path),
                },
                position,
            },
            partial_result_params: Default::default(),
            work_done_progress_params: Default::default(),
        }))?;
        Ok(self.context.pop_response())
    }

    pub fn find_references<P: Into<PathBuf>>(
        &mut self,
        path: P,
        position: Position,
        include_declaration: bool,
    ) -> MosResult<Option<Vec<lsp_types::Location>>> {
        self.handle_message(request::<References>(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: path_to_uri(path),
                },
                position,
            },
            partial_result_params: Default::default(),
            work_done_progress_params: Default::default(),
            context: ReferenceContext {
                include_declaration,
            },
        }))?;
        Ok(self.context.pop_response())
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

pub fn range(
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
) -> lsp_types::Range {
    lsp_types::Range {
        start: Position {
            line: start_line as u32,
            character: start_col as u32,
        },
        end: Position {
            line: end_line as u32,
            character: end_col as u32,
        },
    }
}
