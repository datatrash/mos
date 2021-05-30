use crate::errors::MosResult;
use crate::lsp::{LspContext, LspServer};
use lsp_types::notification::{DidOpenTextDocument, Notification};
use lsp_types::request::{
    Completion, GotoDefinition, HoverRequest, PrepareRenameRequest, References, Rename, Request,
};
use lsp_types::{
    CompletionParams, CompletionResponse, DidOpenTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverParams, Position, ReferenceContext, ReferenceParams,
    RenameParams, TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams, Url,
};
use serde::de::DeserializeOwned;
use std::path::{Path, PathBuf};

impl LspContext {
    fn pop_response<T: DeserializeOwned>(&self) -> T {
        let response = self
            .responses
            .lock()
            .unwrap()
            .pop()
            .unwrap()
            .result
            .unwrap();
        serde_json::from_value(response).unwrap()
    }
}

impl LspServer {
    pub fn did_open_text_document<P: AsRef<Path>>(
        &mut self,
        path: P,
        source: &str,
    ) -> MosResult<()> {
        self.handle_message(notification::<DidOpenTextDocument>(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Url::from_file_path(path)?,
                    language_id: "".to_string(),
                    version: 0,
                    text: source.to_string(),
                },
            },
        ))
    }

    pub fn prepare_rename<P: AsRef<Path>>(&mut self, path: P, position: Position) -> MosResult<()> {
        self.handle_message(request::<PrepareRenameRequest>(
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(path)?,
                },
                position,
            },
        ))
    }

    pub fn rename<P: AsRef<Path>>(
        &mut self,
        path: P,
        position: Position,
        new_name: &str,
    ) -> MosResult<()> {
        self.handle_message(request::<Rename>(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(path)?,
                },
                position,
            },
            new_name: new_name.to_string(),
            work_done_progress_params: Default::default(),
        }))
    }

    pub fn go_to_definition<P: AsRef<Path>>(
        &mut self,
        path: P,
        position: Position,
    ) -> MosResult<GotoDefinitionResponse> {
        self.handle_message(request::<GotoDefinition>(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(path)?,
                },
                position,
            },
            partial_result_params: Default::default(),
            work_done_progress_params: Default::default(),
        }))?;
        Ok(self.lock_context().pop_response())
    }

    pub fn find_references<P: AsRef<Path>>(
        &mut self,
        path: P,
        position: Position,
        include_declaration: bool,
    ) -> MosResult<Option<Vec<lsp_types::Location>>> {
        self.handle_message(request::<References>(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(path)?,
                },
                position,
            },
            partial_result_params: Default::default(),
            work_done_progress_params: Default::default(),
            context: ReferenceContext {
                include_declaration,
            },
        }))?;
        Ok(self.lock_context().pop_response())
    }

    pub fn hover<P: AsRef<Path>>(
        &mut self,
        path: P,
        position: Position,
    ) -> MosResult<Option<Hover>> {
        self.handle_message(request::<HoverRequest>(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(path)?,
                },
                position,
            },
            work_done_progress_params: Default::default(),
        }))?;
        Ok(self.lock_context().pop_response())
    }

    pub fn completion<P: AsRef<Path>>(
        &mut self,
        path: P,
        position: Position,
    ) -> MosResult<Option<CompletionResponse>> {
        self.handle_message(request::<Completion>(CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(path)?,
                },
                position,
            },
            partial_result_params: Default::default(),
            work_done_progress_params: Default::default(),
            context: None,
        }))?;
        Ok(self.lock_context().pop_response())
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

#[cfg(not(windows))]
pub fn test_root() -> PathBuf {
    PathBuf::from("/")
}

#[cfg(windows)]
pub fn test_root() -> PathBuf {
    PathBuf::from("C:\\")
}
