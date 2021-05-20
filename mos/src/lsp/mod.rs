mod completion;
mod documents;
mod formatting;
mod references;
mod rename;
mod semantic_highlighting;
#[cfg(test)]
mod testing;
mod traits;

use crate::config::Config;
use crate::errors::MosResult;
use crate::lsp::completion::CompletionHandler;
use crate::lsp::documents::{
    DidChangeTextDocumentHandler, DidCloseTextDocumentHandler, DidOpenTextDocumentHandler,
};
use crate::lsp::formatting::{FormattingRequestHandler, OnTypeFormattingRequestHandler};
use crate::lsp::references::{
    DocumentHighlightRequestHandler, FindReferencesHandler, GoToDefinitionHandler,
};
use crate::lsp::rename::*;
use crate::lsp::semantic_highlighting::SemanticTokensFullRequestHandler;
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{Connection, IoThreads, Message, RequestId};
use lsp_types::notification::Notification;
use lsp_types::{
    CompletionOptions, DocumentOnTypeFormattingOptions, InitializeParams, OneOf, RenameOptions,
    ServerCapabilities, TextDocumentPositionParams, TextDocumentSyncKind, Url,
};
use mos_core::codegen::{Analysis, CodegenContext, Definition, DefinitionType};
use mos_core::errors::{CoreError, CoreResult};
use mos_core::parser::code_map::{LineCol, SpanLoc};
use mos_core::parser::source::ParsingSource;
use mos_core::parser::ParseTree;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
pub use traits::*;

fn to_line_col(pos: &lsp_types::Position) -> LineCol {
    LineCol {
        line: pos.line as usize,
        column: pos.character as usize,
    }
}

fn to_range(s: SpanLoc) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position {
            line: s.begin.line as u32,
            character: s.begin.column as u32,
        },
        end: lsp_types::Position {
            line: s.end.line as u32,
            character: s.end.column as u32,
        },
    }
}

fn to_location(s: SpanLoc) -> lsp_types::Location {
    lsp_types::Location {
        uri: Url::from_file_path(s.file.name()).unwrap(),
        range: to_range(s),
    }
}

pub struct LspContext {
    connection: Option<(Arc<Connection>, Option<IoThreads>)>,
    tree: Option<Arc<ParseTree>>,
    error: Option<CoreError>,
    codegen: Option<CodegenContext>,
    parsing_source: Arc<Mutex<LspParsingSource>>,
    shutdown_manager: Arc<Mutex<ShutdownManager>>,
    #[cfg(test)]
    responses: Arc<Mutex<Vec<lsp_server::Response>>>,
}

pub struct LspParsingSource {
    files: HashMap<PathBuf, String>,
}

impl LspParsingSource {
    fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn insert(&mut self, path: &Path, src: &str) {
        log::trace!("Inserted file: {:?}", &path);
        self.files.insert(path.to_path_buf(), src.to_string());
    }

    pub fn remove(&mut self, path: &Path) {
        self.files.remove(path);
    }
}

impl ParsingSource for LspParsingSource {
    fn get_contents(&self, path: &Path) -> CoreResult<String> {
        log::trace!("Trying to get file: {:?}", &path);
        let path = path.to_path_buf();
        if let Some(file) = self.files.get(&path) {
            Ok(file.clone())
        } else {
            let data = fs_err::read_to_string(path)?;
            Ok(data)
        }
    }
}

struct ShutdownManager {
    handlers: HashMap<usize, Sender<()>>,
}

static HANDLER_ID: AtomicUsize = AtomicUsize::new(0);

pub struct ShutdownReceiverHandle {
    manager: Arc<Mutex<ShutdownManager>>,
    receiver: Receiver<()>,
    handler_id: usize,
}

impl ShutdownReceiverHandle {
    pub fn receiver(&self) -> &Receiver<()> {
        &self.receiver
    }
}

impl Drop for ShutdownReceiverHandle {
    fn drop(&mut self) {
        self.manager
            .lock()
            .unwrap()
            .handlers
            .remove(&self.handler_id);
    }
}

impl ShutdownManager {
    fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }
}

impl LspContext {
    pub(crate) fn new() -> Self {
        Self {
            connection: None,
            tree: None,
            error: None,
            codegen: None,
            parsing_source: Arc::new(Mutex::new(LspParsingSource::new())),
            shutdown_manager: Arc::new(Mutex::new(ShutdownManager::new())),
            #[cfg(test)]
            responses: Arc::new(Mutex::new(vec![])),
        }
    }

    pub(crate) fn listen_stdio(&mut self) {
        let (connection, io_threads) = Connection::stdio();
        self.connection = Some((Arc::new(connection), Some(io_threads)));
    }

    #[cfg(test)]
    pub(crate) fn listen_memory(&mut self) -> Connection {
        let (connection, test_connection) = Connection::memory();
        self.connection = Some((Arc::new(connection), None));
        test_connection
    }

    pub fn add_shutdown_handler(&mut self) -> ShutdownReceiverHandle {
        let (s, r) = crossbeam_channel::bounded(1);
        let handler_id = HANDLER_ID.fetch_add(1, Ordering::Relaxed);
        self.shutdown_manager
            .lock()
            .unwrap()
            .handlers
            .insert(handler_id, s);
        ShutdownReceiverHandle {
            manager: self.shutdown_manager.clone(),
            receiver: r,
            handler_id,
        }
    }

    pub fn invoke_shutdown_handlers(&mut self) {
        // Grab the handlers and unlock the shutdown manager
        let handlers = {
            let mut mgr = self.shutdown_manager.lock().unwrap();
            std::mem::replace(&mut mgr.handlers, HashMap::new())
        };
        for sender in handlers.values() {
            let _ = sender.send(());
        }
    }

    pub fn config(&self) -> Option<Config> {
        self.parsing_source
            .lock()
            .unwrap()
            .try_get_contents(&Path::new("mos.toml"))
            .map(|toml| Config::from_toml(&toml).ok())
            .flatten()
    }

    #[cfg(test)]
    fn working_directory(&self) -> PathBuf {
        use crate::lsp::testing::test_root;
        test_root()
    }

    #[cfg(not(test))]
    fn working_directory(&self) -> PathBuf {
        use path_absolutize::Absolutize;
        PathBuf::from(".").absolutize().unwrap().into()
    }

    pub fn codegen(&self) -> Option<&CodegenContext> {
        self.codegen.as_ref()
    }

    pub fn codegen_mut(&mut self) -> Option<&mut CodegenContext> {
        self.codegen.as_mut()
    }

    pub fn analysis(&self) -> Option<&Analysis> {
        self.codegen.as_ref().map(|c| c.analysis())
    }

    fn connection(&self) -> Option<Arc<Connection>> {
        self.connection.as_ref().map(|c| c.0.clone())
    }

    fn parsing_source(&self) -> MutexGuard<LspParsingSource> {
        self.parsing_source.lock().unwrap()
    }

    #[cfg(test)]
    fn responses(&self) -> Vec<lsp_server::Response> {
        self.responses.lock().unwrap().clone()
    }

    fn publish_notification<N: Notification>(&self, params: N::Params) -> MosResult<()> {
        let params = serde_json::to_value(&params).unwrap();
        let n = lsp_server::Notification {
            method: N::METHOD.into(),
            params,
        };
        if let Some(conn) = self.connection() {
            conn.sender.send(Message::Notification(n))?;
        }
        Ok(())
    }

    fn send_response<R: DeserializeOwned + Serialize>(
        &self,
        id: RequestId,
        result: R,
    ) -> MosResult<()> {
        let result = serde_json::to_value(&result).unwrap();
        let response = lsp_server::Response {
            id,
            result: Some(result),
            error: None,
        };

        #[cfg(test)]
        {
            self.responses.lock().unwrap().push(response.clone());
        }
        if let Some(conn) = self.connection() {
            conn.sender.send(Message::Response(response))?;
        }
        Ok(())
    }

    fn join(self) -> MosResult<()> {
        if let Some(io) = self.connection.unwrap().1 {
            io.join()?;
        }
        Ok(())
    }

    fn find_definitions<'a>(
        &'a self,
        analysis: &'a Analysis,
        pos: &'a TextDocumentPositionParams,
    ) -> Vec<(&'a DefinitionType, &'a Definition)> {
        analysis.find(
            pos.text_document.uri.to_file_path().unwrap(),
            to_line_col(&pos.position),
        )
    }
}

pub struct LspServer {
    context: Arc<Mutex<LspContext>>,
    request_handlers: HashMap<&'static str, Box<dyn UntypedRequestHandler>>,
    notification_handlers: HashMap<&'static str, Box<dyn UntypedNotificationHandler>>,
}

impl LspServer {
    pub fn new(context: LspContext) -> Self {
        let request_handlers = HashMap::new();
        let notification_handlers = HashMap::new();

        let mut lsp = Self {
            context: Arc::new(Mutex::new(context)),
            request_handlers,
            notification_handlers,
        };

        lsp.register_request_handler(SemanticTokensFullRequestHandler {});
        lsp.register_request_handler(FormattingRequestHandler {});
        lsp.register_request_handler(OnTypeFormattingRequestHandler {});
        lsp.register_request_handler(GoToDefinitionHandler {});
        lsp.register_request_handler(FindReferencesHandler {});
        lsp.register_request_handler(DocumentHighlightRequestHandler {});
        lsp.register_request_handler(PrepareRenameRequestHandler {});
        lsp.register_request_handler(RenameHandler {});
        lsp.register_request_handler(CompletionHandler {});
        lsp.register_notification_handler(DidOpenTextDocumentHandler {});
        lsp.register_notification_handler(DidChangeTextDocumentHandler {});
        lsp.register_notification_handler(DidCloseTextDocumentHandler {});

        lsp
    }

    pub fn context(&self) -> Arc<Mutex<LspContext>> {
        self.context.clone()
    }

    pub fn lock_context(&self) -> MutexGuard<LspContext> {
        self.context.lock().unwrap()
    }

    pub fn start(mut self) -> MosResult<()> {
        log::info!("Starting MOS language server");

        let caps = ServerCapabilities {
            semantic_tokens_provider: Some(semantic_highlighting::caps().into()),
            text_document_sync: Some(TextDocumentSyncKind::Full.into()),
            references_provider: Some(OneOf::Left(true)),
            document_formatting_provider: Some(OneOf::Left(true)),
            document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                first_trigger_character: "}".to_string(),
                more_trigger_character: None,
            }),
            document_highlight_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            definition_provider: Some(OneOf::Left(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".into(), "#".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };
        let server_capabilities = serde_json::to_value(&caps).unwrap();
        let initialization_params = self
            .lock_context()
            .connection()
            .unwrap()
            .initialize(server_capabilities)?;
        self.main_loop(initialization_params)?;
        Arc::try_unwrap(self.context)
            .ok()
            .unwrap()
            .into_inner()
            .unwrap()
            .join()?;

        log::info!("Shutting down MOS language server");
        Ok(())
    }

    pub fn handle_message(&mut self, msg: Message) -> MosResult<()> {
        log::trace!("Handling message: {:?}", msg);

        let cloned_ctx = self.context.clone();
        let mut ctx = cloned_ctx.lock().unwrap();

        match msg {
            Message::Request(req) => match self.request_handlers.get(req.method.as_str()) {
                Some(handler) => {
                    handler.handle(&mut ctx, req)?;
                }
                None => {
                    if req.method == "shutdown" {
                        ctx.invoke_shutdown_handlers();
                        if ctx.connection().unwrap().handle_shutdown(&req)? {
                            return Ok(());
                        }
                    } else {
                        log::trace!("unknown request: {:?}", req);
                    }
                }
            },
            Message::Response(_resp) => {}
            Message::Notification(not) => {
                match self.notification_handlers.get(not.method.as_str()) {
                    Some(handler) => {
                        handler.handle(&mut ctx, not)?;
                    }
                    None => {
                        log::trace!("unknown notification: {:?}", not);
                    }
                }
            }
        }

        Ok(())
    }

    fn main_loop(&mut self, params: serde_json::Value) -> MosResult<()> {
        let connection = self.lock_context().connection().unwrap();

        let _params: InitializeParams = serde_json::from_value(params).unwrap();
        for msg in &connection.receiver {
            self.handle_message(msg)?;
        }

        Ok(())
    }

    fn register_request_handler<T: 'static + UntypedRequestHandler>(&mut self, handler: T) {
        self.request_handlers
            .insert(handler.method(), Box::new(handler));
    }

    fn register_notification_handler<T: 'static + UntypedNotificationHandler>(
        &mut self,
        handler: T,
    ) {
        self.notification_handlers
            .insert(handler.method(), Box::new(handler));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(unused_variables)]
    #[test]
    fn shutdown_handlers() {
        let mut ctx = LspContext::new();
        {
            let h1 = ctx.add_shutdown_handler();
            {
                let h2 = ctx.add_shutdown_handler();
            }
            assert_eq!(ctx.shutdown_manager.lock().unwrap().handlers.len(), 1);
        }

        // This should not block now, because all handlers are dropped
        ctx.invoke_shutdown_handlers();
    }
}
