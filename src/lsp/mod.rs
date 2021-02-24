mod diagnostics;

use crate::errors::MosResult;
use crate::lsp::diagnostics::{DidChange, DidOpen};
use lsp_server::{Connection, IoThreads, Message};
use lsp_types::notification::Notification;
use lsp_types::{InitializeParams, ServerCapabilities};
use std::collections::HashMap;
use std::sync::Arc;

struct LspContext {
    connection: Arc<Connection>,
    io_threads: IoThreads,
    documents: HashMap<String, String>,
}

impl LspContext {
    fn new() -> Self {
        let (connection, io_threads) = Connection::stdio();

        Self {
            connection: Arc::new(connection),
            io_threads,
            documents: HashMap::new(),
        }
    }

    fn publish_notification<N: Notification>(&self, params: N::Params) -> MosResult<()> {
        let params = serde_json::to_value(&params).unwrap();
        let n = lsp_server::Notification {
            method: N::METHOD.into(),
            params,
        };
        self.connection.sender.send(Message::Notification(n))?;
        Ok(())
    }

    fn join(self) -> MosResult<()> {
        self.io_threads.join()?;
        Ok(())
    }
}

trait RequestHandler {}

trait NotificationHandler {
    fn customize(&self, caps: &mut ServerCapabilities);
    fn method(&self) -> &'static str;
    fn handle(&self, ctx: &mut LspContext, req: lsp_server::Notification) -> MosResult<()>;
}

#[macro_export]
macro_rules! impl_notification_handler {
    ($ty:ty) => {
        impl NotificationHandler for $ty {
            fn customize(&self, caps: &mut ServerCapabilities) {
                TypedNotificationHandler::customize(self, caps)
            }

            fn method(&self) -> &'static str {
                TypedNotificationHandler::method(self)
            }

            fn handle(&self, ctx: &mut LspContext, req: lsp_server::Notification) -> MosResult<()> {
                let method = TypedNotificationHandler::method(self);
                let params = req.extract(method).unwrap();
                TypedNotificationHandler::handle(self, ctx, params)
            }
        }
    };
}

trait TypedNotificationHandler<N: Notification> {
    fn customize(&self, _caps: &mut ServerCapabilities) {}
    fn method(&self) -> &'static str {
        N::METHOD
    }
    fn handle(&self, ctx: &mut LspContext, params: N::Params) -> MosResult<()>;
}

pub struct LspServer {
    context: LspContext,
    _request_handlers: HashMap<&'static str, Box<dyn RequestHandler>>,
    notification_handlers: HashMap<&'static str, Box<dyn NotificationHandler>>,
}

impl LspServer {
    pub fn new() -> Self {
        let _request_handlers = HashMap::new();
        let notification_handlers = HashMap::new();

        Self {
            context: LspContext::new(),
            _request_handlers,
            notification_handlers,
        }
    }

    pub fn register_handlers(&mut self) {
        self.register_notification_handler(DidOpen {});
        self.register_notification_handler(DidChange {});
    }

    fn register_notification_handler<T: 'static + NotificationHandler>(&mut self, handler: T) {
        self.notification_handlers
            .insert(handler.method(), Box::new(handler));
    }

    pub fn start(mut self) -> MosResult<()> {
        eprintln!("Starting MOS language server");

        let mut caps = ServerCapabilities::default();
        self.notification_handlers
            .values()
            .for_each(|h| h.customize(&mut caps));

        let server_capabilities = serde_json::to_value(&caps).unwrap();
        let initialization_params = self.context.connection.initialize(server_capabilities)?;
        self.main_loop(initialization_params)?;
        self.context.join()?;

        eprintln!("Shutting down MOS language server");
        Ok(())
    }

    fn main_loop(&mut self, params: serde_json::Value) -> MosResult<()> {
        let _params: InitializeParams = serde_json::from_value(params).unwrap();
        for msg in &self.context.connection.clone().receiver {
            eprintln!("got msg: {:?}", msg);
            match msg {
                Message::Request(req) => {
                    if self.context.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    eprintln!("got request: {:?}", req);
                }
                Message::Response(resp) => {
                    eprintln!("got response: {:?}", resp);
                }
                Message::Notification(not) => {
                    eprintln!("got notification: {:?}", not);
                    match self.notification_handlers.get(not.method.as_str()) {
                        Some(handler) => {
                            handler.handle(&mut self.context, not)?;
                        }
                        None => {
                            eprintln!("unknown notification type: {}", not.method);
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
