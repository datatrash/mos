use crate::diagnostic_emitter::MosResult;
use crate::lsp::LspContext;
use lsp_types::notification::Notification;
use lsp_types::request::Request;

pub trait UntypedRequestHandler {
    fn method(&self) -> &'static str;
    fn handle(&self, ctx: &mut LspContext, req: lsp_server::Request) -> MosResult<()>;
}

pub trait UntypedNotificationHandler {
    fn method(&self) -> &'static str;
    fn handle(&self, ctx: &mut LspContext, req: lsp_server::Notification) -> MosResult<()>;
}

pub trait RequestHandler<R: Request> {
    fn method(&self) -> &'static str {
        R::METHOD
    }
    fn handle(&self, ctx: &mut LspContext, params: R::Params) -> MosResult<R::Result>;
}

pub trait NotificationHandler<N: Notification> {
    fn method(&self) -> &'static str {
        N::METHOD
    }
    fn handle(&self, ctx: &mut LspContext, params: N::Params) -> MosResult<()>;
}

#[macro_export]
macro_rules! impl_request_handler {
    ($ty:ty) => {
        impl $crate::lsp::traits::UntypedRequestHandler for $ty {
            fn method(&self) -> &'static str {
                RequestHandler::method(self)
            }

            fn handle(&self, ctx: &mut LspContext, req: lsp_server::Request) -> MosResult<()> {
                let method = RequestHandler::method(self);
                let (id, params) = req.extract(method).unwrap();
                let result = RequestHandler::handle(self, ctx, params)?;
                ctx.send_response(id, result)?;
                Ok(())
            }
        }
    };
}

#[macro_export]
macro_rules! impl_notification_handler {
    ($ty:ty) => {
        impl $crate::lsp::traits::UntypedNotificationHandler for $ty {
            fn method(&self) -> &'static str {
                NotificationHandler::method(self)
            }

            fn handle(&self, ctx: &mut LspContext, req: lsp_server::Notification) -> MosResult<()> {
                let method = NotificationHandler::method(self);
                let params = req.extract(method).unwrap();
                NotificationHandler::handle(self, ctx, params)
            }
        }
    };
}
