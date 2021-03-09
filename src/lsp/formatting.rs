use crate::errors::MosResult;
use crate::formatting::{format, FormattingOptions};
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use lsp_types::{DocumentFormattingParams, DocumentOnTypeFormattingParams, TextEdit};

pub struct FormattingRequestHandler {}
pub struct OnTypeFormattingRequestHandler {}

impl_request_handler!(FormattingRequestHandler);
impl_request_handler!(OnTypeFormattingRequestHandler);

impl RequestHandler<lsp_types::request::Formatting> for FormattingRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        _params: DocumentFormattingParams,
    ) -> MosResult<Option<Vec<TextEdit>>> {
        Ok(do_formatting(ctx))
    }
}

impl RequestHandler<lsp_types::request::OnTypeFormatting> for OnTypeFormattingRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        _params: DocumentOnTypeFormattingParams,
    ) -> MosResult<Option<Vec<TextEdit>>> {
        Ok(do_formatting(ctx))
    }
}

fn do_formatting(ctx: &mut LspContext) -> Option<Vec<TextEdit>> {
    ctx.analysis.as_ref().map(|analysis| {
        let new_text = format(analysis.tree.clone(), FormattingOptions::default());
        let edit = TextEdit {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: u32::MAX - 1,
                    character: 0,
                },
            },
            new_text,
        };
        vec![edit]
    })
}
