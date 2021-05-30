use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use lsp_types::request::HoverRequest;
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind};

pub struct HoverRequestHandler;

impl_request_handler!(HoverRequestHandler);

impl RequestHandler<HoverRequest> for HoverRequestHandler {
    fn handle(&self, ctx: &mut LspContext, params: HoverParams) -> MosResult<Option<Hover>> {
        if ctx.codegen.is_none() {
            return Ok(None);
        }
        let codegen = ctx.codegen().unwrap();
        let codegen = codegen.lock().unwrap();
        let analysis = codegen.analysis();

        let defs = ctx.find_definitions(analysis, &params.text_document_position_params);

        Ok(defs
            .first()
            .map(|(_, def)| &def.location)
            .map(|location| Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: "hello *there*".into(),
                }),
                range: None,
            }))
    }
}
