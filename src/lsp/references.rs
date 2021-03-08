use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::analysis::{Analysis, Definition};
use crate::lsp::{LspContext, RequestHandler};
use lsp_types::request::{DocumentHighlightRequest, GotoDefinition, References};
use lsp_types::{
    DocumentHighlight, DocumentHighlightParams, GotoDefinitionParams, GotoDefinitionResponse,
    Location, ReferenceParams, TextDocumentPositionParams,
};

pub struct GoToDefinitionHandler {}
pub struct FindReferencesHandler {}
pub struct DocumentHighlightRequestHandler {}

impl_request_handler!(GoToDefinitionHandler);
impl_request_handler!(FindReferencesHandler);
impl_request_handler!(DocumentHighlightRequestHandler);

fn find_definition<'a>(
    ctx: &'a LspContext,
    pos: &'a TextDocumentPositionParams,
) -> Option<(&'a Analysis, &'a Definition)> {
    if let Some(analysis) = ctx.documents.get(&pos.text_document.uri) {
        if let Some(def) = analysis.find(pos.position) {
            return Some((analysis, def));
        }
    }

    None
}

impl RequestHandler<GotoDefinition> for GoToDefinitionHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: GotoDefinitionParams,
    ) -> MosResult<Option<GotoDefinitionResponse>> {
        Ok(find_definition(ctx, &params.text_document_position_params)
            .map(|(analysis, def)| {
                let l = analysis.look_up_span(def.location.unwrap());
                Some(Location::from(l).into())
            })
            .flatten())
    }
}

impl RequestHandler<References> for FindReferencesHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: ReferenceParams,
    ) -> MosResult<Option<Vec<Location>>> {
        Ok(find_definition(ctx, &params.text_document_position)
            .map(|(analysis, def)| {
                let mut r = vec![];
                if let Some(l) = def.location {
                    r.push(analysis.look_up_span(l).into());
                }
                for u in &def.usages {
                    r.push(analysis.look_up_span(*u).into());
                }

                Some(r)
            })
            .flatten())
    }
}

impl RequestHandler<DocumentHighlightRequest> for DocumentHighlightRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: DocumentHighlightParams,
    ) -> MosResult<Option<Vec<DocumentHighlight>>> {
        Ok(find_definition(ctx, &params.text_document_position_params)
            .map(|(analysis, def)| {
                let mut r = vec![];
                if let Some(l) = def.location {
                    r.push(analysis.look_up_span(l).into());
                }
                for u in &def.usages {
                    r.push(analysis.look_up_span(*u).into());
                }

                Some(r)
            })
            .flatten())
    }
}
