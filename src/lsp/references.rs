use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use lsp_types::request::{DocumentHighlightRequest, GotoDefinition, References};
use lsp_types::{
    DocumentHighlight, DocumentHighlightParams, GotoDefinitionParams, GotoDefinitionResponse,
    Location, ReferenceParams,
};

pub struct GoToDefinitionHandler {}
pub struct FindReferencesHandler {}
pub struct DocumentHighlightRequestHandler {}

impl_request_handler!(GoToDefinitionHandler);
impl_request_handler!(FindReferencesHandler);
impl_request_handler!(DocumentHighlightRequestHandler);

impl RequestHandler<GotoDefinition> for GoToDefinitionHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: GotoDefinitionParams,
    ) -> MosResult<Option<GotoDefinitionResponse>> {
        Ok(ctx
            .find_definition(&params.text_document_position_params)
            .map(|def| {
                ctx.analysis.as_ref().map(|a| {
                    let l = a.look_up_span(def.location.unwrap());
                    Location::from(l).into()
                })
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
        Ok(ctx
            .find_definition(&params.text_document_position)
            .map(|def| {
                ctx.analysis.as_ref().map(|a| {
                    def.definition_and_usages()
                        .into_iter()
                        .map(|span| a.look_up_span(span).into())
                        .collect()
                })
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
        Ok(ctx
            .find_definition(&params.text_document_position_params)
            .map(|def| {
                ctx.analysis.as_ref().map(|a| {
                    def.definition_and_usages()
                        .into_iter()
                        .map(|span| a.look_up_span(span).into())
                        .collect()
                })
            })
            .flatten())
    }
}
