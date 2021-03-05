use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_lsp_location, LspContext, RequestHandler};
use lsp_types::request::GotoDefinition;
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse};

pub struct GoToDefinitionHandler {}

impl_request_handler!(GoToDefinitionHandler);

impl RequestHandler<GotoDefinition> for GoToDefinitionHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: GotoDefinitionParams,
    ) -> MosResult<Option<GotoDefinitionResponse>> {
        match ctx
            .documents
            .get(&params.text_document_position_params.text_document.uri)
        {
            Some(analysis) => match analysis.find(params.text_document_position_params.position) {
                Some((_, location)) => Ok(Some(to_lsp_location(location).into())),
                None => Ok(None),
            },
            _ => Ok(None),
        }
    }
}
