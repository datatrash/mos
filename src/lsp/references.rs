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
        let def = ctx.find_definition(&params.text_document_position_params);
        if let Some(def) = def {
            if let Some(location) = &def.location {
                let l = ctx.analysis().unwrap().look_up(*location);
                let loc: lsp_types::Location = l.into();
                return Ok(Some(loc.into()));
            }
        }

        Ok(None)
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
                def.definition_and_usages()
                    .into_iter()
                    .map(|i| ctx.analysis().unwrap().look_up(*i).into())
                    .collect()
            }))
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
                def.definition_and_usages()
                    .into_iter()
                    .map(|i| {
                        let loc: lsp_types::Location = ctx.analysis().unwrap().look_up(*i).into();
                        DocumentHighlight {
                            range: loc.range,
                            kind: None,
                        }
                    })
                    .collect()
            }))
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::MosResult;
    use crate::lsp::testing::range;
    use crate::lsp::{path_from_uri, LspServer};
    use lsp_types::{GotoDefinitionResponse, Position};

    #[test]
    fn can_go_to_reference_in_other_file() -> MosResult<()> {
        let mut server = LspServer::new();
        server.did_open_text_document("/bar.asm", "foo: nop\n.export foo")?;
        server.did_open_text_document("/main.asm", "lda foo\n.import foo from \"bar.asm\"")?;
        let response = server.go_to_definition("/main.asm", Position::new(0, 4))?;
        let location = match response {
            GotoDefinitionResponse::Scalar(location) => location,
            _ => panic!(),
        };
        assert_eq!(path_from_uri(&location.uri).to_string_lossy(), "/bar.asm");
        assert_eq!(location.range, range(0, 0, 0, 3));
        Ok(())
    }
}
