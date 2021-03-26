use crate::core::codegen::DefinitionType;
use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{path_from_uri, LspContext, RequestHandler};
use itertools::Itertools;
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
        let def = ctx.find_definitions(&params.text_document_position_params);
        if let Some(def) = def.first() {
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
        if let Some(analysis) = ctx.analysis() {
            let defs = analysis.find_filter(
                path_from_uri(&params.text_document_position.text_document.uri),
                params.text_document_position.position,
                |ty| matches!(ty, DefinitionType::Symbol(_)),
            );

            let locations = defs
                .into_iter()
                .map(|def| match params.context.include_declaration {
                    true => def.definition_and_usages(),
                    false => def.usages(),
                })
                .flatten()
                .map(|span| ctx.analysis().unwrap().look_up(*span).into())
                .collect_vec();

            Ok(Some(locations))
        } else {
            Ok(None)
        }
    }
}

impl RequestHandler<DocumentHighlightRequest> for DocumentHighlightRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: DocumentHighlightParams,
    ) -> MosResult<Option<Vec<DocumentHighlight>>> {
        let highlights = ctx
            .find_definitions(&params.text_document_position_params)
            .into_iter()
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
                    .collect_vec()
            })
            .flatten()
            .collect();

        Ok(Some(highlights))
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::MosResult;
    use crate::lsp::testing::range;
    use crate::lsp::{path_from_uri, path_to_uri, LspServer};
    use crate::testing::enable_default_tracing;
    use itertools::Itertools;
    use lsp_types::{GotoDefinitionResponse, Location, Position};

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

    #[test]
    fn find_all_references() -> MosResult<()> {
        enable_default_tracing();
        let mut server = LspServer::new();
        server.did_open_text_document("/bar.asm", "foo: nop\n.export foo")?;
        server.did_open_text_document("/main.asm", "lda f1\nlda f2\n.import foo as f1 from \"bar.asm\"\n.import foo as f2 from \"bar.asm\"")?;
        let response = server.find_references("/bar.asm", Position::new(0, 0), false)?;
        let response = response
            .unwrap()
            .into_iter()
            .sorted_by_key(|loc| loc.range.start)
            .collect_vec();
        assert_eq!(
            response,
            vec![
                Location {
                    uri: path_to_uri("/main.asm"),
                    range: range(0, 4, 0, 6)
                },
                Location {
                    uri: path_to_uri("/main.asm"),
                    range: range(1, 4, 1, 6)
                },
            ]
        );
        Ok(())
    }
}
