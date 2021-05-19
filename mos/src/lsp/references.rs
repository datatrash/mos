use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_line_col, to_location, to_range, LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::{DocumentHighlightRequest, GotoDefinition, References};
use lsp_types::{
    DocumentHighlight, DocumentHighlightParams, GotoDefinitionParams, GotoDefinitionResponse,
    Location, LocationLink, ReferenceParams, Url,
};
use mos_core::codegen::DefinitionType;

pub struct GoToDefinitionHandler;
pub struct FindReferencesHandler;
pub struct DocumentHighlightRequestHandler;

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
                let tree = ctx.tree.as_ref().unwrap();
                let origin = def.try_get_usage_containing(
                    &tree,
                    &params
                        .text_document_position_params
                        .text_document
                        .uri
                        .to_file_path()?,
                    to_line_col(&params.text_document_position_params.position),
                );
                let origin = origin.map(|span| tree.code_map.look_up_span(*span));

                let l = ctx.analysis().unwrap().look_up(*location);
                let link = LocationLink {
                    origin_selection_range: origin.map(to_range),
                    target_uri: Url::from_file_path(l.file.name())?,
                    target_range: to_range(tree.code_map.look_up_span(*location)),
                    target_selection_range: to_range(tree.code_map.look_up_span(*location)),
                };
                return Ok(Some(vec![link].into()));
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
                params
                    .text_document_position
                    .text_document
                    .uri
                    .to_file_path()?,
                to_line_col(&params.text_document_position.position),
                |ty| matches!(ty, DefinitionType::Symbol(_)),
            );

            let locations = defs
                .into_iter()
                .map(|def| match params.context.include_declaration {
                    true => def.definition_and_usages(),
                    false => def.usages(),
                })
                .flatten()
                .map(|span| to_location(ctx.analysis().unwrap().look_up(*span)))
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
                        let loc = to_location(ctx.analysis().unwrap().look_up(*i));
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
    use crate::lsp::testing::{range, test_root};
    use crate::lsp::{LspContext, LspServer};
    use itertools::Itertools;
    use lsp_types::{GotoDefinitionResponse, Location, Position, Url};

    #[test]
    fn can_go_to_const_reference() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "vic: { .const border = $d020 }\nlda vic.border",
        )?;
        // Look up 'vic.border'
        let response =
            server.go_to_definition(test_root().join("main.asm"), Position::new(1, 9))?;
        let location = match &response {
            GotoDefinitionResponse::Link(links) => links.first().unwrap(),
            _ => panic!(),
        };
        assert_eq!(
            location.target_uri.to_file_path()?.to_string_lossy(),
            test_root().join("main.asm").to_string_lossy()
        );
        assert_eq!(location.target_range, range(0, 14, 0, 20));
        assert_eq!(location.target_selection_range, range(0, 14, 0, 20));
        assert_eq!(location.origin_selection_range, Some(range(1, 4, 1, 14)));
        Ok(())
    }

    #[test]
    fn can_go_to_const_reference_in_other_file() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("bar.asm"),
            "vic: { .const border = $d020 }",
        )?;
        server.did_open_text_document(
            test_root().join("main.asm"),
            "lda vic.border\n.import * from \"bar.asm\"",
        )?;
        // Look up 'vic.border'
        let response =
            server.go_to_definition(test_root().join("main.asm"), Position::new(0, 9))?;
        let location = match &response {
            GotoDefinitionResponse::Link(links) => links.first().unwrap(),
            _ => panic!(),
        };
        assert_eq!(
            location.target_uri.to_file_path()?.to_string_lossy(),
            test_root().join("bar.asm").to_string_lossy()
        );
        assert_eq!(location.target_range, range(0, 14, 0, 20));
        assert_eq!(location.target_selection_range, range(0, 14, 0, 20));
        assert_eq!(location.origin_selection_range, Some(range(0, 4, 0, 14)));
        Ok(())
    }

    #[test]
    fn find_all_references() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(test_root().join("bar.asm"), "foo: nop")?;
        server.did_open_text_document(test_root().join("main.asm"), "lda f1\nlda f2\n.import foo as f1 from \"bar.asm\"\n.import foo as f2 from \"bar.asm\"")?;
        let response =
            server.find_references(test_root().join("main.asm"), Position::new(0, 4), false)?;
        let response = response
            .unwrap()
            .into_iter()
            .sorted_by_key(|loc| loc.range.start)
            .collect_vec();
        assert_eq!(
            response,
            vec![
                Location {
                    uri: Url::from_file_path(test_root().join("main.asm"))?,
                    range: range(0, 4, 0, 6)
                },
                Location {
                    uri: Url::from_file_path(test_root().join("main.asm"))?,
                    range: range(2, 8, 2, 17)
                },
            ]
        );
        Ok(())
    }
}
