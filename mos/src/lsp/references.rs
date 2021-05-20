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
        let defs = ctx
            .analysis()
            .map(|a| ctx.find_definitions(a, &params.text_document_position_params))
            .unwrap_or_default();
        if let Some((_, def)) = defs.first() {
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
                let origin = origin.map(|dl| tree.code_map.look_up_span(dl.span));

                let l = ctx.analysis().unwrap().look_up(location.span);
                let link = LocationLink {
                    origin_selection_range: origin.map(to_range),
                    target_uri: Url::from_file_path(l.file.name())?,
                    target_range: to_range(tree.code_map.look_up_span(location.span)),
                    target_selection_range: to_range(tree.code_map.look_up_span(location.span)),
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
                .map(|(_, def)| match params.context.include_declaration {
                    true => def.definition_and_usages(),
                    false => def.usages(),
                })
                .flatten()
                .map(|dl| to_location(ctx.analysis().unwrap().look_up(dl.span)))
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
        let defs = ctx
            .analysis()
            .map(|a| ctx.find_definitions(a, &params.text_document_position_params))
            .unwrap_or_default();
        let highlights = defs
            .into_iter()
            .map(|(_, def)| {
                def.definition_and_usages()
                    .into_iter()
                    .filter_map(|dl| {
                        let loc = to_location(ctx.analysis().unwrap().look_up(dl.span));
                        if loc.uri == params.text_document_position_params.text_document.uri {
                            Some(DocumentHighlight {
                                range: loc.range,
                                kind: None,
                            })
                        } else {
                            None
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
        assert_eq!(location.origin_selection_range, Some(range(1, 8, 1, 14)));
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
        assert_eq!(location.origin_selection_range, Some(range(0, 8, 0, 14)));
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
