use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::Rename;
use lsp_types::{RenameParams, TextEdit, WorkspaceEdit};

pub struct RenameHandler {}

impl_request_handler!(RenameHandler);

impl RequestHandler<Rename> for RenameHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: RenameParams,
    ) -> MosResult<Option<WorkspaceEdit>> {
        Ok(ctx
            .find_definition(&params.text_document_position)
            .map(|def| {
                ctx.analysis.as_ref().map(|a| {
                    let changes = def
                        .definition_and_usages()
                        .into_iter()
                        .map(|span| {
                            let span = a.look_up_span(span);
                            let edit = TextEdit {
                                range: span.clone().into(),
                                new_text: params.new_name.clone(),
                            };
                            (span.uri(), edit)
                        })
                        .into_group_map();
                    WorkspaceEdit {
                        changes: Some(changes),
                        document_changes: None,
                        change_annotations: None,
                    }
                })
            })
            .flatten())
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::MosResult;
    use crate::lsp::analysis::to_file_uri;
    use crate::lsp::testing::response;
    use crate::lsp::LspServer;
    use lsp_types::request::Rename;
    use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};
    use std::collections::HashMap;

    #[test]
    fn rename() -> MosResult<()> {
        let mut server = LspServer::new();
        server.did_open_text_document("test.asm", "foo: nop\nlda foo")?;
        server.rename("test.asm", Position::new(1, 4), "bar")?;

        let mut expected_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        expected_changes.insert(
            to_file_uri("test.asm"),
            vec![
                TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(0, 3)),
                    new_text: "bar".to_string(),
                },
                TextEdit {
                    range: Range::new(Position::new(1, 4), Position::new(1, 7)),
                    new_text: "bar".to_string(),
                },
            ],
        );
        assert_eq!(
            server.context.responses().pop().unwrap().result,
            response::<Rename>(Some(WorkspaceEdit {
                changes: Some(expected_changes),
                document_changes: None,
                change_annotations: None
            }))
            .result
        );

        Ok(())
    }
}
