use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_location, LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::Rename;
use lsp_types::{RenameParams, TextEdit, WorkspaceEdit};

pub struct RenameHandler;

impl_request_handler!(RenameHandler);

impl RequestHandler<Rename> for RenameHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: RenameParams,
    ) -> MosResult<Option<WorkspaceEdit>> {
        let edit = ctx
            .find_definitions(&params.text_document_position)
            .first()
            .map(|def| {
                ctx.analysis().as_ref().map(|a| {
                    let changes = def
                        .definition_and_usages()
                        .into_iter()
                        .map(|i| {
                            let loc = to_location(a.look_up(*i));
                            let edit = TextEdit {
                                range: loc.range,
                                new_text: params.new_name.clone(),
                            };
                            (loc.uri, edit)
                        })
                        .into_group_map();
                    WorkspaceEdit {
                        changes: Some(changes),
                        document_changes: None,
                        change_annotations: None,
                    }
                })
            })
            .flatten();
        Ok(edit)
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::MosResult;
    use crate::lsp::testing::{response, test_root};
    use crate::lsp::{LspContext, LspServer};
    use lsp_types::request::Rename;
    use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};
    use std::collections::HashMap;

    #[test]
    fn rename() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(test_root().join("main.asm"), "foo: {\nlda foo\n}")?;
        server.rename(test_root().join("main.asm"), Position::new(1, 4), "bar")?;

        let mut expected_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        expected_changes.insert(
            Url::from_file_path(test_root().join("main.asm"))?,
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
            server.lock_context().responses().pop().unwrap().result,
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
