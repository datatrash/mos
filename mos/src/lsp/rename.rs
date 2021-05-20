use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_location, LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::Rename;
use lsp_types::{RenameParams, TextEdit, WorkspaceEdit};
use mos_core::codegen::DefinitionType;
use mos_core::parser::{Identifier, IdentifierPath};
use std::collections::HashMap;

pub struct RenameHandler;

impl_request_handler!(RenameHandler);

impl RequestHandler<Rename> for RenameHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: RenameParams,
    ) -> MosResult<Option<WorkspaceEdit>> {
        let codegen = match ctx.codegen() {
            Some(cg) => cg,
            None => return Ok(None),
        };
        let mut defs = ctx.find_definitions(codegen.analysis(), &params.text_document_position);
        if defs.is_empty() {
            return Ok(None);
        }
        let (def_ty, def) = defs.remove(0);
        let (def_ty, def) = (def_ty.clone(), def.clone());
        match def_ty {
            DefinitionType::Filename(_) => Ok(None),
            DefinitionType::Symbol(def_symbol_nx) => {
                if let Some(location) = &def.location {
                    let codegen = ctx.codegen_mut().unwrap();

                    // First, determine all the query_indices for every usage
                    let query_indices = def
                        .usages()
                        .into_iter()
                        .map(|dl| {
                            let sl = codegen.analysis().look_up(dl.span);
                            let path = IdentifierPath::from(sl.file.source_slice(dl.span));
                            (
                                dl,
                                (
                                    codegen
                                        .symbols()
                                        .query_traversal_steps(dl.parent_scope, &path),
                                    path,
                                ),
                            )
                        })
                        .collect::<HashMap<_, _>>();

                    // Now, rename the actual symbol
                    codegen.symbols_mut().rename(
                        location.parent_scope,
                        def_symbol_nx,
                        Identifier::from(params.new_name.as_str()),
                    );

                    // And reconstruct the identifiers
                    let new_paths = query_indices
                        .into_iter()
                        .filter_map(|(dl, (query_traversal_steps, old_path))| {
                            let include_super = old_path.contains_super();
                            codegen
                                .symbols()
                                .query_steps_to_path(
                                    dl.parent_scope,
                                    &query_traversal_steps,
                                    include_super,
                                )
                                .map(|path| (dl, path))
                        })
                        .collect::<HashMap<_, _>>();

                    let changes = def
                        .definition_and_usages()
                        .into_iter()
                        .map(|dl| {
                            let loc = to_location(codegen.analysis().look_up(dl.span));

                            // We either grab a renamed usage, or we fallback to the name specified by the user for the source definition
                            let new_text = match new_paths.get(dl) {
                                Some(new_path) => new_path.to_string(),
                                None => params.new_name.clone(),
                            };

                            let edit = TextEdit {
                                range: loc.range,
                                new_text,
                            };
                            (loc.uri, edit)
                        })
                        .into_group_map();
                    return Ok(Some(WorkspaceEdit {
                        changes: Some(changes),
                        document_changes: None,
                        change_annotations: None,
                    }));
                }

                Ok(None)
            }
        }
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
    fn rename_end_of_path() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "foo: { .const bar = 1 }\n{\nlda foo.bar\n}",
        )?;
        server.rename(test_root().join("main.asm"), Position::new(2, 8), "baz")?;

        let mut expected_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        expected_changes.insert(
            Url::from_file_path(test_root().join("main.asm"))?,
            vec![
                TextEdit {
                    range: Range::new(Position::new(0, 14), Position::new(0, 17)),
                    new_text: "baz".to_string(),
                },
                TextEdit {
                    range: Range::new(Position::new(2, 4), Position::new(2, 11)),
                    new_text: "foo.baz".to_string(),
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
