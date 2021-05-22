use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_location, LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::{PrepareRenameRequest, Rename};
use lsp_types::{
    PrepareRenameResponse, RenameParams, TextDocumentPositionParams, TextEdit, WorkspaceEdit,
};
use mos_core::codegen::{DefinitionType, QueryTraversalStep};
use mos_core::parser::{Identifier, IdentifierPath};
use std::collections::HashMap;

pub struct PrepareRenameRequestHandler;
pub struct RenameHandler;

impl_request_handler!(PrepareRenameRequestHandler);
impl_request_handler!(RenameHandler);

/// Determines whether renaming is allowed.
/// We allow renaming of symbols, so we check whether the text document position refers to a symbol.
impl RequestHandler<PrepareRenameRequest> for PrepareRenameRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: TextDocumentPositionParams,
    ) -> MosResult<Option<PrepareRenameResponse>> {
        if let Some(codegen) = &ctx.codegen {
            if let Some(tree) = &ctx.tree {
                let file_path = &params.text_document.uri.to_file_path().unwrap();

                let source_line = params.position.line as usize;
                let source_column = params.position.character as usize;

                if let Some(source_file) = codegen.tree().files.get(file_path) {
                    let line = source_file.file.source_line(source_line);

                    // Find the beginning of the full identifier path under the cursor
                    let path_start = line[..source_column]
                        .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
                        .map(|pos| pos + 1)
                        .unwrap_or_default();

                    // Now, also try to find the identifier under the cursor
                    let start = line[..source_column]
                        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
                        .map(|pos| pos + 1)
                        .unwrap_or_default();

                    // Find the end of the identifier under the cursor
                    let end = line[source_column..]
                        .find(|c: char| !c.is_alphanumeric() && c != '_')
                        .unwrap_or_else(|| line[source_column..].len());

                    // Adjust the offset to match the full line, not just the substring
                    let end = source_column + end;

                    // This is now the identifier under the cursor
                    let id = Identifier::from(&line[start..end]);

                    if id.is_super() {
                        // We don't want to allow renaming 'super'
                        return Ok(None);
                    }

                    // This is now the full identifier path that is under the cursor
                    let path = IdentifierPath::from(&line[path_start..end]);

                    for offset in codegen.source_map().line_col_to_offsets(
                        &tree.code_map,
                        file_path.to_str().unwrap(),
                        source_line,
                        source_column,
                    ) {
                        if codegen.symbols().query(offset.scope, &path).is_some() {
                            // We're referring to an existing symbol here!
                            let range = lsp_types::Range {
                                start: lsp_types::Position {
                                    line: source_line as u32,
                                    character: start as u32,
                                },
                                end: lsp_types::Position {
                                    line: source_line as u32,
                                    character: end as u32,
                                },
                            };
                            return Ok(Some(PrepareRenameResponse::Range(range)));
                        }
                    }
                }
            }
        }

        Ok(None)
    }
}

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
                    let steps = def
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

                    // And rename it across all other paths by which it may be reached
                    // (other paths may exist due to imports)
                    for (dl, (steps, _)) in steps.iter() {
                        if let Some(QueryTraversalStep::Symbol(nx)) = steps.last() {
                            codegen.symbols_mut().rename(
                                dl.parent_scope,
                                *nx,
                                Identifier::from(params.new_name.as_str()),
                            );
                        }
                    }

                    // And reconstruct the identifiers
                    let new_paths = steps
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
    use lsp_types::request::{PrepareRenameRequest, Rename};
    use lsp_types::{Position, PrepareRenameResponse, Range, TextEdit, Url, WorkspaceEdit};
    use std::collections::HashMap;

    #[test]
    fn prepare_rename() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());

        server.did_open_text_document(
            test_root().join("main.asm"),
            "foo: { .const bar = 1 }\n{\nlda foo.bar\nlda foo.bar.super\n}",
        )?;

        // 'lda foo.*b*ar'
        server.prepare_rename(test_root().join("main.asm"), Position::new(2, 8))?;
        assert_can_rename(&server, true, 2, 8..11);

        // 'lda f*o*o.bar'
        server.prepare_rename(test_root().join("main.asm"), Position::new(2, 5))?;
        assert_can_rename(&server, true, 2, 4..7);

        // '*l*da foo.bar'
        server.prepare_rename(test_root().join("main.asm"), Position::new(2, 0))?;
        assert_can_rename(&server, false, 0, 0..0);

        // 'lda foo.ba*r*.super'
        server.prepare_rename(test_root().join("main.asm"), Position::new(3, 10))?;
        assert_can_rename(&server, true, 3, 8..11);

        // 'lda foo.bar.*s*uper'
        server.prepare_rename(test_root().join("main.asm"), Position::new(3, 12))?;
        assert_can_rename(&server, false, 0, 0..0);

        Ok(())
    }

    #[test]
    fn rename_end_of_path() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "foo: { .const bar = 1 }\n{\nlda foo.bar\n}",
        )?;
        server.prepare_rename(test_root().join("main.asm"), Position::new(2, 8))?;
        assert_can_rename(&server, true, 2, 8..11);
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
                    range: Range::new(Position::new(2, 8), Position::new(2, 11)),
                    new_text: "baz".to_string(),
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

    #[test]
    fn rename_start_of_path() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "foo: { .const bar = 1 }\n{\nlda foo.bar\n}",
        )?;
        server.prepare_rename(test_root().join("main.asm"), Position::new(2, 4))?;
        assert_can_rename(&server, true, 2, 4..7);
        server.rename(test_root().join("main.asm"), Position::new(2, 4), "foz")?;

        let mut expected_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        expected_changes.insert(
            Url::from_file_path(test_root().join("main.asm"))?,
            vec![
                TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(0, 3)),
                    new_text: "foz".to_string(),
                },
                TextEdit {
                    range: Range::new(Position::new(2, 4), Position::new(2, 7)),
                    new_text: "foz".to_string(),
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

    #[test]
    fn rename_start_of_path_in_other_file() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            ".import * from \"other.asm\"\n{\nlda foo.bar\n}",
        )?;
        server.did_open_text_document(test_root().join("other.asm"), "foo: { .const bar = 1 }")?;
        server.prepare_rename(test_root().join("main.asm"), Position::new(2, 4))?;
        assert_can_rename(&server, true, 2, 4..7);
        server.rename(test_root().join("main.asm"), Position::new(2, 4), "foz")?;

        let mut expected_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        expected_changes.insert(
            Url::from_file_path(test_root().join("main.asm"))?,
            vec![TextEdit {
                range: Range::new(Position::new(2, 4), Position::new(2, 7)),
                new_text: "foz".to_string(),
            }],
        );
        expected_changes.insert(
            Url::from_file_path(test_root().join("other.asm"))?,
            vec![TextEdit {
                range: Range::new(Position::new(0, 0), Position::new(0, 3)),
                new_text: "foz".to_string(),
            }],
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

    #[test]
    fn rename_definition() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "foo: { .const bar = 1 }\n{\nlda foo.bar\n}",
        )?;
        server.prepare_rename(test_root().join("main.asm"), Position::new(0, 0))?;
        assert_can_rename(&server, true, 0, 0..3);
        server.rename(test_root().join("main.asm"), Position::new(0, 0), "foz")?;

        let mut expected_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        expected_changes.insert(
            Url::from_file_path(test_root().join("main.asm"))?,
            vec![
                TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(0, 3)),
                    new_text: "foz".to_string(),
                },
                TextEdit {
                    range: Range::new(Position::new(2, 4), Position::new(2, 7)),
                    new_text: "foz".to_string(),
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

    fn assert_can_rename(
        server: &LspServer,
        can_rename: bool,
        line: u32,
        columns: std::ops::Range<u32>,
    ) {
        let range = lsp_types::Range {
            start: lsp_types::Position {
                line,
                character: columns.start,
            },
            end: lsp_types::Position {
                line,
                character: columns.end,
            },
        };
        let expected = if can_rename {
            response::<PrepareRenameRequest>(Some(PrepareRenameResponse::Range(range))).result
        } else {
            response::<PrepareRenameRequest>(None).result
        };

        assert_eq!(
            server.lock_context().responses().pop().unwrap().result,
            expected
        );
    }
}
