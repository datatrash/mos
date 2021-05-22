use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::Completion;
use lsp_types::{CompletionItem, CompletionParams, CompletionResponse};
use mos_core::parser::IdentifierPath;
use std::collections::HashMap;

pub struct CompletionHandler;

impl_request_handler!(CompletionHandler);

impl RequestHandler<Completion> for CompletionHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: CompletionParams,
    ) -> MosResult<Option<CompletionResponse>> {
        if let Some(codegen) = &ctx.codegen {
            if let Some(tree) = &ctx.tree {
                let path = &params
                    .text_document_position
                    .text_document
                    .uri
                    .to_file_path()
                    .unwrap();

                let source_line = params.text_document_position.position.line as usize;
                let source_column = params.text_document_position.position.character as usize;

                // Try to determine the previous characters to see if we're trying to auto-complete inside a scope
                let mut nested_scope = None;
                if let Some(source_file) = codegen.tree().files.get(path) {
                    let line = source_file.file.source_line(source_line);

                    // Only look at the line until the source_column
                    if source_column <= line.len() {
                        let (line, suffix) = line.split_at(source_column - 1);

                        // Are we autocompleting a dot?
                        if suffix.starts_with('.') {
                            // Go back until the first non-identifer or dot character to determine the scope prefix
                            let scope_at = line
                                .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
                                .map(|pos| pos + 1)
                                .unwrap_or_default();
                            let (_, scope) = line.split_at(scope_at);
                            nested_scope = Some(IdentifierPath::from(scope));
                        }
                    }
                }

                let mut offsets = codegen.source_map().line_col_to_offsets(
                    &tree.code_map,
                    path.to_str().unwrap(),
                    source_line,
                    source_column,
                );

                // If we couldn't find anything, let's try it without a column
                if offsets.is_empty() {
                    offsets = codegen.source_map().line_col_to_offsets(
                        &tree.code_map,
                        path.to_str().unwrap(),
                        source_line,
                        None,
                    );
                }

                let mut symbols = HashMap::new();
                offsets.into_iter().for_each(|offset| {
                    let scope_nx = match &nested_scope {
                        Some(ns) => codegen.symbols().try_index(offset.scope, ns),
                        None => Some(offset.scope),
                    };

                    if let Some(scope_nx) = scope_nx {
                        symbols.extend(
                            codegen
                                .symbols()
                                .visible_symbols(scope_nx, nested_scope.is_none()),
                        );
                    }
                });

                let items = symbols
                    .into_iter()
                    .filter(|(id, _)| !id.is_special())
                    .map(|(id, _)| CompletionItem {
                        label: id.to_string(),
                        ..Default::default()
                    })
                    .collect_vec();
                return Ok(Some(CompletionResponse::from(items)));
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::testing::test_root;
    use crate::lsp::LspServer;
    use lsp_types::Position;
    use mos_testing::assert_unordered_eq;

    #[test]
    fn basic_completion() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "vic: { .const border = $d020 }\n  lda ",
        )?;
        let response = server.completion(test_root().join("main.asm"), Position::new(1, 6))?;
        assert_unordered_eq(&unwrap(&response), &vec!["segments", "vic"]);
        Ok(())
    }

    #[test]
    fn basic_completion_inside_a_block() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "{\nlda \n}\nvic: { .const border = $d020 }",
        )?;
        let response = server.completion(test_root().join("main.asm"), Position::new(1, 4))?;
        assert_unordered_eq(&unwrap(&response), &vec!["segments", "vic"]);
        Ok(())
    }

    #[test]
    fn scoped_completion() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "outer: { vic: { .const border = $d020 } }\n lda outer.vic.",
        )?;
        let response = server.completion(test_root().join("main.asm"), Position::new(1, 15))?;
        assert_unordered_eq(&unwrap(&response), &vec!["border"]);
        Ok(())
    }

    #[test]
    fn scoped_completion_with_address_modifier() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "outer: { vic: { .const border = $d020 } }\n lda #outer.vic.",
        )?;
        let response = server.completion(test_root().join("main.asm"), Position::new(1, 16))?;
        assert_unordered_eq(&unwrap(&response), &vec!["border"]);
        Ok(())
    }

    fn unwrap(response: &Option<CompletionResponse>) -> Vec<&str> {
        match response {
            Some(r) => match r {
                CompletionResponse::Array(items) => {
                    items.into_iter().map(|i| i.label.as_str()).collect()
                }
                _ => panic!(),
            },
            None => vec![],
        }
    }
}
