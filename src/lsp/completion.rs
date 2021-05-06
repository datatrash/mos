use crate::core::parser::IdentifierPath;
use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::Completion;
use lsp_types::{CompletionItem, CompletionParams, CompletionResponse};
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
                if source_column > 0 {
                    if let Some(source_file) = codegen.tree().files.get(path) {
                        let line = source_file.file.source_line(source_line);

                        // Only look at the line until the source_column
                        let (line, _) = line.split_at(source_column - 1);

                        // Go back until the first non-identifier character (e.g. a space or a dot) to determine the scope
                        if let Some(scope_at) =
                            line.find(|c: char| !c.is_alphanumeric() && c != '_')
                        {
                            let (_, scope) = line.split_at(scope_at + 1);
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
    use crate::testing::assert_unordered_eq;
    use lsp_types::Position;

    #[test]
    fn basic_completion() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "vic: { .const border = $d020 }\nlda ",
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
            "vic: { .const border = $d020 }\nlda vic.",
        )?;
        let response = server.completion(test_root().join("main.asm"), Position::new(1, 8))?;
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
