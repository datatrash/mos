use crate::core::codegen::{relativize_symbols, simplify_symbols};
use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::Completion;
use lsp_types::{CompletionItem, CompletionParams, CompletionResponse};
use std::collections::{HashMap, HashSet};

pub struct CompletionHandler {}

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

                let mut offsets = codegen.source_map().line_col_to_offsets(
                    &tree.code_map,
                    path.to_str().unwrap(),
                    params.text_document_position.position.line as usize,
                    params.text_document_position.position.character as usize,
                );

                // If we couldn't find anything, let's try it without a column
                if offsets.is_empty() {
                    offsets = codegen.source_map().line_col_to_offsets(
                        &tree.code_map,
                        path.to_str().unwrap(),
                        params.text_document_position.position.line as usize,
                        None,
                    );
                }

                let mut scopes = HashMap::new();
                let offset_len = offsets.len();
                offsets.into_iter().for_each(|offset| {
                    let s = codegen.visible_symbols(&offset.scope);
                    let mut s = relativize_symbols(s, &offset.scope);
                    if offset_len == 1 {
                        // If this offset only refers to one location (usual case) then we can try and simplify the symbols a bit
                        s = simplify_symbols(s, &offset.scope);
                    }
                    scopes.extend(s);
                });

                let mut items = HashSet::new();
                for (_, symbols) in scopes {
                    let scope_items = symbols.keys().map(|id| id.to_string()).collect_vec();
                    items.extend(scope_items);
                }
                let items = items
                    .into_iter()
                    .map(|label| CompletionItem {
                        label,
                        ..Default::default()
                    })
                    .collect_vec();
                return Ok(Some(CompletionResponse::from(items)));
            }
        }

        Ok(None)
    }
}
