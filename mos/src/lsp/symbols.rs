use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_range, LspContext, RequestHandler};
use lsp_types::request::DocumentSymbolRequest;
use lsp_types::{DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, SymbolKind};
use mos_core::parser::code_map::Span;
use mos_core::parser::{ParseTree, Token};
use std::sync::Arc;

pub struct DocumentSymbolRequestHandler;

impl_request_handler!(DocumentSymbolRequestHandler);

impl RequestHandler<DocumentSymbolRequest> for DocumentSymbolRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: DocumentSymbolParams,
    ) -> MosResult<Option<DocumentSymbolResponse>> {
        if let Some(tree) = &ctx.tree {
            let path = params.text_document.uri.to_file_path()?;
            if let Some(file) = tree.try_get_file(&path) {
                let docsyms = emit_document_symbols(&file.tokens);
                let document_symbols = docsyms
                    .into_iter()
                    .map(|ds| ds.into_document_symbol(tree))
                    .collect();
                return Ok(Some(DocumentSymbolResponse::Nested(document_symbols)));
            }
        }
        Ok(None)
    }
}

struct DocSym {
    name: String,
    span: Span,
    kind: SymbolKind,
    children: Vec<DocSym>,
}

impl DocSym {
    #[allow(deprecated)]
    fn into_document_symbol(self, tree: &Arc<ParseTree>) -> DocumentSymbol {
        let range = to_range(tree.code_map.look_up_span(self.span));

        DocumentSymbol {
            name: self.name,
            detail: None,
            kind: self.kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: Some(
                self.children
                    .into_iter()
                    .map(|ds| ds.into_document_symbol(tree))
                    .collect(),
            ),
        }
    }
}

fn emit_document_symbols(ast: &[Token]) -> Vec<DocSym> {
    ast.iter()
        .map(|tok| emit_document_symbol(tok))
        .flatten()
        .collect()
}

fn emit_document_symbol(token: &Token) -> Vec<DocSym> {
    match token {
        Token::Braces { block, .. }
        | Token::Config(block)
        | Token::Loop { block, .. }
        | Token::MacroDefinition { block, .. }
        | Token::Test { block, .. } => emit_document_symbols(&block.inner),
        Token::If { if_, else_, .. } => {
            let mut result = emit_document_symbols(&if_.inner);
            if let Some(e) = else_ {
                result.extend(emit_document_symbols(&e.inner));
            }
            result
        }
        Token::Import { block, .. } | Token::Segment { block, .. } => {
            if let Some(b) = block {
                emit_document_symbols(&b.inner)
            } else {
                vec![]
            }
        }
        Token::Label { id, block, .. } => {
            vec![DocSym {
                name: id.data.to_string(),
                span: id.span,
                kind: SymbolKind::Class,
                children: block
                    .as_ref()
                    .map(|b| emit_document_symbols(&b.inner))
                    .unwrap_or_default(),
            }]
        }
        _ => vec![],
    }
}
