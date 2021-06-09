use crate::diagnostic_emitter::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_range, LspContext, RequestHandler};
use lsp_types::request::{DocumentSymbolRequest, WorkspaceSymbol};
use lsp_types::{
    DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Location, SymbolInformation,
    SymbolKind, Url, WorkspaceSymbolParams,
};
use mos_core::codegen::CodegenContext;
use mos_core::parser::code_map::Span;
use mos_core::parser::{Identifier, ParseTree, Token};
use std::sync::{Arc, Mutex};

pub struct DocumentSymbolRequestHandler;
pub struct WorkspaceSymbolHandler;

impl_request_handler!(DocumentSymbolRequestHandler);
impl_request_handler!(WorkspaceSymbolHandler);

impl RequestHandler<DocumentSymbolRequest> for DocumentSymbolRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: DocumentSymbolParams,
    ) -> MosResult<Option<DocumentSymbolResponse>> {
        if let Some(tree) = &ctx.tree {
            let path = params.text_document.uri.to_file_path().unwrap();
            if let Some(file) = tree.try_get_file(&path) {
                if let Some(codegen) = ctx.codegen() {
                    let emitter = DocSymEmitter {
                        tree,
                        codegen,
                        filename: file.file.name(),
                        recurse: false,
                    };
                    let docsyms = emitter.emit_document_symbols(&file.tokens, None);
                    let document_symbols = docsyms
                        .into_iter()
                        .map(|ds| ds.into_document_symbol(tree))
                        .collect();
                    return Ok(Some(DocumentSymbolResponse::Nested(document_symbols)));
                }
            }
        }
        Ok(None)
    }
}

impl RequestHandler<WorkspaceSymbol> for WorkspaceSymbolHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: WorkspaceSymbolParams,
    ) -> MosResult<Option<Vec<SymbolInformation>>> {
        if let Some(tree) = &ctx.tree {
            if let Some(file) = tree.try_get_file(&tree.main_file) {
                if let Some(codegen) = ctx.codegen() {
                    let emitter = DocSymEmitter {
                        tree,
                        codegen,
                        filename: file.file.name(),
                        recurse: true,
                    };
                    let docsyms = emitter.emit_document_symbols(&file.tokens, None);
                    let workspace_symbols = docsyms
                        .into_iter()
                        .filter(|ds| {
                            params.query.is_empty()
                                || ds.name.to_string().starts_with(&params.query)
                        })
                        .map(|ds| ds.into_workspace_symbol(tree))
                        .collect();
                    return Ok(Some(workspace_symbols));
                }
            }
        }
        Ok(None)
    }
}

struct DocSym {
    uri: Url,
    name: Identifier,
    parent: Option<Identifier>,
    span: Span,
    kind: SymbolKind,
    children: Vec<DocSym>,
}

#[allow(deprecated)]
impl DocSym {
    fn into_document_symbol(self, tree: &Arc<ParseTree>) -> DocumentSymbol {
        let range = to_range(tree.code_map.look_up_span(self.span));

        DocumentSymbol {
            name: self.name.to_string(),
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

    fn into_workspace_symbol(self, tree: &Arc<ParseTree>) -> SymbolInformation {
        let range = to_range(tree.code_map.look_up_span(self.span));

        SymbolInformation {
            name: self.name.to_string(),
            kind: self.kind,
            tags: None,
            deprecated: None,
            location: Location {
                uri: self.uri,
                range,
            },
            container_name: self.parent.map(|p| p.to_string()),
        }
    }
}

struct DocSymEmitter<'a> {
    tree: &'a Arc<ParseTree>,
    codegen: Arc<Mutex<CodegenContext>>,
    filename: &'a str,
    recurse: bool,
}

impl<'a> DocSymEmitter<'a> {
    fn emit_document_symbols(&self, ast: &'a [Token], parent: Option<&Identifier>) -> Vec<DocSym> {
        ast.iter()
            .map(|token| self.emit_document_symbol(token, parent))
            .flatten()
            .collect()
    }

    fn emit_document_symbol(&self, token: &Token, parent: Option<&Identifier>) -> Vec<DocSym> {
        match token {
            Token::Braces { block, .. }
            | Token::Config(block)
            | Token::Loop { block, .. }
            | Token::Test { block, .. } => self.emit_document_symbols(&block.inner, None),
            Token::MacroDefinition { id, block, .. } => {
                self.emit_document_symbols(&block.inner, Some(&id.data))
            }
            Token::If { if_, else_, .. } => {
                let mut result = self.emit_document_symbols(&if_.inner, None);
                if let Some(e) = else_ {
                    result.extend(self.emit_document_symbols(&e.inner, None));
                }
                result
            }
            Token::Import {
                resolved_path,
                block,
                ..
            } => {
                let mut result = vec![];
                if self.recurse {
                    if let Some(file) = self.tree.try_get_file(&resolved_path) {
                        let emitter = DocSymEmitter {
                            tree: self.tree,
                            codegen: self.codegen.clone(),
                            filename: file.file.name(),
                            recurse: self.recurse,
                        };
                        result.extend(emitter.emit_document_symbols(&file.tokens, None));
                    }
                }

                if let Some(b) = block {
                    result.extend(self.emit_document_symbols(&b.inner, None));
                }
                result
            }
            Token::Label { id, block, .. } => {
                vec![DocSym {
                    uri: Url::from_file_path(self.filename).unwrap(),
                    name: id.data.clone(),
                    parent: parent.cloned(),
                    span: id.span,
                    kind: SymbolKind::Class,
                    children: block
                        .as_ref()
                        .map(|b| self.emit_document_symbols(&b.inner, Some(&id.data)))
                        .unwrap_or_default(),
                }]
            }
            Token::Segment { id, block, .. } => {
                if let Some(b) = block {
                    if let Ok(Some(symbol_id)) = self
                        .codegen
                        .lock()
                        .unwrap()
                        .evaluate_expression_as_string(&id, false)
                    {
                        self.emit_document_symbols(&b.inner, Some(&Identifier::new(symbol_id)))
                    } else {
                        vec![]
                    }
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }
}
