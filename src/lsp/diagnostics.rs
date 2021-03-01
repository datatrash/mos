use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::errors::{MosError, MosResult};
use crate::impl_notification_handler;
use crate::lsp::{Analysis, LspContext, NotificationHandler};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    Position, PublishDiagnosticsParams, Range, Url,
};
use std::path::PathBuf;

pub struct DidOpen {}
pub struct DidChange {}
pub struct DidClose {}

impl_notification_handler!(DidOpen);
impl_notification_handler!(DidChange);
impl_notification_handler!(DidClose);

impl NotificationHandler<DidOpenTextDocument> for DidOpen {
    fn handle(&self, ctx: &mut LspContext, params: DidOpenTextDocumentParams) -> MosResult<()> {
        insert_parsed_document(ctx, &params.text_document.uri, &params.text_document.text);
        publish_diagnostics(ctx, &params.text_document.uri)?;
        Ok(())
    }
}

impl NotificationHandler<DidChangeTextDocument> for DidChange {
    fn handle(&self, ctx: &mut LspContext, params: DidChangeTextDocumentParams) -> MosResult<()> {
        let text_changes = params.content_changes.first().unwrap();
        insert_parsed_document(ctx, &params.text_document.uri, &text_changes.text);
        publish_diagnostics(ctx, &params.text_document.uri)?;
        Ok(())
    }
}

impl NotificationHandler<DidCloseTextDocument> for DidClose {
    fn handle(&self, ctx: &mut LspContext, params: DidCloseTextDocumentParams) -> MosResult<()> {
        ctx.documents.remove(&params.text_document.uri);
        Ok(())
    }
}

fn insert_parsed_document(ctx: &mut LspContext, uri: &Url, source: &str) {
    let path = PathBuf::from(uri.path());
    let source = source.to_string();

    let mut analysis = Analysis::new();
    let (tree, error) = parse(path.as_path(), &source);
    analysis.tree = Some(tree.clone());
    match error {
        Some(e) => analysis.error = Some(e),
        None => match codegen(tree, CodegenOptions::default()) {
            Ok(_) => {}
            Err(e) => {
                analysis.error = Some(e);
            }
        },
    }

    ctx.documents.insert(uri.clone(), analysis);
}

fn publish_diagnostics(ctx: &LspContext, uri: &Url) -> MosResult<()> {
    let ast = ctx.documents.get(uri).unwrap();
    let params = PublishDiagnosticsParams::new(
        uri.clone(),
        ast.error
            .as_ref()
            .map(|e| to_diagnostics(e))
            .unwrap_or_default(),
        None, // todo: handle document version
    );
    ctx.publish_notification::<PublishDiagnostics>(params)?;
    Ok(())
}

fn to_diagnostics(error: &MosError) -> Vec<Diagnostic> {
    match &error {
        MosError::Parser {
            tree,
            span,
            message,
        }
        | MosError::Codegen {
            tree,
            span,
            message,
        } => {
            let l = tree.code_map().look_up_span(*span);
            let start = Position::new(l.begin.line as u32, l.begin.column as u32);
            let end = Position::new(l.end.line as u32, l.end.column as u32);
            let range = Range::new(start, end);
            let d = Diagnostic::new_simple(range, message.clone());
            vec![d]
        }
        MosError::Multiple(errors) => errors.iter().map(to_diagnostics).flatten().collect(),
        _ => {
            eprintln!("Unknown parsing error: {:?}", error);
            vec![]
        }
    }
}
