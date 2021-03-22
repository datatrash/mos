use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::errors::{MosError, MosResult};
use crate::impl_notification_handler;
use crate::lsp::analysis::{from_file_uri, Analysis};
use crate::lsp::{LspContext, NotificationHandler};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    Position, PublishDiagnosticsParams, Range, Url,
};
use std::path::Path;

pub struct DidOpenTextDocumentHandler {}
pub struct DidChangeTextDocumentHandler {}
pub struct DidCloseTextDocumentHandler {}

impl_notification_handler!(DidOpenTextDocumentHandler);
impl_notification_handler!(DidChangeTextDocumentHandler);
impl_notification_handler!(DidCloseTextDocumentHandler);

impl NotificationHandler<DidOpenTextDocument> for DidOpenTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidOpenTextDocumentParams) -> MosResult<()> {
        register_document(ctx, &params.text_document.uri, &params.text_document.text);
        publish_diagnostics(ctx, &params.text_document.uri)?;
        Ok(())
    }
}

impl NotificationHandler<DidChangeTextDocument> for DidChangeTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidChangeTextDocumentParams) -> MosResult<()> {
        let text_changes = params.content_changes.first().unwrap();
        register_document(ctx, &params.text_document.uri, &text_changes.text);
        publish_diagnostics(ctx, &params.text_document.uri)?;
        Ok(())
    }
}

impl NotificationHandler<DidCloseTextDocument> for DidCloseTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidCloseTextDocumentParams) -> MosResult<()> {
        let path = Path::new(from_file_uri(&params.text_document.uri));
        ctx.parsing_source.borrow_mut().remove(&path);
        Ok(())
    }
}

fn register_document(ctx: &mut LspContext, uri: &Url, source: &str) {
    let path = Path::new(from_file_uri(uri));
    ctx.parsing_source().insert(path, source);

    log::trace!("Performing analysis, caused by: {}", path.to_str().unwrap());
    let (tree, error) = parse(path, ctx.parsing_source.clone());
    let error = match error {
        Some(e) => Some(e),
        None => match tree {
            Some(ref tree) => match codegen(tree.clone(), CodegenOptions::default()) {
                Ok(_) => None,
                Err(e) => Some(e),
            },
            _ => panic!(),
        },
    };

    if let Some(tree) = tree {
        ctx.analysis = Some(Analysis::new(tree, error));
    }
}

fn publish_diagnostics(ctx: &LspContext, uri: &Url) -> MosResult<()> {
    let analysis = ctx.analysis.as_ref().unwrap();
    let params = PublishDiagnosticsParams::new(
        uri.clone(),
        analysis
            .error
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
        MosError::Parser { location, message } | MosError::Codegen { location, message } => {
            let start = Position::new(location.begin.line as u32, location.begin.column as u32);
            let end = Position::new(location.end.line as u32, location.end.column as u32);
            let range = Range::new(start, end);
            let mut d = Diagnostic::new_simple(range, message.clone());
            d.source = Some("mos".into());
            vec![d]
        }
        MosError::Multiple(errors) => errors.iter().map(to_diagnostics).flatten().collect(),
        _ => {
            log::error!("Unknown parsing error: {:?}", error);
            vec![]
        }
    }
}
