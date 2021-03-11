use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::errors::{MosError, MosResult};
use crate::impl_notification_handler;
use crate::lsp::analysis::{from_file_uri, Analysis};
use crate::lsp::{LspContext, NotificationHandler};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, PublishDiagnostics};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Position,
    PublishDiagnosticsParams, Range, Url,
};
use std::path::Path;

pub struct DidOpenTextDocumentHandler {}
pub struct DidChangeTextDocumentHandler {}

impl_notification_handler!(DidOpenTextDocumentHandler);
impl_notification_handler!(DidChangeTextDocumentHandler);

impl NotificationHandler<DidOpenTextDocument> for DidOpenTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidOpenTextDocumentParams) -> MosResult<()> {
        perform_analysis(ctx, &params.text_document.uri, &params.text_document.text);
        publish_diagnostics(ctx, &params.text_document.uri)?;
        Ok(())
    }
}

impl NotificationHandler<DidChangeTextDocument> for DidChangeTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidChangeTextDocumentParams) -> MosResult<()> {
        let text_changes = params.content_changes.first().unwrap();
        perform_analysis(ctx, &params.text_document.uri, &text_changes.text);
        publish_diagnostics(ctx, &params.text_document.uri)?;
        Ok(())
    }
}

fn perform_analysis(ctx: &mut LspContext, uri: &Url, source: &str) {
    let path = Path::new(from_file_uri(uri));
    log::trace!("Performing analysis, caused by: {}", path.to_str().unwrap());
    let source = source.to_string();

    let (tree, error) = parse(path, &source);
    let error = match error {
        Some(e) => Some(e),
        None => match codegen(tree.clone(), CodegenOptions::default()) {
            Ok(_) => None,
            Err(e) => Some(e),
        },
    };

    ctx.analysis = Some(Analysis::new(tree, error));
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
