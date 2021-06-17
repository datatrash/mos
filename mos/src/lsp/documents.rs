use crate::diagnostic_emitter::MosResult;
use crate::impl_notification_handler;
use crate::lsp::{LspContext, NotificationHandler};
use itertools::Itertools;
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    Position, PublishDiagnosticsParams, Range, Url,
};
use mos_core::codegen::{codegen, CodegenOptions};
use mos_core::errors::Diagnostics;
use mos_core::parser::parse;
use mos_core::parser::source::ParsingSource;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub struct DidOpenTextDocumentHandler;
pub struct DidChangeTextDocumentHandler;
pub struct DidCloseTextDocumentHandler;

impl_notification_handler!(DidOpenTextDocumentHandler);
impl_notification_handler!(DidChangeTextDocumentHandler);
impl_notification_handler!(DidCloseTextDocumentHandler);

impl NotificationHandler<DidOpenTextDocument> for DidOpenTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidOpenTextDocumentParams) -> MosResult<()> {
        register_document(ctx, &params.text_document.uri, &params.text_document.text);
        publish_diagnostics(ctx)?;
        Ok(())
    }
}

impl NotificationHandler<DidChangeTextDocument> for DidChangeTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidChangeTextDocumentParams) -> MosResult<()> {
        let text_changes = params.content_changes.first().unwrap();
        register_document(ctx, &params.text_document.uri, &text_changes.text);
        publish_diagnostics(ctx)?;
        Ok(())
    }
}

impl NotificationHandler<DidCloseTextDocument> for DidCloseTextDocumentHandler {
    fn handle(&self, ctx: &mut LspContext, params: DidCloseTextDocumentParams) -> MosResult<()> {
        ctx.parsing_source()
            .lock()
            .unwrap()
            .remove(&params.text_document.uri.to_file_path().unwrap());
        Ok(())
    }
}

fn register_document(ctx: &mut LspContext, uri: &Url, source: &str) {
    let path = uri.to_file_path().unwrap();
    ctx.parsing_source().lock().unwrap().insert(&path, source);

    ctx.tree = None;
    ctx.codegen = None;
    ctx.error = Diagnostics::default();

    let entry = ctx.config().unwrap_or_default().build.entry;
    let entry = ctx.working_directory().join(&entry);
    if !ctx.parsing_source().lock().unwrap().exists(entry.as_ref()) {
        log::trace!(
            "`--> Entrypoint does not (yet) exist in memory or disk. Not doing any parsing."
        );
        return;
    }
    let (tree, error) = parse(entry.as_ref(), ctx.parsing_source.clone());
    ctx.tree = tree;
    ctx.error = error;
    if let Some(tree) = &ctx.tree {
        let (context, error) = codegen(
            tree.clone(),
            CodegenOptions {
                enable_greedy_analysis: true,
                ..Default::default()
            },
        );
        ctx.codegen = context.map(|c| Arc::new(Mutex::new(c)));

        // Merge already existing parse errors
        ctx.error.extend(error);
    }
}

fn publish_diagnostics(ctx: &LspContext) -> MosResult<()> {
    log::trace!("Publish diagnostics");

    let mut result: HashMap<String, Vec<Diagnostic>> =
        to_diagnostics(&ctx.error).into_iter().into_group_map();

    // Grab all the files in the project
    if let Some(tree) = ctx.tree.as_ref() {
        let filenames = tree
            .code_map
            .files()
            .iter()
            .map(|file| file.name().to_string())
            .collect_vec();

        // Publish errors (or no errors!) for every file
        for filename in filenames {
            let diags = result.remove(filename.as_str()).unwrap_or_default();
            let params = PublishDiagnosticsParams::new(
                Url::from_file_path(filename).unwrap(),
                diags,
                None, // todo: handle document version
            );
            ctx.publish_notification::<PublishDiagnostics>(params)?;
        }
    }
    Ok(())
}

fn to_diagnostics(error: &Diagnostics) -> Vec<(String, Diagnostic)> {
    error
        .iter()
        .filter_map(|diag| {
            diag.labels
                .first()
                .map(|label| {
                    error
                        .code_map()
                        .as_ref()
                        .unwrap()
                        .look_up_span(label.file_id)
                })
                .map(|location| {
                    let start =
                        Position::new(location.begin.line as u32, location.begin.column as u32);
                    let end = Position::new(location.end.line as u32, location.end.column as u32);
                    let range = Range::new(start, end);
                    let mut d = Diagnostic::new_simple(range, diag.message.clone());
                    d.source = Some("mos".into());
                    (location.file.name().to_string(), d)
                })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    #[cfg(windows)]
    #[test]
    fn can_parse_windows_uris() {
        use lsp_types::Url;
        let url = Url::parse("file:///g%3A/code/mos/vscode/test-workspace/main.asm").unwrap();
        let _ = url.to_file_path().unwrap();
    }
}
