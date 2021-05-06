use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::core::parser::source::ParsingSource;
use crate::errors::{MosError, MosResult};
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
use std::collections::HashMap;

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
            .remove(&params.text_document.uri.to_file_path().unwrap());
        Ok(())
    }
}

fn register_document(ctx: &mut LspContext, uri: &Url, source: &str) {
    let path = uri.to_file_path().unwrap();
    ctx.parsing_source().insert(&path, source);

    ctx.tree = None;
    ctx.codegen = None;
    ctx.error = None;

    let entry = ctx.config().unwrap_or_default().build.entry;
    let entry = ctx.working_directory().join(&entry);
    if !ctx.parsing_source().exists(entry.as_ref()) {
        log::trace!(
            "`--> Entrypoint does not (yet) exist in memory or disk. Not doing any parsing."
        );
        return;
    }
    let (tree, error) = parse(entry.as_ref(), ctx.parsing_source.clone());
    ctx.tree = tree;
    ctx.error = error;
    if let Some(tree) = &ctx.tree {
        let (context, error) = codegen(tree.clone(), CodegenOptions::default());
        ctx.codegen = context;

        // Merge already existing parse errors
        let existing_error = std::mem::replace(&mut ctx.error, None);
        ctx.error = match (existing_error, error) {
            (Some(existing), Some(error)) => Some(MosError::Multiple(vec![existing, error])),
            (None, error) => error,
            (error, None) => error,
        };
    }
}

fn publish_diagnostics(ctx: &LspContext) -> MosResult<()> {
    log::trace!("Publish diagnostics");

    let mut result: HashMap<&str, Vec<Diagnostic>> = ctx
        .error
        .as_ref()
        .map(to_diagnostics)
        .unwrap_or_default()
        .into_iter()
        .into_group_map();

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
                Url::from_file_path(filename)?,
                diags,
                None, // todo: handle document version
            );
            ctx.publish_notification::<PublishDiagnostics>(params)?;
        }
    }
    Ok(())
}

fn to_diagnostics(error: &MosError) -> Vec<(&str, Diagnostic)> {
    match &error {
        MosError::Parser { location, message } | MosError::Codegen { location, message } => {
            let start = Position::new(location.begin.line as u32, location.begin.column as u32);
            let end = Position::new(location.end.line as u32, location.end.column as u32);
            let range = Range::new(start, end);
            let mut d = Diagnostic::new_simple(range, message.clone());
            d.source = Some("mos".into());
            vec![(location.file.name(), d)]
        }
        MosError::Multiple(errors) => errors.iter().map(to_diagnostics).flatten().collect(),
        _ => {
            log::error!("Unknown parsing error: {:?}", error);
            vec![]
        }
    }
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
