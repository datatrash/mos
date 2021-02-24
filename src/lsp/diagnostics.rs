use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::errors::{MosError, MosResult};
use crate::impl_notification_handler;
use crate::lsp::{LspContext, NotificationHandler, TypedNotificationHandler};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, PublishDiagnostics};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncKind,
};

pub struct DidOpen {}
pub struct DidChange {}

impl_notification_handler!(DidOpen);
impl_notification_handler!(DidChange);

impl TypedNotificationHandler<DidOpenTextDocument> for DidOpen {
    fn customize(&self, caps: &mut ServerCapabilities) {
        caps.text_document_sync = Some(TextDocumentSyncKind::Full.into());
    }

    fn handle(&self, ctx: &mut LspContext, params: DidOpenTextDocumentParams) -> MosResult<()> {
        ctx.documents.insert(
            params.text_document.uri.as_ref().to_string(),
            params.text_document.text.clone(),
        );
        publish_diagnostics(
            ctx,
            params.text_document.uri.as_ref(),
            &params.text_document.text,
        )?;
        Ok(())
    }
}

impl TypedNotificationHandler<DidChangeTextDocument> for DidChange {
    fn handle(&self, ctx: &mut LspContext, params: DidChangeTextDocumentParams) -> MosResult<()> {
        let text_changes = params.content_changes.first().unwrap();
        ctx.documents.insert(
            params.text_document.uri.as_ref().to_string(),
            text_changes.text.clone(),
        );
        publish_diagnostics(ctx, params.text_document.uri.as_ref(), &text_changes.text)?;
        Ok(())
    }
}

fn publish_diagnostics(ctx: &LspContext, uri: &str, source: &str) -> MosResult<()> {
    let ast = parse(uri.as_ref(), source);
    let diagnostics = match ast {
        Ok(ast) => {
            let code = codegen(ast, CodegenOptions::default());
            match code {
                Ok(_) => vec![],
                Err(e) => to_diagnostics(e),
            }
        }
        Err(e) => to_diagnostics(e),
    };

    let params = PublishDiagnosticsParams::new(
        uri.parse().unwrap(),
        diagnostics,
        None, // todo: handle document version
    );
    ctx.publish_notification::<PublishDiagnostics>(params)?;
    Ok(())
}

fn to_diagnostics(e: MosError) -> Vec<Diagnostic> {
    match e {
        MosError::Parser {
            ref location,
            ref message,
        }
        | MosError::Codegen {
            ref location,
            ref message,
        } => match location {
            Some(l) => {
                let start = Position::new(l.line - 1, l.column - 1);
                let end = Position::new(l.line - 1, l.column - 1 + l.fragment.len() as u32);
                let range = Range::new(start, end);
                let d = Diagnostic::new_simple(range, message.clone());
                vec![d]
            }
            None => {
                eprintln!("Parser error without a location: {:?}", e);
                vec![]
            }
        },
        MosError::Multiple(errors) => errors.into_iter().map(to_diagnostics).flatten().collect(),
        _ => {
            eprintln!("Unknown parsing error: {:?}", e);
            vec![]
        }
    }
}
