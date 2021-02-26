use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::errors::{MosError, MosResult};
use crate::impl_notification_handler;
use crate::lsp::{LspContext, NotificationHandler, ParsedAst};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, PublishDiagnostics};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Position,
    PublishDiagnosticsParams, Range, Url,
};
use std::path::PathBuf;

pub struct DidOpen {}
pub struct DidChange {}

impl_notification_handler!(DidOpen);
impl_notification_handler!(DidChange);

fn insert_parsed_document(ctx: &mut LspContext, uri: &Url, source: &str) {
    let path = PathBuf::from(uri.path());
    let source = source.to_string();
    let mut parsed = Box::new(ParsedAst {
        path,
        source,
        ast: vec![],
        error: None,
    });

    let parse_result = unsafe {
        // Since we know 'parsed' won't move in memory anymore since it's a Box, we can do partial borrows here
        let path = &parsed.path as *const PathBuf;
        let source = &parsed.source as *const String;
        parse(path.as_ref().unwrap(), source.as_ref().unwrap().as_str())
    };

    match parse_result {
        Ok(ast) => match codegen(ast.clone(), CodegenOptions::default()) {
            Ok(_) => {
                parsed.ast = ast;
            }
            Err(e) => {
                parsed.error = Some(e);
            }
        },
        Err(e) => {
            parsed.error = Some(e);
        }
    }

    ctx.documents.insert(uri.clone(), parsed);
}

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
    match error {
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
                eprintln!("Parser error without a location: {:?}", error);
                vec![]
            }
        },
        MosError::Multiple(errors) => errors.iter().map(to_diagnostics).flatten().collect(),
        _ => {
            eprintln!("Unknown parsing error: {:?}", error);
            vec![]
        }
    }
}
