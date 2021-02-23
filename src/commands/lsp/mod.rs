use clap::{App, ArgMatches};
use lsp_server::{Connection, Message};
use lsp_types::notification::{
    DidChangeTextDocument, DidOpenTextDocument, Notification, PublishDiagnostics,
};
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncKind,
};

use crate::commands::lsp::messages::MosNotification;
use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::parse;
use crate::errors::{MosError, MosResult};

mod messages;

pub fn lsp_app() -> App<'static> {
    App::new("lsp").about("Starts a language server, listening on STDIN")
}

pub fn lsp_command(_args: &ArgMatches) -> MosResult<()> {
    eprintln!("Starting MOS language server");
    let (connection, io_threads) = Connection::stdio();

    let caps = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncKind::Full.into()),
        ..Default::default()
    };
    let server_capabilities = serde_json::to_value(&caps).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(&connection, initialization_params)?;
    io_threads.join()?;

    eprintln!("Shutting down MOS language server");

    Ok(())
}

fn main_loop(connection: &Connection, params: serde_json::Value) -> MosResult<()> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => match not.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    let params: DidOpenTextDocumentParams =
                        serde_json::from_value(not.params).unwrap();
                    let diagnostics =
                        parse_document(params.text_document.uri.path(), &params.text_document.text);
                    let params = PublishDiagnosticsParams::new(
                        params.text_document.uri,
                        diagnostics,
                        Some(params.text_document.version),
                    );
                    let n = MosNotification::<PublishDiagnostics>::new(params);
                    connection.sender.send(n.into())?;
                }
                DidChangeTextDocument::METHOD => {
                    let params: DidChangeTextDocumentParams =
                        serde_json::from_value(not.params).unwrap();
                    let text_changes = params.content_changes.first().unwrap();
                    let diagnostics =
                        parse_document(params.text_document.uri.path(), &text_changes.text);
                    let params = PublishDiagnosticsParams::new(
                        params.text_document.uri,
                        diagnostics,
                        Some(params.text_document.version),
                    );
                    let n = MosNotification::<PublishDiagnostics>::new(params);
                    connection.sender.send(n.into())?;
                }
                _ => eprintln!("got notification: {:?}", not),
            },
        }
    }
    Ok(())
}

fn parse_document(filename: &str, source: &str) -> Vec<Diagnostic> {
    let ast = parse(filename.as_ref(), source);
    match ast {
        Ok(ast) => {
            let code = codegen(ast, CodegenOptions::default());
            match code {
                Ok(_) => vec![],
                Err(e) => to_diagnostics(e),
            }
        }
        Err(e) => to_diagnostics(e),
    }
}

fn to_diagnostics(e: MosError) -> Vec<Diagnostic> {
    match e {
        MosError::Parser {
            ref location,
            ref length,
            ref message,
        } => match location {
            Some(l) => {
                let start = Position::new(l.line - 1, l.column - 1);
                let end = Position::new(l.line - 1, l.column + *length as u32);
                let range = Range::new(start, end);
                let d = Diagnostic::new_simple(range, message.clone());
                vec![d]
            }
            None => {
                eprintln!("Parser error without a location: {:?}", e);
                vec![]
            }
        },
        MosError::Codegen {
            ref location,
            ref message,
        } => match location {
            Some(l) => {
                let start = Position::new(l.line - 1, l.column - 1);
                let end = Position::new(l.line - 1, l.column);
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
