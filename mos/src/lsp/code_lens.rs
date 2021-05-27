use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use lsp_types::request::CodeLensRequest;
use lsp_types::{CodeLens, CodeLensParams, Command, Position, Range};

pub struct CodeLensRequestHandler;

impl_request_handler!(CodeLensRequestHandler);

impl RequestHandler<CodeLensRequest> for CodeLensRequestHandler {
    fn handle(
        &self,
        _ctx: &mut LspContext,
        _params: CodeLensParams,
    ) -> MosResult<Option<Vec<CodeLens>>> {
        Ok(Some(vec![CodeLens {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 4,
                },
            },
            command: Some(Command {
                title: "Run".to_string(),
                command: "mos.runSingleTest".to_string(),
                arguments: Some(vec![serde_json::Value::String("foo".into())]),
            }),
            data: None,
        }]))
    }
}
