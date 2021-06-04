use crate::diagnostic_emitter::MosResult;
use crate::impl_request_handler;
use crate::lsp::{to_range, LspContext, RequestHandler};
use crate::test_runner::enumerate_test_cases;
use lsp_types::request::CodeLensRequest;
use lsp_types::{CodeLens, CodeLensParams, Command};

pub struct CodeLensRequestHandler;

impl_request_handler!(CodeLensRequestHandler);

impl RequestHandler<CodeLensRequest> for CodeLensRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: CodeLensParams,
    ) -> MosResult<Option<Vec<CodeLens>>> {
        let tests = enumerate_test_cases(
            ctx.parsing_source(),
            &params.text_document.uri.to_file_path().unwrap(),
        )
        .unwrap_or_default();

        let result = tests
            .into_iter()
            .map(|(sl, test_case_path)| {
                let run = CodeLens {
                    range: to_range(sl.clone()),
                    command: Some(Command {
                        title: "Run".to_string(),
                        command: "mos.runSingleTest".to_string(),
                        arguments: Some(vec![serde_json::Value::String(
                            test_case_path.to_string(),
                        )]),
                    }),
                    data: None,
                };

                let debug = CodeLens {
                    range: to_range(sl),
                    command: Some(Command {
                        title: "Debug".to_string(),
                        command: "mos.debugSingleTest".to_string(),
                        arguments: Some(vec![serde_json::Value::String(
                            test_case_path.to_string(),
                        )]),
                    }),
                    data: None,
                };

                vec![run, debug]
            })
            .flatten()
            .collect();

        Ok(Some(result))
    }
}
