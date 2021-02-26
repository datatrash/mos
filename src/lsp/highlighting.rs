use crate::core::parser::{Located, Location, Token};
use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use lsp_types::request::SemanticTokensFullRequest;
use lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
};

pub fn caps() -> SemanticTokensOptions {
    let legend = SemanticTokensLegend {
        token_types: vec![SemanticTokenType::CLASS],
        token_modifiers: vec![],
    };

    SemanticTokensOptions {
        legend,
        full: Some(SemanticTokensFullOptions::Bool(true)),
        ..Default::default()
    }
}

pub struct FullRequest {}

impl_request_handler!(FullRequest);

impl RequestHandler<SemanticTokensFullRequest> for FullRequest {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: SemanticTokensParams,
    ) -> MosResult<Option<SemanticTokensResult>> {
        match ctx.documents.get(&params.text_document.uri) {
            Some(parsed) => {
                let tokens = SemanticTokens {
                    result_id: None,
                    data: to_tokens(&parsed.ast, &mut DeltaState::new()),
                };
                Ok(Some(SemanticTokensResult::Tokens(tokens)))
            }
            None => Ok(None),
        }
    }
}

#[derive(Clone)]
struct DeltaState {
    prev_line: u32,
    prev_start: u32,
}

impl DeltaState {
    fn new() -> Self {
        Self {
            prev_line: 0,
            prev_start: 0,
        }
    }

    fn emit(&mut self, location: &Location, length: u32, token_type: u32) -> SemanticToken {
        let cur_line = location.line - 1;
        let cur_start = location.column - 1;
        let delta_line = cur_line - self.prev_line;
        let delta_start = if cur_line == self.prev_line {
            cur_start - self.prev_start
        } else {
            cur_start
        };
        self.prev_line = cur_line;
        self.prev_start = cur_start;
        SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: 0,
        }
    }
}

fn to_tokens(ast: &[Located<Token>], state: &mut DeltaState) -> Vec<SemanticToken> {
    ast.iter()
        .filter_map(|lt| match &lt.data {
            Token::Braces { inner, .. } | Token::Config { inner, .. } => {
                Some(to_tokens(&inner.data, state))
            }
            Token::Instruction(i) => Some(vec![state.emit(&i.mnemonic.location, 3, 0)]),
            _ => None,
        })
        .flatten()
        .collect()
}
