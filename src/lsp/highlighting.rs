use crate::core::parser::{Expression, ExpressionFactor, Located, Location, Token};
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
        token_types: vec![
            SemanticTokenType::CLASS,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::NUMBER,
        ],
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
                let data = emit_semantic_ast(&parsed.ast, &mut DeltaState::new());
                let tokens = SemanticTokens {
                    result_id: None,
                    data,
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

    fn emit(&mut self, location: &Location, length: usize, token_type: u32) -> SemanticToken {
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
            length: length as u32,
            token_type,
            token_modifiers_bitset: 0,
        }
    }
}

fn emit_semantic_ast(ast: &[Located<Token>], state: &mut DeltaState) -> Vec<SemanticToken> {
    ast.iter()
        .map(|lt| emit_semantic(lt, state))
        .flatten()
        .collect()
}

fn emit_semantic(lt: &Located<Token>, state: &mut DeltaState) -> Vec<SemanticToken> {
    match &lt.data {
        Token::Braces { inner, .. } | Token::Config { inner, .. } => {
            emit_semantic_ast(&inner.data, state)
        }
        Token::Instruction(i) => {
            let mut r = vec![];
            r.push(state.emit(&i.mnemonic.location, 3, 0));
            if let Some(op) = &i.operand {
                r.extend(emit_semantic(op, state));
            }
            r
        }
        Token::Operand(op) => emit_expression_semantic(&op.expr, state),
        _ => vec![],
    }
}

fn emit_expression_semantic(
    lt: &Located<Expression>,
    state: &mut DeltaState,
) -> Vec<SemanticToken> {
    match &lt.data {
        Expression::BinaryExpression(bin) => {
            let mut lhs = emit_expression_semantic(&bin.lhs, state);
            let rhs = emit_expression_semantic(&bin.lhs, state);
            lhs.extend(rhs);
            lhs
        }
        Expression::Factor { factor, .. } => match &factor.data {
            ExpressionFactor::Number { value, .. } => {
                let formatted = format!("{}", value.data);
                let o = state.emit(&value.location, formatted.len(), 2);
                vec![o]
            }
            _ => vec![],
        },
    }
}
