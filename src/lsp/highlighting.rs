use crate::core::parser::{Expression, ExpressionFactor, Located, Token};
use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use codemap::{CodeMap, Span, SpanLoc};
use itertools::Itertools;
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
            Some(parsed) => match &parsed.tree {
                Some(tree) => {
                    let semtoks = emit_semantic_ast(tree.code_map(), tree.tokens());
                    let data = to_deltas(semtoks);
                    let tokens = SemanticTokens {
                        result_id: None,
                        data,
                    };
                    Ok(Some(SemanticTokensResult::Tokens(tokens)))
                }
                None => Ok(None),
            },
            None => Ok(None),
        }
    }
}

fn to_deltas(semtoks: Vec<SemTok>) -> Vec<SemanticToken> {
    let mut prev_line = 0;
    let mut prev_start = 0;

    let mut result = vec![];
    for st in semtoks
        .iter()
        .sorted_by_key(|t| (t.location.begin.line, t.location.begin.column))
    {
        let cur_line = st.location.begin.line;
        let cur_start = st.location.begin.column;
        let delta_line = cur_line - prev_line;
        let delta_start = if cur_line == prev_line {
            cur_start - prev_start
        } else {
            cur_start
        };
        prev_line = cur_line;
        prev_start = cur_start;
        result.push(SemanticToken {
            delta_line: delta_line as u32,
            delta_start: delta_start as u32,
            length: st.length as u32,
            token_type: st.token_type,
            token_modifiers_bitset: 0,
        });
    }

    result
}

struct SemTok {
    location: SpanLoc,
    length: usize,
    token_type: u32,
}

impl SemTok {
    fn new(code_map: &CodeMap, location: Span, length: usize, token_type: u32) -> Self {
        let location = code_map.look_up_span(location);
        Self {
            location,
            length,
            token_type,
        }
    }
}

fn emit_semantic_ast(code_map: &CodeMap, ast: &[Located<Token>]) -> Vec<SemTok> {
    ast.iter()
        .map(|lt| emit_semantic(code_map, lt))
        .flatten()
        .collect()
}

fn emit_semantic(code_map: &CodeMap, lt: &Located<Token>) -> Vec<SemTok> {
    match &lt.data {
        Token::Braces { inner, .. } | Token::Config { inner, .. } => {
            emit_semantic_ast(code_map, &inner.data)
        }
        Token::Instruction(i) => {
            let mut r = vec![];
            r.push(SemTok::new(code_map, i.mnemonic.span, 3, 0));
            if let Some(op) = &i.operand {
                r.extend(emit_semantic(code_map, op));
            }
            r
        }
        Token::Operand(op) => emit_expression_semantic(code_map, &op.expr),
        _ => vec![],
    }
}

fn emit_expression_semantic(code_map: &CodeMap, lt: &Located<Expression>) -> Vec<SemTok> {
    match &lt.data {
        Expression::BinaryExpression(bin) => {
            let mut lhs = emit_expression_semantic(code_map, &bin.lhs);
            let rhs = emit_expression_semantic(code_map, &bin.lhs);
            lhs.extend(rhs);
            lhs
        }
        Expression::Factor { factor, .. } => match &factor.data {
            ExpressionFactor::Number { value, .. } => {
                let formatted = format!("{}", value.data);
                let o = SemTok::new(code_map, value.span, formatted.len(), 2);
                vec![o]
            }
            _ => vec![],
        },
    }
}
