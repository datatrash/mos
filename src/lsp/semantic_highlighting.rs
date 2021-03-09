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
            SemanticTokenType::METHOD,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::NUMBER,
            SemanticTokenType::VARIABLE,
        ],
        token_modifiers: vec![],
    };

    SemanticTokensOptions {
        legend,
        full: Some(SemanticTokensFullOptions::Bool(true)),
        ..Default::default()
    }
}

pub struct SemanticTokensFullRequestHandler {}

impl_request_handler!(SemanticTokensFullRequestHandler);

impl RequestHandler<SemanticTokensFullRequest> for SemanticTokensFullRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        _params: SemanticTokensParams,
    ) -> MosResult<Option<SemanticTokensResult>> {
        if let Some(analysis) = &ctx.analysis {
            let semtoks = emit_semantic_ast(analysis.tree.code_map(), analysis.tree.tokens());
            let data = to_deltas(semtoks);
            let tokens = SemanticTokens {
                result_id: None,
                data,
            };
            Ok(Some(SemanticTokensResult::Tokens(tokens)))
        } else {
            Ok(None)
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
            length: (st.location.end.column - st.location.begin.column) as u32,
            token_type: st.token_type,
            token_modifiers_bitset: 0,
        });
    }

    result
}

struct SemTok {
    location: SpanLoc,
    token_type: u32,
}

impl SemTok {
    fn new(code_map: &CodeMap, location: Span, token_type: u32) -> Self {
        let location = code_map.look_up_span(location);
        Self {
            location,
            token_type,
        }
    }
}

fn emit_semantic_ast(code_map: &CodeMap, ast: &[Token]) -> Vec<SemTok> {
    ast.iter()
        .map(|tok| emit_semantic(code_map, tok))
        .flatten()
        .collect()
}

fn emit_semantic(code_map: &CodeMap, token: &Token) -> Vec<SemTok> {
    match &token {
        Token::Braces { block, .. } | Token::Config(block) => {
            emit_semantic_ast(code_map, &block.inner)
        }
        Token::Label { id, block, .. } => {
            let mut r = vec![];
            r.push(SemTok::new(code_map, id.span, 3));
            if let Some(b) = block {
                r.extend(emit_semantic_ast(code_map, &b.inner));
            }
            r
        }
        Token::If {
            if_, value, else_, ..
        } => {
            let mut r = vec![];
            r.extend(emit_semantic_ast(code_map, &if_.inner));
            r.extend(emit_expression_semantic(code_map, value));
            if let Some(e) = else_ {
                r.extend(emit_semantic_ast(code_map, &e.inner));
            }
            r
        }
        Token::VariableDefinition { id, .. } => {
            let r = SemTok::new(code_map, id.span, 3);
            vec![r]
        }
        Token::Instruction(i) => {
            let mut r = vec![];
            r.push(SemTok::new(code_map, i.mnemonic.span, 0));
            if let Some(op) = &i.operand {
                r.extend(emit_expression_semantic(code_map, &op.expr));
            }
            r
        }
        _ => vec![],
    }
}

fn emit_expression_semantic(code_map: &CodeMap, lt: &Located<Expression>) -> Vec<SemTok> {
    match &lt.data {
        Expression::BinaryExpression(bin) => {
            let mut lhs = emit_expression_semantic(code_map, &bin.lhs);
            let rhs = emit_expression_semantic(code_map, &bin.rhs);
            lhs.extend(rhs);
            lhs
        }
        Expression::Factor { factor, .. } => match &factor.data {
            ExpressionFactor::ExprParens { inner, .. } => emit_expression_semantic(code_map, inner),
            ExpressionFactor::Number { value, .. } => {
                let r = SemTok::new(code_map, value.span, 2);
                vec![r]
            }
            ExpressionFactor::IdentifierValue { path, .. } => {
                let r = SemTok::new(code_map, path.span, 3);
                vec![r]
            }
            _ => vec![],
        },
    }
}
