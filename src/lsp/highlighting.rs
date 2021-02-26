use crate::core::parser::{Expression, ExpressionFactor, Located, Location, Token};
use crate::errors::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
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
            Some(parsed) => {
                let semtoks = emit_semantic_ast(&parsed.ast);
                let data = to_deltas(semtoks);
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

fn to_deltas(semtoks: Vec<SemTok>) -> Vec<SemanticToken> {
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    let mut result = vec![];
    for st in semtoks
        .iter()
        .sorted_by_key(|t| (t.location.line, t.location.column))
    {
        let cur_line = st.location.line - 1;
        let cur_start = st.location.column - 1;
        let delta_line = cur_line - prev_line;
        let delta_start = if cur_line == prev_line {
            cur_start - prev_start
        } else {
            cur_start
        };
        prev_line = cur_line;
        prev_start = cur_start;
        result.push(SemanticToken {
            delta_line,
            delta_start,
            length: st.length as u32,
            token_type: st.token_type,
            token_modifiers_bitset: 0,
        });
    }

    result
}

struct SemTok<'a> {
    location: &'a Location<'a>,
    length: usize,
    token_type: u32,
}

impl<'a> SemTok<'a> {
    fn new(location: &'a Location, length: usize, token_type: u32) -> Self {
        Self {
            location,
            length,
            token_type,
        }
    }
}

fn emit_semantic_ast<'a>(ast: &'a [Located<'a, Token<'a>>]) -> Vec<SemTok<'a>> {
    ast.iter().map(|lt| emit_semantic(lt)).flatten().collect()
}

fn emit_semantic<'a>(lt: &'a Located<'a, Token<'a>>) -> Vec<SemTok<'a>> {
    match &lt.data {
        Token::Braces { inner, .. } | Token::Config { inner, .. } => emit_semantic_ast(&inner.data),
        Token::Instruction(i) => {
            let mut r = vec![];
            r.push(SemTok::new(&i.mnemonic.location, 3, 0));
            if let Some(op) = &i.operand {
                r.extend(emit_semantic(op));
            }
            r
        }
        Token::Operand(op) => emit_expression_semantic(&op.expr),
        _ => vec![],
    }
}

fn emit_expression_semantic<'a>(lt: &'a Located<'a, Expression<'a>>) -> Vec<SemTok<'a>> {
    match &lt.data {
        Expression::BinaryExpression(bin) => {
            let mut lhs = emit_expression_semantic(&bin.lhs);
            let rhs = emit_expression_semantic(&bin.lhs);
            lhs.extend(rhs);
            lhs
        }
        Expression::Factor { factor, .. } => match &factor.data {
            ExpressionFactor::Number { value, .. } => {
                let formatted = format!("{}", value.data);
                let o = SemTok::new(&value.location, formatted.len(), 2);
                vec![o]
            }
            _ => vec![],
        },
    }
}
