use crate::diagnostic_emitter::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
use itertools::Itertools;
use lsp_types::request::SemanticTokensFullRequest;
use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult,
};
use mos_core::parser::code_map::{CodeMap, LineCol, Span, SpanLoc};
use mos_core::parser::{
    ArgItem, Block, Expression, ExpressionFactor, Identifier, ImportArgs, ImportAs,
    InterpolatedString, InterpolatedStringItem, SpecificImportArg, Token, VariableType,
};
use once_cell::sync::OnceCell;
use std::collections::HashMap;
use strum::IntoEnumIterator;

#[derive(Clone, Copy, Debug, strum::EnumIter, Hash, PartialEq, Eq)]
enum TokenType {
    Identifier,
    Mnemonic,
    Number,
    Variable,
    Constant,
    Keyword,
}

static TOKEN_TYPE_LOOKUP: OnceCell<HashMap<TokenType, u32>> = OnceCell::new();
static TOKEN_MODIFIER_LOOKUP: OnceCell<HashMap<TokenType, u32>> = OnceCell::new();

impl TokenType {
    fn semantic_token_type(&self) -> SemanticTokenType {
        match &self {
            TokenType::Identifier => SemanticTokenType::METHOD,
            TokenType::Mnemonic => SemanticTokenType::PROPERTY,
            TokenType::Number => SemanticTokenType::NUMBER,
            TokenType::Variable => SemanticTokenType::VARIABLE,
            TokenType::Constant => SemanticTokenType::VARIABLE,
            TokenType::Keyword => SemanticTokenType::KEYWORD,
        }
    }

    fn semantic_token_modifiers(&self) -> Vec<SemanticTokenModifier> {
        match &self {
            TokenType::Constant => vec![SemanticTokenModifier::READONLY],
            _ => vec![],
        }
    }

    fn available_types() -> Vec<SemanticTokenType> {
        Self::iter()
            .map(|ty| ty.semantic_token_type())
            .unique()
            .collect_vec()
    }

    fn available_modifiers() -> Vec<SemanticTokenModifier> {
        Self::iter().flat_map(|ty| ty.semantic_token_modifiers())
            .unique()
            .collect_vec()
    }

    fn type_to_u32(&self) -> u32 {
        let ty = self.semantic_token_type();
        let (val, _) = Self::available_types()
            .into_iter()
            .find_position(|available_ty| available_ty == &ty)
            .unwrap();
        val as u32
    }

    fn modifiers_to_u32(&self) -> u32 {
        let mut result = 0;
        for modifier in self.semantic_token_modifiers() {
            let (val, _) = Self::available_modifiers()
                .into_iter()
                .find_position(|available_mod| available_mod == &modifier)
                .unwrap();
            result += val as u32;
        }
        result
    }
}

pub fn caps() -> SemanticTokensOptions {
    let lookup = TokenType::iter().map(|ty| (ty, ty.type_to_u32())).collect();
    let _ = TOKEN_TYPE_LOOKUP.set(lookup);
    let lookup = TokenType::iter()
        .map(|ty| (ty, ty.modifiers_to_u32()))
        .collect();
    let _ = TOKEN_MODIFIER_LOOKUP.set(lookup);

    let legend = SemanticTokensLegend {
        token_types: TokenType::available_types(),
        token_modifiers: TokenType::available_modifiers(),
    };

    SemanticTokensOptions {
        legend,
        full: Some(SemanticTokensFullOptions::Bool(true)),
        ..Default::default()
    }
}

pub struct SemanticTokensFullRequestHandler;

impl_request_handler!(SemanticTokensFullRequestHandler);

impl RequestHandler<SemanticTokensFullRequest> for SemanticTokensFullRequestHandler {
    fn handle(
        &self,
        ctx: &mut LspContext,
        params: SemanticTokensParams,
    ) -> MosResult<Option<SemanticTokensResult>> {
        if let Some(tree) = &ctx.tree {
            let path = params.text_document.uri.to_file_path().unwrap();
            if let Some(file) = tree.try_get_file(&path) {
                let semtoks = emit_semantic_ast(&file.tokens);
                let data = to_deltas(&tree.code_map, semtoks);
                let tokens = SemanticTokens {
                    result_id: None,
                    data,
                };
                return Ok(Some(SemanticTokensResult::Tokens(tokens)));
            }
        }

        Ok(None)
    }
}

fn to_deltas(code_map: &CodeMap, semtoks: Vec<SemTok>) -> Vec<SemanticToken> {
    let semtoks = semtoks
        .into_iter()
        .map(|st| (code_map.look_up_span(st.span), st.token_type))
        .collect_vec();

    let semtoks_per_line = semtoks
        .into_iter()
        .sorted_by_key(|(location, _)| (location.begin.line, location.begin.column)).flat_map(|(location, ty)| {
            let mut result = vec![];

            let mut line = location.begin.line;
            let mut column = location.begin.column;
            loop {
                let length = if line == location.end.line {
                    location.end.column
                } else {
                    location.file.source_line(line).len()
                };

                let begin = LineCol { line, column };
                let end = LineCol {
                    line,
                    column: length,
                };
                result.push((
                    SpanLoc {
                        file: location.file.clone(),
                        begin,
                        end,
                    },
                    ty,
                ));

                if line == location.end.line {
                    break;
                } else {
                    line += 1;
                    column = 0;
                }
            }

            result
        })
        .collect_vec();

    let mut prev_line = 0;
    let mut prev_start = 0;
    let mut result = vec![];
    for (location, ty) in semtoks_per_line
        .iter()
        .sorted_by_key(|(location, _)| (location.begin.line, location.begin.column))
    {
        let cur_line = location.begin.line;
        let cur_start = location.begin.column;
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
            length: (location.end.column - location.begin.column) as u32,
            token_type: *TOKEN_TYPE_LOOKUP.get().unwrap().get(ty).unwrap(),
            token_modifiers_bitset: *TOKEN_MODIFIER_LOOKUP.get().unwrap().get(ty).unwrap(),
        });
    }

    result
}

struct SemTok {
    span: Span,
    token_type: TokenType,
}

impl SemTok {
    fn new(span: Span, token_type: TokenType) -> Self {
        Self { span, token_type }
    }
}

struct SemTokBuilder {
    tokens: Vec<SemTok>,
}

impl SemTokBuilder {
    fn new() -> Self {
        Self { tokens: vec![] }
    }

    fn push<T: Into<Span>>(mut self, s: T, ty: TokenType) -> Self {
        self.tokens.push(SemTok::new(s.into(), ty));
        self
    }

    fn identifier<T: Into<Span>>(self, val: T) -> Self {
        self.push(val, TokenType::Identifier)
    }

    fn token(mut self, val: &Token) -> Self {
        self.tokens.extend(emit_semantic(val).tokens);
        self
    }

    fn block(mut self, val: &Block) -> Self {
        self.tokens.extend(emit_semantic_ast(&val.inner));
        self
    }

    fn expression(mut self, val: &Expression) -> Self {
        self.tokens.extend(emit_expression_semantic(val).tokens);
        self
    }

    fn expression_args(mut self, args: &[ArgItem<Expression>]) -> Self {
        for (expr, _) in args {
            self = self.expression(&expr.data);
        }
        self
    }

    fn identifier_args(mut self, args: &[ArgItem<Identifier>]) -> Self {
        for (id, _) in args {
            self = self.identifier(id);
        }
        self
    }

    fn import_as(self, as_: &ImportAs) -> Self {
        self.identifier(&as_.path)
    }

    fn specific_import_args(mut self, args: &[ArgItem<SpecificImportArg>]) -> Self {
        for (arg, _) in args {
            let arg = &arg.data;
            self = self.identifier(&arg.path);
            self = match &arg.as_ {
                Some(as_) => self.import_as(as_),
                None => self,
            };
        }
        self
    }

    fn interpolated_string(mut self, i: &InterpolatedString) -> Self {
        for item in &i.items {
            self = match item {
                InterpolatedStringItem::String(str) => self.push(str, TokenType::Constant),
                InterpolatedStringItem::IdentifierPath(path) => self.identifier(path),
            }
        }
        self
    }
}

fn emit_semantic_ast(ast: &[Token]) -> Vec<SemTok> {
    ast.iter().flat_map(|tok| emit_semantic(tok).tokens)
        .collect()
}

fn emit_semantic(token: &Token) -> SemTokBuilder {
    let b = SemTokBuilder::new();

    match token {
        Token::Align { tag: _, value } => b.expression(&value.data),
        Token::Assert {
            tag: _,
            value,
            failure_message: _,
        } => b.expression(&value.data),
        Token::Braces { block, .. } | Token::Config(block) => b.block(block),
        Token::ConfigPair { key, value, .. } => b.push(key, TokenType::Keyword).token(&value.data),
        Token::Data { values, size: _ } => b.expression_args(values),
        Token::Definition { tag: _, id, value } => {
            let b = b.identifier(id);
            match value {
                Some(v) => b.token(v),
                None => b,
            }
        }
        Token::Eof(_) => b,
        Token::Error(_) => b,
        Token::Expression(expr) => b.expression(expr),
        Token::File {
            tag: _, filename, ..
        } => b.interpolated_string(filename),
        Token::Label { id, block, .. } => {
            let b = b.identifier(id);
            match block {
                Some(block) => b.block(block),
                None => b,
            }
        }
        Token::Import {
            tag: _,
            args,
            from: _,
            filename,
            block,
            ..
        } => {
            let b = match args {
                ImportArgs::All(_, as_) => match as_ {
                    Some(a) => b.import_as(a),
                    None => b,
                },
                ImportArgs::Specific(args) => b.specific_import_args(args),
            };
            let b = b.interpolated_string(filename);
            match block {
                Some(block) => b.block(block),
                None => b,
            }
        }
        Token::If {
            tag_if: _,
            if_,
            value,
            tag_else: _,
            else_,
            ..
        } => {
            let b = b.block(if_).expression(&value.data);
            match else_ {
                Some(e) => b.block(e),
                None => b,
            }
        }
        Token::Instruction(i) => match &i.operand {
            Some(op) => b.expression(&op.expr.data),
            None => b,
        },
        Token::Loop {
            tag: _,
            loop_scope: _,
            expr,
            block,
        } => b.expression(&expr.data).block(block),
        Token::MacroDefinition {
            tag: _,
            id,
            args,
            block,
            ..
        } => b.identifier(id).identifier_args(args).block(block),
        Token::MacroInvocation { id: name, args, .. } => {
            SemTokBuilder::new().identifier(name).expression_args(args)
        }
        Token::ProgramCounterDefinition { star, value, .. } => {
            b.push(star, TokenType::Keyword).expression(&value.data)
        }
        Token::Segment { tag: _, id, block } => {
            let b = b.identifier(id);
            match block {
                Some(block) => b.block(block),
                None => b,
            }
        }
        Token::Test { tag: _, id, block } => b.identifier(id).block(block),
        Token::Text { tag: _, text, .. } => b.expression(&text.data),
        Token::Trace { args, .. } => b.expression_args(args),
        Token::VariableDefinition { ty, id, value, .. } => {
            let token_type = match &ty.data {
                VariableType::Constant => TokenType::Constant,
                VariableType::Variable => TokenType::Variable,
            };
            b.identifier(id).push(value, token_type)
        }
    }
}

fn emit_expression_semantic(expression: &Expression) -> SemTokBuilder {
    match &expression {
        Expression::BinaryExpression(bin) => SemTokBuilder::new()
            .expression(&bin.lhs.data)
            .expression(&bin.rhs.data),
        Expression::Factor { factor, .. } => match &factor.data {
            ExpressionFactor::ExprParens { inner, .. } => emit_expression_semantic(&inner.data),
            ExpressionFactor::Number { .. } => SemTokBuilder::new(),
            ExpressionFactor::InterpolatedString(i) => SemTokBuilder::new().interpolated_string(i),
            ExpressionFactor::IdentifierValue { path, .. } => SemTokBuilder::new().identifier(path),
            ExpressionFactor::CurrentProgramCounter(pc) => {
                SemTokBuilder::new().push(pc, TokenType::Constant)
            }
            ExpressionFactor::FunctionCall { name, args, .. } => {
                SemTokBuilder::new().identifier(name).expression_args(args)
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::testing::test_root;
    use crate::lsp::LspServer;

    #[test]
    fn provide_tokens() -> MosResult<()> {
        caps(); // initialize a few globals
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            ".macro foo() {\n    .const bar = 1\n}",
        )?;
        let response = server.semantic_tokens(test_root().join("main.asm"))?;
        if let Some(SemanticTokensResult::Tokens(SemanticTokens { data: tokens, .. })) = response {
            let tokens = tokens
                .into_iter()
                .map(|tok| (tok.delta_line, tok.delta_start, tok.length))
                .collect_vec();

            assert_eq!(
                tokens,
                vec![
                    /* 'foo' */ (0, 7, 3),
                    /* 'bar' */ (1, 11, 3),
                    /* '1' */ (0, 6, 1),
                ]
            );
        } else {
            panic!()
        }

        Ok(())
    }
}
