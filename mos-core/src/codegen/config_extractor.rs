use crate::codegen::CodegenContext;
use crate::errors::CoreResult;
use crate::parser::code_map::Span;
use crate::parser::{Expression, Located, Token};
use codespan_reporting::diagnostic::Diagnostic;

pub struct ConfigExtractor<'a> {
    config_span: Span,
    kvps: Vec<(&'a Located<String>, &'a Located<Token>)>,
}

impl<'a> ConfigExtractor<'a> {
    pub fn new(config_span: Span, kvps: &[(&'a Located<String>, &'a Located<Token>)]) -> Self {
        Self {
            config_span,
            kvps: kvps.to_vec(),
        }
    }

    pub fn get_string(&self, ctx: &mut CodegenContext, key: &str) -> CoreResult<String> {
        if let Some(str) = self.try_get_string(ctx, key)? {
            Ok(str)
        } else {
            let span = self
                .try_get_kvp(key)
                .map(|(k, v)| k.span.merge(v.span))
                .unwrap_or(self.config_span);
            Err(Diagnostic::error()
                .with_message(format!("could not evaluate configuration key '{}'", key))
                .with_labels(vec![span.to_label()])
                .into())
        }
    }

    pub fn try_get_string(
        &self,
        ctx: &mut CodegenContext,
        key: &str,
    ) -> CoreResult<Option<String>> {
        match self.try_get_expression(key) {
            Some(expr) => ctx.evaluate_expression_as_string(&expr, true),
            None => Ok(None),
        }
    }

    pub fn try_get_i64(&self, ctx: &mut CodegenContext, key: &str) -> CoreResult<Option<i64>> {
        match self.try_get_expression(key) {
            Some(expr) => ctx.evaluate_expression_as_i64(&expr, true),
            None => Ok(None),
        }
    }

    pub fn try_get_expression(&self, key: &str) -> Option<Located<Expression>> {
        let expr = self.try_get_located_token(key).map(|lt| {
            lt.map(|tok| match tok {
                Token::Expression(expr) => Some(expr.clone()),
                _ => None,
            })
        });

        match expr {
            Some(expr) if expr.data.is_some() => Some(Located::new_with_trivia(
                expr.span,
                expr.data.unwrap(),
                expr.trivia,
            )),
            _ => None,
        }
    }

    fn try_get_located_token(&self, wanted: &str) -> Option<&Located<Token>> {
        for (key, value) in &self.kvps {
            if key.data == wanted {
                return Some(value);
            }
        }
        None
    }

    fn try_get_kvp(&self, wanted: &str) -> Option<(&Located<String>, &Located<Token>)> {
        for (key, value) in &self.kvps {
            if key.data == wanted {
                return Some((key, value));
            }
        }
        None
    }
}
