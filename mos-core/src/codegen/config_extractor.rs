use crate::codegen::CodegenContext;
use crate::errors::CoreResult;
use crate::parser::{Expression, ExpressionFactor, Identifier, IdentifierPath, Located, Token};
use codespan_reporting::diagnostic::Diagnostic;

pub struct ConfigExtractor<'a> {
    kvps: Vec<(&'a Located<String>, &'a Located<Token>)>,
}

impl<'a> ConfigExtractor<'a> {
    pub fn new(kvps: &[(&'a Located<String>, &'a Located<Token>)]) -> Self {
        Self {
            kvps: kvps.to_vec(),
        }
    }

    pub fn try_get_string(&self, key: &str) -> Option<String> {
        self.try_get_path(key).map(|p| p.to_string())
    }

    pub fn try_get_identifier(&self, key: &str) -> CoreResult<Option<Identifier>> {
        match self.try_get_path(key) {
            Some(mut path) if path.len() == 1 => Ok(path.pop_front()),
            Some(path) if path.len() > 1 => Err(Diagnostic::error()
                .with_message("expected single identifier")
                .with_labels(vec![self.get_located_token(key).span.to_label()])
                .into()),
            _ => Ok(None),
        }
    }

    pub fn get_identifier(&self, key: &str) -> CoreResult<Identifier> {
        let path = self.get_path(key)?;
        match path.len() {
            1 => Ok(path.first().unwrap().clone()),
            _ => Err(Diagnostic::error()
                .with_message("expected single identifier")
                .with_labels(vec![self.get_located_token(key).span.to_label()])
                .into()),
        }
    }

    pub fn try_get_path(&self, key: &str) -> Option<IdentifierPath> {
        let factor = match self.try_get_located_token(key) {
            Some(lt) => match &lt.data {
                Token::Expression(Expression::Factor { factor, .. }) => Some(&factor.data),
                _ => None,
            },
            None => None,
        };

        factor
            .map(|factor| match factor {
                ExpressionFactor::IdentifierValue { path, .. } => Some(path.data.clone()),
                _ => None,
            })
            .flatten()
    }

    pub fn get_path(&self, key: &str) -> CoreResult<IdentifierPath> {
        match self.try_get_path(key) {
            Some(path) => Ok(path),
            None => Err(Diagnostic::error()
                .with_message("expected identifier path")
                .with_labels(vec![self.get_located_token(key).span.to_label()])
                .into()),
        }
    }

    pub fn try_get_i64(&self, ctx: &mut CodegenContext, key: &str) -> CoreResult<Option<i64>> {
        match self.try_get_located_token(key) {
            Some(lt) => match &lt.data {
                Token::Expression(expr) => {
                    Ok(ctx.evaluate_expression(&lt.map(|_| expr.clone()), true)?)
                }
                _ => Err(Diagnostic::error().into()),
            },
            None => Ok(None),
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

    fn get_located_token(&self, wanted: &str) -> &Located<Token> {
        self.try_get_located_token(wanted)
            .unwrap_or_else(|| panic!("Unknown key: {}", wanted))
    }
}
