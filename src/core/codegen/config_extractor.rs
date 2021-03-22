use crate::core::codegen::CodegenContext;
use crate::core::parser::{
    Expression, ExpressionFactor, Identifier, IdentifierPath, Located, Token,
};
use crate::errors::{MosError, MosResult};

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

    pub fn get_identifier(&self, ctx: &mut CodegenContext, key: &str) -> MosResult<Identifier> {
        let path = self.get_path(ctx, key)?;
        match path.len() {
            1 => Ok(path.single().clone()),
            _ => Err(MosError::Codegen {
                location: ctx
                    .tree
                    .code_map()
                    .look_up_span(self.get_located_token(key).span),
                message: "expected single identifier".into(),
            }),
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

    pub fn get_path(&self, ctx: &mut CodegenContext, key: &str) -> MosResult<IdentifierPath> {
        match self.try_get_path(key) {
            Some(path) => Ok(path),
            None => Err(MosError::Codegen {
                location: ctx
                    .tree
                    .code_map()
                    .look_up_span(self.get_located_token(key).span),
                message: "expected identifier".into(),
            }),
        }
    }

    pub fn try_get_i64(&self, ctx: &mut CodegenContext, key: &str) -> MosResult<Option<i64>> {
        match self.try_get_located_token(key) {
            Some(lt) => match &lt.data {
                Token::Expression(expr) => {
                    Ok(Some(ctx.evaluate_expression(&lt.map(|_| expr.clone()))?))
                }
                _ => Ok(None),
            },
            None => Ok(None),
        }
    }

    pub fn get_i64(&self, ctx: &mut CodegenContext, key: &str) -> MosResult<i64> {
        match self.try_get_i64(ctx, key)? {
            Some(val) => Ok(val),
            None => Err(MosError::Codegen {
                location: ctx
                    .tree
                    .code_map()
                    .look_up_span(self.get_located_token(key).span),
                message: "expected expression".into(),
            }),
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
