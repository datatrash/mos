use crate::core::codegen::config_extractor::ConfigExtractor;
use crate::core::parser::code_map::Span;
use crate::core::parser::{Located, ParseTree, Token};
use crate::errors::{MosError, MosResult};
use itertools::Itertools;
use std::collections::HashSet;
use std::sync::Arc;

pub struct ConfigValidator {
    required: HashSet<String>,
    allowed: HashSet<String>,
}

impl ConfigValidator {
    pub fn new() -> Self {
        Self {
            required: HashSet::new(),
            allowed: HashSet::new(),
        }
    }

    pub fn required(mut self, key: &str) -> Self {
        self.required.insert(key.into());
        self
    }

    pub fn allowed(mut self, key: &str) -> Self {
        self.allowed.insert(key.into());
        self
    }

    pub fn extract<'a>(
        self,
        tree: Arc<ParseTree>,
        config_span: Span,
        kvps: &'a [(&'a Located<String>, &'a Located<Token>)],
    ) -> MosResult<ConfigExtractor<'a>> {
        let mut errors = vec![];

        let mut req = self.required.clone();
        for (key, _) in kvps.iter().sorted_by_key(|(k, _)| &k.data) {
            if req.contains(&key.data) {
                req.remove(&key.data);
            } else if !self.allowed.contains(&key.data) {
                errors.push((Some(key.span), format!("field not allowed: {}", key.data)));
            }
        }

        if !req.is_empty() {
            let r = req.iter().sorted().join(", ");
            errors.push((None, format!("missing required fields: {}", r)));
        }

        let errors = errors
            .into_iter()
            .map(|(span, message)| MosError::Codegen {
                location: tree.code_map().look_up_span(span.unwrap_or(config_span)),
                message,
            })
            .collect_vec();
        match errors.is_empty() {
            true => Ok(ConfigExtractor::new(kvps)),
            false => Err(MosError::Multiple(errors)),
        }
    }
}
