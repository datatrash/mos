#![allow(dead_code)]
#![allow(unused_imports)]
use crate::core::parser::{
    Expression, ExpressionFactor, Identifier, IdentifierPath, Located, Operand, ParseTree, Token,
};
use crate::errors::MosError;
use codemap::{File, Span, SpanLoc};
use lsp_types::Position;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

pub struct Analysis {
    pub tree: Arc<ParseTree>,
    pub error: Option<MosError>,
    pub definitions: DefinitionMap,
}

pub struct DefinitionMap {
    tree: Arc<ParseTree>,
    map: HashMap<IdentifierPath, Definition>,
}

impl DefinitionMap {
    pub fn new(tree: Arc<ParseTree>) -> Self {
        Self {
            tree,
            map: HashMap::new(),
        }
    }

    pub fn get_or_create_mut(&mut self, path: &IdentifierPath) -> &mut Definition {
        self.map.entry(path.clone()).or_insert_with(Definition::new)
    }

    pub fn find(&self, pos: Position) -> Option<(&Definition, SpanLoc)> {
        for def in self.map.values() {
            if let Some(def_location) = &def.location {
                if self.span_contains(def_location, pos)
                    || def.usages.iter().any(|s| self.span_contains(s, pos))
                {
                    let loc = self.tree.code_map().look_up_span(*def_location);
                    return Some((def, loc));
                }
            }
        }

        None
    }

    fn span_contains(&self, span: &Span, pos: Position) -> bool {
        let loc = self.tree.code_map().look_up_span(*span);
        pos.line >= loc.begin.line as u32
            && pos.line <= loc.end.line as u32
            && pos.character >= loc.begin.column as u32
            && pos.character <= loc.end.column as u32
    }
}

#[derive(Debug)]
pub struct Definition {
    location: Option<Span>,
    usages: Vec<Span>,
}

impl Definition {
    fn new() -> Self {
        Self {
            location: None,
            usages: vec![],
        }
    }
}

impl Analysis {
    pub fn new(tree: Arc<ParseTree>, error: Option<MosError>) -> Self {
        let definitions = generate_definitions(tree.clone());

        Self {
            tree,
            error,
            definitions,
        }
    }

    pub fn find(&self, pos: Position) -> Option<(&Definition, SpanLoc)> {
        self.definitions.find(pos)
    }
}

pub fn generate_definitions(tree: Arc<ParseTree>) -> DefinitionMap {
    let mut defs = DefinitionMap::new(tree.clone());
    gen_def_tokens(tree.tokens(), &mut defs);
    defs
}

pub fn gen_def_tokens(tokens: &[Token], defs: &mut DefinitionMap) {
    tokens.iter().for_each(|token| gen_def_token(token, defs));
}

fn gen_def_token(token: &Token, defs: &mut DefinitionMap) {
    match token {
        Token::Braces(block) | Token::Config(block) => gen_def_tokens(&block.inner, defs),
        Token::If { value, .. } => {
            gen_def_expression(value, defs);
        }
        Token::Instruction(i) => {
            if let Some(o) = &i.operand {
                gen_def_expression(&o.expr, defs);
            }
        }
        Token::Label { id, .. } | Token::VariableDefinition { id, .. } => {
            defs.get_or_create_mut(&IdentifierPath::new(&[id.data.clone()]))
                .location = Some(id.span);
        }
        _ => (),
    }
}

fn gen_def_expression(expr: &Located<Expression>, defs: &mut DefinitionMap) {
    match &expr.data {
        Expression::Factor { factor, .. } => gen_def_expression_factor(&factor, defs),
        Expression::BinaryExpression(bexp) => {
            gen_def_expression(&bexp.lhs, defs);
            gen_def_expression(&bexp.rhs, defs);
        }
    }
}

fn gen_def_expression_factor(factor: &Located<ExpressionFactor>, defs: &mut DefinitionMap) {
    match &factor.data {
        ExpressionFactor::ExprParens { inner, .. } => {
            gen_def_expression(inner, defs);
        }
        ExpressionFactor::IdentifierValue { path, .. } => {
            let def = defs.get_or_create_mut(&path.data);
            def.usages.push(path.span);
        }
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use crate::core::parser::parse;
    use crate::errors::MosResult;
    use crate::lsp::analysis::Analysis;
    use codemap::Span;
    use lsp_types::Position;

    #[test]
    fn can_find_token() -> MosResult<()> {
        let (tree, error) = parse("test.asm".as_ref(), "lda foo\nfoo: nop");
        let analysis = Analysis::new(tree.clone(), error);
        let (_, loc) = analysis.find(Position::new(0, 4)).unwrap();
        assert_eq!(loc.to_string(), "test.asm:2:1: 2:4");
        Ok(())
    }
}
