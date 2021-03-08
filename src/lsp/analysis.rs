#![allow(dead_code)]
#![allow(unused_imports)]
use crate::core::parser::{
    Expression, ExpressionFactor, Identifier, IdentifierPath, Located, Operand, ParseTree, Token,
};
use crate::errors::MosError;
use codemap::{File, Span, SpanLoc};
use lsp_types::{Position, Url};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub struct Analysis {
    pub tree: Arc<ParseTree>,
    pub error: Option<MosError>,
    pub definitions: DefinitionMap,
}

pub struct AnalysisSpan(SpanLoc);

impl Display for AnalysisSpan {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<SpanLoc> for AnalysisSpan {
    fn from(sl: SpanLoc) -> Self {
        Self(sl)
    }
}

impl From<AnalysisSpan> for lsp_types::Location {
    fn from(a: AnalysisSpan) -> Self {
        let span = &a.0;
        Self {
            uri: Url::from_file_path(span.file.name()).unwrap(),
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: span.begin.line as u32,
                    character: span.begin.column as u32,
                },
                end: lsp_types::Position {
                    line: span.end.line as u32,
                    character: span.end.column as u32,
                },
            },
        }
    }
}

impl From<AnalysisSpan> for lsp_types::DocumentHighlight {
    fn from(a: AnalysisSpan) -> Self {
        let loc: lsp_types::Location = a.into();
        Self {
            range: loc.range,
            kind: None,
        }
    }
}

pub struct DefinitionMap {
    tree: Arc<ParseTree>,
    map: HashMap<IdentifierPath, Definition>,
}

impl DefinitionMap {
    pub fn new(tree: Arc<ParseTree>) -> Self {
        let mut d = Self {
            tree,
            map: HashMap::new(),
        };

        DefinitionGenerator::generate(&mut d);

        d
    }

    pub fn get_or_create_mut(&mut self, path: &IdentifierPath) -> &mut Definition {
        self.map.entry(path.clone()).or_insert_with(Definition::new)
    }

    pub fn find(&self, pos: Position) -> Option<&Definition> {
        for def in self.map.values() {
            if let Some(def_location) = &def.location {
                if self.span_contains(def_location, pos)
                    || def.usages.iter().any(|s| self.span_contains(s, pos))
                {
                    return Some(def);
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
    pub location: Option<Span>,
    pub usages: Vec<Span>,
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
        let definitions = DefinitionMap::new(tree.clone());

        Self {
            tree,
            error,
            definitions,
        }
    }

    pub fn look_up_span(&self, span: Span) -> AnalysisSpan {
        self.tree.code_map().look_up_span(span).into()
    }

    pub fn find(&self, pos: Position) -> Option<&Definition> {
        self.definitions.find(pos)
    }
}

struct DefinitionGenerator<'a> {
    defs: &'a mut DefinitionMap,
    scope: IdentifierPath,
}

impl<'a> DefinitionGenerator<'a> {
    fn generate(defs: &'a mut DefinitionMap) {
        let tree = defs.tree.clone();
        let mut gen = DefinitionGenerator {
            defs,
            scope: IdentifierPath::empty(),
        };
        gen.gen_def_tokens(tree.tokens());
    }

    fn gen_def_tokens(&mut self, tokens: &[Token]) {
        tokens.iter().for_each(|token| self.gen_def_token(token));
    }

    fn gen_def_token(&mut self, token: &Token) {
        match token {
            Token::Braces { block, .. } | Token::Config(block) => self.gen_def_tokens(&block.inner),
            Token::If { value, .. } => {
                self.gen_def_expression(value);
            }
            Token::Instruction(i) => {
                if let Some(o) = &i.operand {
                    self.gen_def_expression(&o.expr);
                }
            }
            Token::Label { id, block, .. } => {
                self.defs
                    .get_or_create_mut(&self.scope.join(&id.data))
                    .location = Some(id.span);

                if let Some(b) = block {
                    self.scope.push(&id.data);
                    self.gen_def_tokens(&b.inner);
                    self.scope.pop();
                }
            }
            Token::VariableDefinition { id, .. } => {
                self.defs
                    .get_or_create_mut(&self.scope.join(&id.data))
                    .location = Some(id.span);
            }
            _ => (),
        }
    }

    fn gen_def_expression(&mut self, expr: &Located<Expression>) {
        match &expr.data {
            Expression::Factor { factor, .. } => self.gen_def_expression_factor(&factor),
            Expression::BinaryExpression(bexp) => {
                self.gen_def_expression(&bexp.lhs);
                self.gen_def_expression(&bexp.rhs);
            }
        }
    }

    fn gen_def_expression_factor(&mut self, factor: &Located<ExpressionFactor>) {
        match &factor.data {
            ExpressionFactor::ExprParens { inner, .. } => {
                self.gen_def_expression(inner);
            }
            ExpressionFactor::IdentifierValue { path, .. } => {
                let full_path = self.scope.join(&path.data).canonicalize();
                let def = self.defs.get_or_create_mut(&full_path);
                def.usages.push(path.span);
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::{codegen, CodegenOptions};
    use crate::core::parser::parse;
    use crate::errors::MosResult;
    use crate::lsp::analysis::Analysis;
    use codemap::Span;
    use lsp_types::Position;

    #[test]
    fn can_find_basic_token() -> MosResult<()> {
        let analysis = analysis("lda foo\nfoo: nop");
        let def = analysis.find(Position::new(0, 4)).unwrap();
        assert_eq!(
            analysis.look_up_span(def.location.unwrap()).to_string(),
            "test.asm:2:1: 2:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_complex_token() -> MosResult<()> {
        let analysis = analysis("lda a.super.b.foo\na: {foo: nop}\nb: {foo: nop}");
        let def = analysis.find(Position::new(0, 4)).unwrap();
        assert_eq!(
            analysis.look_up_span(def.location.unwrap()).to_string(),
            "test.asm:3:5: 3:8"
        );
        Ok(())
    }

    fn analysis(src: &str) -> Analysis {
        let (tree, error) = parse("test.asm".as_ref(), src);
        Analysis::new(tree.clone(), error)
    }
}
