use crate::codegen::symbols::SymbolIndex;
use crate::codegen::{QueryTraversalStep, Symbol, SymbolTable};
use crate::parser::code_map::{LineCol, Span, SpanLoc};
use crate::parser::{IdentifierPath, ParseTree};
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct Definition {
    pub location: Option<DefinitionLocation>,
    pub usages: HashSet<DefinitionLocation>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DefinitionLocation {
    pub parent_scope: SymbolIndex,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DefinitionType {
    Filename(PathBuf),
    Symbol(SymbolIndex),
}

impl Definition {
    pub fn usages(&self) -> Vec<&DefinitionLocation> {
        self.usages.iter().collect_vec()
    }

    pub fn definition_and_usages(&self) -> Vec<&DefinitionLocation> {
        let mut result = vec![];
        if let Some(l) = &self.location {
            result.push(l);
        }
        result.extend(self.usages.iter().collect_vec());
        result
    }

    pub fn set_location(&mut self, location: DefinitionLocation) {
        self.location = Some(location);
    }

    pub fn add_usage(&mut self, location: DefinitionLocation) {
        self.usages.insert(location);
    }

    pub fn contains(&self, tree: &ParseTree, path: &Path, pos: LineCol) -> bool {
        if let Some(loc) = &self.location {
            if span_contains(loc.span, tree, path, pos) {
                return true;
            }
        }

        self.usages
            .iter()
            .any(|usage| span_contains(usage.span, tree, path, pos))
    }

    pub fn try_get_usage_containing(
        &self,
        tree: &ParseTree,
        path: &Path,
        pos: LineCol,
    ) -> Option<&DefinitionLocation> {
        self.usages
            .iter()
            .find(|usage| span_contains(usage.span, tree, path, pos))
    }
}

fn span_contains(span: Span, tree: &ParseTree, path: &Path, pos: LineCol) -> bool {
    let loc = tree.code_map.look_up_span(span);
    loc.file.name() == path.to_str().unwrap()
        && pos.line >= loc.begin.line
        && pos.line <= loc.end.line
        && pos.column >= loc.begin.column
        && pos.column <= loc.end.column
}

pub struct Analysis {
    tree: Arc<ParseTree>,
    definitions: HashMap<DefinitionType, Definition>,
}

impl Debug for Analysis {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self.definitions)
    }
}

impl Analysis {
    pub fn new(tree: Arc<ParseTree>) -> Self {
        Self {
            tree,
            definitions: HashMap::new(),
        }
    }

    pub fn tree(&self) -> Arc<ParseTree> {
        self.tree.clone()
    }

    pub fn get_or_create_definition_mut(&mut self, ty: DefinitionType) -> &mut Definition {
        match self.definitions.entry(ty) {
            Entry::Occupied(e) => {
                log::trace!("Getting existing definition: {:?}", e.key());
                e.into_mut()
            }
            Entry::Vacant(e) => {
                log::trace!("Creating new definition: {:?}", e.key());
                e.insert(Definition {
                    location: None,
                    usages: Default::default(),
                })
            }
        }
    }

    /// Adds multiple symbol usages, for every part of the path
    /// For example, when adding a usage for 'foo.bar' it will add usages for 'foo' and for 'bar'.
    pub fn add_symbol_usage(
        &mut self,
        symbols: &SymbolTable<Symbol>,
        scope: SymbolIndex,
        path: &IdentifierPath,
        span: Span,
    ) {
        log::trace!("Trying to add symbol usages for definition '{}'", &path);
        let contains_super = path.contains_super();
        let mut path = path.clone();
        let mut pos = 0;
        for step in symbols.query_traversal_steps(scope, &path) {
            let (nx, id) = match step {
                QueryTraversalStep::Symbol(nx) => (nx, path.pop_front()),
                QueryTraversalStep::Super(nx) => {
                    // If the original path contains a 'super' then we can pop it off here and add a usage for it
                    if contains_super {
                        (nx, path.pop_front())
                    } else {
                        // If not, we'll just ignore this step
                        (nx, None)
                    }
                }
            };

            if let Some(id) = id {
                let def = self.get_or_create_definition_mut(DefinitionType::Symbol(nx));
                if let Some(parent_scope) = symbols.parent(nx) {
                    log::trace!("Adding usage for definition '{}' ({:?})", &id, nx);
                    def.add_usage(DefinitionLocation {
                        parent_scope,
                        span: span.subspan(pos, pos + id.len() as u64),
                    });
                }
                pos += id.len() as u64 + 1; // add 1 for the dot separator in the path
            }
        }
    }

    pub fn find<P: Into<PathBuf>>(
        &self,
        path: P,
        pos: LineCol,
    ) -> Vec<(&DefinitionType, &Definition)> {
        self.find_filter(path, pos, |_| true)
    }

    pub fn find_filter<P: Into<PathBuf>, F: Fn(&DefinitionType) -> bool>(
        &self,
        path: P,
        pos: LineCol,
        filter: F,
    ) -> Vec<(&DefinitionType, &Definition)> {
        let path = path.into();
        self.definitions
            .iter()
            .filter(|(ty, definition)| filter(ty) && definition.contains(&self.tree, &path, pos))
            .collect()
    }

    pub fn look_up(&self, span: Span) -> SpanLoc {
        self.tree.code_map.look_up_span(span)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::tests::{test_codegen, test_codegen_parsing_source};
    use crate::codegen::CodegenOptions;
    use crate::errors::CoreResult;
    use crate::parser::code_map::LineCol;
    use crate::parser::source::InMemoryParsingSource;

    #[test]
    fn can_find_basic_token() -> CoreResult<()> {
        let ctx = test_codegen("lda foo\nfoo: nop")?;
        let defs = ctx.analysis().find("test.asm", line_col(0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "test.asm:2:1: 2:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_complex_token() -> CoreResult<()> {
        let ctx = test_codegen("lda a.super.b.foo\na: {foo: nop}\nb: {foo: nop}")?;

        // 'a'
        let defs = ctx.analysis().find("test.asm", line_col(0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "test.asm:2:1: 2:2"
        );

        // 'a.super'
        let defs = ctx.analysis().find("test.asm", line_col(0, 6));
        assert!(defs.is_empty());

        // 'a.super.b'
        let defs = ctx.analysis().find("test.asm", line_col(0, 13));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "test.asm:3:1: 3:2"
        );

        // 'a.super.b.foo'
        let defs = ctx.analysis().find("test.asm", line_col(0, 15));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "test.asm:3:5: 3:8"
        );

        Ok(())
    }

    #[test]
    fn can_find_imported_token() -> CoreResult<()> {
        let ctx = test_codegen_parsing_source(
            InMemoryParsingSource::new()
                .add("test.asm", "lda foo\n.import foo from \"bar\"")
                .add("bar", "foo: nop")
                .into(),
            CodegenOptions::default(),
        )?;
        let defs = ctx.analysis().find("test.asm", line_col(0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "bar:1:1: 1:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_complex_imported_token() -> CoreResult<()> {
        let ctx = test_codegen_parsing_source(
            InMemoryParsingSource::new()
                .add(
                    "test.asm",
                    "lda baz\n.import foo as baz from \"bar\" { .const ENABLE = 1 }",
                )
                .add("bar", ".if defined(ENABLE) {\nfoo: nop\n}")
                .into(),
            CodegenOptions::default(),
        )?;
        let defs = ctx.analysis().find("test.asm", line_col(0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "bar:2:1: 2:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_import() -> CoreResult<()> {
        let ctx = test_codegen_parsing_source(
            InMemoryParsingSource::new()
                .add("test.asm", ".import foo from \"bar\"")
                .add("bar", "foo: nop")
                .into(),
            CodegenOptions::default(),
        )?;
        // Click on the 'bar' filename in the import
        let defs = ctx.analysis().find("test.asm", line_col(0, 20));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().1.location.as_ref().unwrap().span)
                .to_string(),
            "bar:1:1: 1:9"
        );
        Ok(())
    }

    fn line_col(line: usize, column: usize) -> LineCol {
        LineCol { line, column }
    }
}
