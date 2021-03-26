use crate::core::parser::code_map::{LineCol, Span, SpanLoc};
use crate::core::parser::{IdentifierPath, ParseTree};
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct Definition {
    pub location: Option<Span>,
    pub usages: HashSet<Span>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum DefinitionType {
    Filename(PathBuf),
    Symbol(IdentifierPath),
}

impl Definition {
    pub fn usages(&self) -> Vec<&Span> {
        self.usages.iter().collect_vec()
    }

    pub fn definition_and_usages(&self) -> Vec<&Span> {
        let mut result = vec![];
        if let Some(l) = &self.location {
            result.push(l);
        }
        result.extend(self.usages.iter().collect_vec());
        result
    }

    pub fn set_location<DI: Into<Span>>(&mut self, location: DI) {
        let location = location.into();
        self.location = Some(location);
    }

    pub fn add_usage<DI: Into<Span>>(&mut self, location: DI) {
        let location = location.into();
        self.usages.insert(location);
    }

    pub fn contains(&self, tree: &ParseTree, path: &Path, pos: LineCol) -> bool {
        if let Some(loc) = &self.location {
            if span_contains(*loc, tree, path, pos) {
                return true;
            }
        }

        self.usages
            .iter()
            .any(|u| span_contains(*u, tree, path, pos))
    }

    pub fn try_get_usage_containing(
        &self,
        tree: &ParseTree,
        path: &Path,
        pos: LineCol,
    ) -> Option<&Span> {
        self.usages
            .iter()
            .find(|u| span_contains(**u, tree, path, pos))
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
                e.insert(Definition::default())
            }
        }
    }

    pub fn find<P: Into<PathBuf>, LC: Into<LineCol>>(&self, path: P, pos: LC) -> Vec<&Definition> {
        self.find_filter(path, pos, |_| true)
    }

    pub fn find_filter<P: Into<PathBuf>, LC: Into<LineCol>, F: Fn(&DefinitionType) -> bool>(
        &self,
        path: P,
        pos: LC,
        filter: F,
    ) -> Vec<&Definition> {
        let path = path.into();
        let pos = pos.into();
        self.definitions
            .iter()
            .filter(|(ty, _)| filter(ty))
            .filter_map(|(_, definition)| {
                if definition.contains(&self.tree, &path, pos) {
                    Some(definition)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn look_up(&self, span: Span) -> SpanLoc {
        self.tree.code_map.look_up_span(span)
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::tests::{test_codegen, test_codegen_parsing_source};
    use crate::core::parser::source::InMemoryParsingSource;
    use crate::errors::MosResult;

    #[test]
    fn can_find_basic_token() -> MosResult<()> {
        let ctx = test_codegen("lda foo\nfoo: nop")?;
        let defs = ctx.analysis().find("test.asm", (0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().location.unwrap())
                .to_string(),
            "test.asm:2:1: 2:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_complex_token() -> MosResult<()> {
        let ctx = test_codegen("lda a.super.b.foo\na: {foo: nop}\nb: {foo: nop}")?;
        let defs = ctx.analysis().find("test.asm", (0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().location.unwrap())
                .to_string(),
            "test.asm:3:5: 3:8"
        );
        Ok(())
    }

    #[test]
    fn can_find_imported_token() -> MosResult<()> {
        let ctx = test_codegen_parsing_source(
            InMemoryParsingSource::new()
                .add("test.asm", "lda foo\n.import foo from \"bar\"")
                .add("bar", "foo: nop\n.export foo")
                .into(),
        )?;
        let defs = ctx.analysis().find("test.asm", (0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().location.unwrap())
                .to_string(),
            "bar:1:1: 1:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_complex_imported_token() -> MosResult<()> {
        let ctx = test_codegen_parsing_source(
            InMemoryParsingSource::new()
                .add(
                    "test.asm",
                    "lda baz\n.import foo as baz from \"bar\" { .const ENABLE = 1 }",
                )
                .add("bar", ".if defined(ENABLE) {\nfoo: nop\n.export foo\n}")
                .into(),
        )?;
        let defs = ctx.analysis().find("test.asm", (0, 4));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().location.unwrap())
                .to_string(),
            "bar:2:1: 2:4"
        );
        Ok(())
    }

    #[test]
    fn can_find_import() -> MosResult<()> {
        let ctx = test_codegen_parsing_source(
            InMemoryParsingSource::new()
                .add("test.asm", ".import foo from \"bar\"")
                .add("bar", "foo: nop\n.export foo")
                .into(),
        )?;
        // Click on the 'bar' filename in the import
        let defs = ctx.analysis().find("test.asm", (0, 20));
        assert_eq!(
            ctx.analysis()
                .look_up(defs.first().unwrap().location.unwrap())
                .to_string(),
            "bar:1:1: 2:12"
        );
        Ok(())
    }
}
