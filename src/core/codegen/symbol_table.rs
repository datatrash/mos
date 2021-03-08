use codemap::Span;
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::commands::SymbolType;
use crate::core::codegen::{CodegenError, CodegenResult, DetailedCodegenError, ProgramCounter};
use crate::core::parser::Identifier;
use crate::LINE_ENDING;

#[derive(Debug, PartialEq)]
pub(super) enum Symbol {
    Label(ProgramCounter),
    Variable(i64),
    Constant(i64),
    System(i64),
}

pub struct SymbolTable {
    root: Scope,
    current: Vec<String>,
    anonymous_scope_counter: usize,
}

#[derive(Debug)]
struct Scope {
    table: HashMap<String, Symbol>,
    parent: Option<Vec<String>>,
    children: HashMap<String, Scope>,
}

impl Scope {
    fn new(parent: Option<Vec<String>>) -> Self {
        Self {
            table: HashMap::new(),
            parent,
            children: HashMap::new(),
        }
    }

    fn to_symbols(&self, ty: SymbolType, path_prefix: Option<&str>) -> String {
        let mut symbols = self
            .table
            .iter()
            .filter_map(|(path, symbol)| match symbol {
                Symbol::Label(pc) => {
                    let full_path = match path_prefix {
                        Some(prefix) => format!("{}.{}", prefix, path),
                        None => path.to_string(),
                    };
                    Some(format!("al C:{:X} .{}", pc, full_path))
                }
                _ => None,
            })
            .collect_vec()
            .join(LINE_ENDING);

        for (child, scope) in &self.children {
            let child_symbols = scope.to_symbols(ty, Some(child));
            if !child_symbols.is_empty() {
                symbols = vec![symbols, child_symbols].join(LINE_ENDING);
            }
        }

        symbols
    }
}

impl SymbolTable {
    pub(super) fn new() -> Self {
        Self {
            root: Scope::new(None),
            current: vec![],
            anonymous_scope_counter: 0,
        }
    }

    pub(super) fn register<'a, ID: Into<&'a str>>(
        &mut self,
        id: ID,
        value: Symbol,
        span: Option<&Span>,
        allow_same_type_redefinition: bool,
    ) -> CodegenResult<()> {
        let id = id.into();
        if !allow_same_type_redefinition {
            let span = span.unwrap();
            if let Some(existing) = self.lookup(span, &[id], false)? {
                let is_same_type =
                    std::mem::discriminant(existing) == std::mem::discriminant(&value);

                if !is_same_type || existing != &value {
                    return Err(CodegenError::new(
                        *span,
                        DetailedCodegenError::SymbolRedefinition(Identifier(id.into())),
                    ));
                }
            }
        }

        self.current_scope_mut().table.insert(id.to_string(), value);
        Ok(())
    }

    pub(super) fn register_path(
        &mut self,
        path: &[&str],
        value: Symbol,
        span: Option<&Span>,
        allow_same_type_redefinition: bool,
    ) -> CodegenResult<()> {
        if let Some((last, parents)) = path.split_last() {
            for parent in parents {
                if !self.current_scope().children.contains_key(*parent) {
                    self.add_child_scope(Some(parent));
                }
                self.enter(parent);
            }

            let result = self.register(*last, value, span, allow_same_type_redefinition);

            for _ in parents {
                self.leave();
            }

            result
        } else {
            panic!("Empty path");
        }
    }

    // Look up a symbol in the symbol table.
    // If `bubble_up` is set, the lookup will bubble up to parent scopes if the symbol is not found in the current scope.
    fn lookup(
        &self,
        span: &Span,
        path: &[&str],
        bubble_up: bool,
    ) -> CodegenResult<Option<&Symbol>> {
        if path.is_empty() {
            return Ok(None);
        }

        let mut scope = self.current_scope();

        let (id, path_ids) = path.split_last().unwrap();

        let mut in_super_phase = true;
        for id in path_ids {
            if id.eq_ignore_ascii_case("super") {
                if !in_super_phase {
                    return Err(CodegenError::new(
                        *span,
                        DetailedCodegenError::SuperNotAllowed(path.iter().join(".")),
                    ));
                }

                match &scope.parent {
                    Some(p) => scope = self.scope(p),
                    None => {
                        // Already at the root scope
                    }
                }
            } else {
                in_super_phase = false;

                match scope.children.get(*id) {
                    Some(s) => scope = s,
                    None => {
                        return Ok(None);
                    }
                }
            }
        }

        loop {
            if let Some(symbol) = scope.table.get(&id.to_string()) {
                return Ok(Some(symbol));
            }

            if !bubble_up {
                // Didn't find it in the current scope, so bail out
                return Ok(None);
            }

            match &scope.parent {
                Some(p) => scope = self.scope(p),
                None => {
                    return Ok(None);
                }
            }
        }
    }

    pub(super) fn value(&self, span: &Span, path: &[&str]) -> CodegenResult<Option<i64>> {
        Ok(self
            .lookup(span, path, true)?
            .map(|s| match s {
                Symbol::Label(pc) => Some(pc.as_i64()),
                Symbol::Variable(val) | Symbol::Constant(val) | Symbol::System(val) => Some(*val),
            })
            .flatten())
    }

    fn current_scope(&self) -> &Scope {
        self.scope(&self.current)
    }

    fn scope(&self, path: &[String]) -> &Scope {
        let mut t = &self.root;
        for child in path {
            t = t.children.get(child).unwrap();
        }
        t
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        let mut t = &mut self.root;
        for child in &self.current {
            t = t.children.get_mut(child).unwrap();
        }
        t
    }

    pub(super) fn add_child_scope(&mut self, name: Option<&str>) -> String {
        let name = match name {
            Some(n) => n.to_string(),
            None => {
                self.anonymous_scope_counter += 1;
                format!("$$scope_{}", self.anonymous_scope_counter)
            }
        };
        log::trace!("Adding child scope: '{}'", name);
        let parent = self.current.clone();
        match self.current_scope_mut().children.entry(name.clone()) {
            Entry::Occupied(o) => {
                panic!("Trying to add child scope that already exists: {}", o.key());
            }
            Entry::Vacant(v) => {
                v.insert(Scope::new(Some(parent)));
                name
            }
        }
    }

    pub(super) fn enter(&mut self, name: &str) {
        log::trace!("Entering scope: '{}'", name);
        assert!(
            self.current_scope().children.contains_key(name),
            format!(
                "Can't enter unknown scope: '{}' from scope: {:?}",
                name,
                self.current_scope()
            )
        );
        self.current.push(name.to_string());
    }

    pub(super) fn leave(&mut self) {
        log::trace!("Leaving scope");
        assert!(!self.current.is_empty(), "Can't pop root scope");
        self.current.pop();
    }

    pub fn to_symbols(&self, ty: SymbolType) -> String {
        self.root.to_symbols(ty, None)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::commands::SymbolType;
    use crate::core::codegen::symbol_table::{Symbol, SymbolTable};
    use crate::core::codegen::CodegenResult;
    use codemap::{CodeMap, Span};

    type TestResult = CodegenResult<()>;

    #[test]
    fn can_lookup_symbols() -> TestResult {
        let mut st = SymbolTable::new();
        reg(&mut st, "A", 1)?;
        reg(&mut st, "B", 2)?;
        let scope = st.add_child_scope(Some("foo"));
        st.enter(&scope);
        reg(&mut st, "A", 100)?;

        assert_eq!(
            st.lookup(&loc(), &["A"], true)?,
            Some(&Symbol::Constant(100))
        );

        let scope = st.add_child_scope(Some("bar"));
        st.enter(&scope);
        reg(&mut st, "A", 555)?;
        assert_eq!(
            st.lookup(&loc(), &["A"], true)?,
            Some(&Symbol::Constant(555))
        );
        st.leave();

        assert_eq!(st.lookup(&loc(), &["B"], true)?, Some(&Symbol::Constant(2)));
        assert_eq!(
            st.lookup(&loc(), &["super", "A"], true)?,
            Some(&Symbol::Constant(1))
        );
        assert_eq!(
            st.lookup(&loc(), &["super", "foo", "A"], true)?,
            Some(&Symbol::Constant(100))
        );

        st.leave();

        assert_eq!(
            st.lookup(&loc(), &["foo", "bar", "A"], true)?,
            Some(&Symbol::Constant(555))
        );

        assert_eq!(st.lookup(&loc(), &["A"], true)?, Some(&Symbol::Constant(1)));
        assert_eq!(
            st.lookup(&loc(), &["foo", "A"], true)?,
            Some(&Symbol::Constant(100))
        );
        assert_eq!(
            st.lookup(&loc(), &["super", "A"], true)?,
            Some(&Symbol::Constant(1))
        );

        assert_eq!(st.lookup(&loc(), &["foo2", "A"], true)?, None);
        let scope = st.add_child_scope(Some("foo2"));
        st.enter(&scope);
        reg(&mut st, "A", 200)?;
        assert_eq!(
            st.lookup(&loc(), &["A"], true)?,
            Some(&Symbol::Constant(200))
        );
        st.leave();
        assert_eq!(
            st.lookup(&loc(), &["foo2", "A"], true)?,
            Some(&Symbol::Constant(200))
        );

        Ok(())
    }

    #[test]
    fn can_register_nested_symbols() -> TestResult {
        let mut st = SymbolTable::new();
        st.register_path(&["a", "a"], Symbol::Constant(1), Some(&loc()), false)?;
        st.register_path(&["a", "b"], Symbol::Constant(2), Some(&loc()), false)?;
        assert_eq!(
            st.lookup(&loc(), &["a", "a"], true)?,
            Some(&Symbol::Constant(1))
        );
        assert_eq!(
            st.lookup(&loc(), &["a", "b"], true)?,
            Some(&Symbol::Constant(2))
        );
        Ok(())
    }

    #[test]
    fn super_is_only_allowed_at_start() -> TestResult {
        let mut st = SymbolTable::new();
        st.register_path(&["foo", "bar"], Symbol::Constant(1), Some(&loc()), false)?;
        assert_eq!(
            st.lookup(&loc(), &["foo", "super", "foo", "bar"], true)
                .err()
                .unwrap()
                .to_string(),
            "'super' is only allowed at the start of a path: foo.super.foo.bar"
        );
        Ok(())
    }

    #[test]
    fn can_have_multiple_unnamed_scopes_side_by_side() -> TestResult {
        let mut st = SymbolTable::new();
        reg(&mut st, "A", 1)?;
        let scope = st.add_child_scope(None);
        st.enter(&scope);
        reg(&mut st, "A", 100)?;
        assert_eq!(
            st.lookup(&loc(), &["A"], true)?,
            Some(&Symbol::Constant(100))
        );
        st.leave();
        let scope = st.add_child_scope(None);
        st.enter(&scope);
        reg(&mut st, "A", 200)?;
        assert_eq!(
            st.lookup(&loc(), &["A"], true)?,
            Some(&Symbol::Constant(200))
        );
        st.leave();
        Ok(())
    }

    #[test]
    fn can_generate_vice_symbols() -> TestResult {
        let mut st = SymbolTable::new();
        st.register("foo", Symbol::Label(0x1234.into()), Some(&loc()), false)?;
        let scope = st.add_child_scope(Some("scope"));
        st.enter(&scope);
        st.register("foo", Symbol::Label(0xCAFE.into()), Some(&loc()), false)?;
        assert_eq!(
            st.to_symbols(SymbolType::Vice).lines().collect_vec(),
            &["al C:1234 .foo", "al C:CAFE .scope.foo"]
        );
        Ok(())
    }

    fn reg<'a>(st: &'a mut SymbolTable, id: &'a str, val: i64) -> TestResult {
        st.register(id, Symbol::Constant(val), Some(&loc()), false)
    }

    fn loc() -> Span {
        let mut codemap = CodeMap::new();
        let f1 = codemap.add_file("test1.rs".to_string(), "abcd\nefghij\nqwerty".to_string());
        f1.span
    }
}
