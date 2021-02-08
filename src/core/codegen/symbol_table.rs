use std::collections::hash_map::Entry;
use std::collections::HashMap;

use itertools::Itertools;

use crate::commands::SymbolType;
use crate::core::codegen::{CodegenError, CodegenResult};
use crate::core::parser::{Identifier, Location};
use crate::LINE_ENDING;

use super::segment::ProgramCounter;

#[derive(Debug, PartialEq)]
pub(super) enum Symbol {
    Label(ProgramCounter),
    Variable(i64),
    Constant(i64),
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
                    Some(format!("al C:{:X} .{}", pc.as_usize(), full_path))
                }
                _ => None,
            })
            .collect_vec()
            .join(LINE_ENDING);

        for (child, scope) in &self.children {
            symbols = vec![symbols, scope.to_symbols(ty, Some(child))].join(LINE_ENDING);
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

    pub(super) fn register<'a>(
        &mut self,
        id: &Identifier<'a>,
        value: Symbol,
        location: &Location<'a>,
        allow_same_type_redefinition: bool,
    ) -> CodegenResult<'a, ()> {
        if !allow_same_type_redefinition {
            if let Some(existing) = self.lookup_in_current_scope(&[id.0]) {
                let is_same_type =
                    std::mem::discriminant(existing) == std::mem::discriminant(&value);

                if !is_same_type || existing != &value {
                    return Err(CodegenError::SymbolRedefinition(
                        location.clone(),
                        id.clone(),
                    ));
                }
            }
        }

        self.current_scope_mut()
            .table
            .insert(id.0.to_string(), value);
        Ok(())
    }

    // Look up a symbol in the symbol table.
    // If `check_target_scope_only` is set, the lookup will not bubble up to parent scopes if the
    // symbol is not found.
    fn lookup_internal(&self, path: &[&str], check_target_scope_only: bool) -> Option<&Symbol> {
        if path.is_empty() {
            return None;
        }

        let mut scope = self.current_scope();

        let (id, path_ids) = path.split_last().unwrap();
        for path in path_ids {
            match *path {
                "super" => match &scope.parent {
                    Some(p) => scope = self.scope(p),
                    None => {
                        // Already at the root scope
                    }
                },
                id => match scope.children.get(id) {
                    Some(s) => scope = s,
                    None => {
                        return None;
                    }
                },
            }
        }

        loop {
            if let Some(symbol) = scope.table.get(&id.to_string()) {
                return Some(symbol);
            }

            if check_target_scope_only {
                // Didn't find it in the current scope, so bail out
                return None;
            }

            match &scope.parent {
                Some(p) => scope = self.scope(p),
                None => {
                    return None;
                }
            }
        }
    }

    pub(super) fn lookup(&self, path: &[&str]) -> Option<&Symbol> {
        self.lookup_internal(path, false)
    }

    pub(super) fn lookup_in_current_scope(&self, path: &[&str]) -> Option<&Symbol> {
        self.lookup_internal(path, true)
    }

    pub(super) fn value(&self, path: &[&str]) -> Option<i64> {
        self.lookup(path)
            .map(|s| match s {
                Symbol::Label(pc) => Some(pc.as_i64()),
                Symbol::Variable(val) => Some(*val),
                Symbol::Constant(val) => Some(*val),
            })
            .flatten()
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
    use crate::core::parser::{Identifier, Location};

    type TestResult = CodegenResult<'static, ()>;

    #[test]
    fn can_lookup_symbols() -> TestResult {
        let mut st = SymbolTable::new();
        reg(&mut st, "A", 1)?;
        reg(&mut st, "B", 2)?;
        let scope = st.add_child_scope(Some("foo"));
        st.enter(&scope);
        reg(&mut st, "A", 100)?;

        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(100)));

        let scope = st.add_child_scope(Some("bar"));
        st.enter(&scope);
        reg(&mut st, "A", 555)?;
        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(555)));
        st.leave();

        assert_eq!(st.lookup(&["B"]), Some(&Symbol::Constant(2)));
        assert_eq!(st.lookup(&["super", "A"]), Some(&Symbol::Constant(1)));
        assert_eq!(
            st.lookup(&["super", "foo", "A"]),
            Some(&Symbol::Constant(100))
        );

        st.leave();

        assert_eq!(
            st.lookup(&["foo", "bar", "A"]),
            Some(&Symbol::Constant(555))
        );

        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(1)));
        assert_eq!(st.lookup(&["foo", "A"]), Some(&Symbol::Constant(100)));
        assert_eq!(st.lookup(&["super", "A"]), Some(&Symbol::Constant(1)));

        assert_eq!(st.lookup(&["foo2", "A"]), None);
        let scope = st.add_child_scope(Some("foo2"));
        st.enter(&scope);
        reg(&mut st, "A", 200)?;
        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(200)));
        st.leave();
        assert_eq!(st.lookup(&["foo2", "A"]), Some(&Symbol::Constant(200)));

        Ok(())
    }

    #[test]
    fn can_have_multiple_unnamed_scopes_side_by_side() -> TestResult {
        let mut st = SymbolTable::new();
        reg(&mut st, "A", 1)?;
        let scope = st.add_child_scope(None);
        st.enter(&scope);
        reg(&mut st, "A", 100)?;
        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(100)));
        st.leave();
        let scope = st.add_child_scope(None);
        st.enter(&scope);
        reg(&mut st, "A", 200)?;
        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(200)));
        st.leave();
        Ok(())
    }

    #[test]
    fn can_generate_vice_symbols() -> TestResult {
        let mut st = SymbolTable::new();
        st.register(
            &Identifier("foo"),
            Symbol::Label(0x1234.into()),
            &loc(),
            false,
        )?;
        let scope = st.add_child_scope(Some("scope"));
        st.enter(&scope);
        st.register(
            &Identifier("foo"),
            Symbol::Label(0xCAFE.into()),
            &loc(),
            false,
        )?;
        assert_eq!(
            st.to_symbols(SymbolType::Vice).lines().collect_vec(),
            &["al C:1234 .foo", "al C:CAFE .scope.foo"]
        );
        Ok(())
    }

    fn reg(st: &mut SymbolTable, id: &'static str, val: i64) -> TestResult {
        st.register(&Identifier(id), Symbol::Constant(val), &loc(), false)
    }

    fn loc() -> Location<'static> {
        Location {
            path: "test.asm",
            line: 1,
            column: 1,
        }
    }
}
