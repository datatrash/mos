use std::collections::HashMap;

use crate::core::codegen::{CodegenError, CodegenResult};
use crate::core::parser::{Identifier, Location};

use super::segment::ProgramCounter;

#[derive(Debug, PartialEq)]
pub(super) enum Symbol {
    Label(ProgramCounter),
    Variable(usize),
    Constant(usize),
}

pub(super) struct SymbolTable {
    root: Scope,
    current: Vec<String>,
}

struct Scope {
    table: HashMap<String, Symbol>,
    children: HashMap<String, Scope>,
}

impl Scope {
    fn new() -> Self {
        Self {
            table: HashMap::new(),
            children: HashMap::new(),
        }
    }
}

impl SymbolTable {
    pub(super) fn new() -> Self {
        Self {
            root: Scope::new(),
            current: vec![],
        }
    }

    pub(super) fn register<'a, 'b>(
        &mut self,
        id: &Identifier<'a>,
        value: Symbol,
        location: &Location<'a>,
    ) -> CodegenResult<'a, ()> {
        if let Some(existing) = self.lookup(&[id.0]) {
            let is_same_type = std::mem::discriminant(existing) == std::mem::discriminant(&value);

            if !is_same_type {
                return Err(CodegenError::SymbolRedefinition(
                    location.clone(),
                    id.clone(),
                ));
            }
        }

        self.current_scope_mut()
            .table
            .insert(id.0.to_string(), value);
        Ok(())
    }

    pub(super) fn lookup(&self, path: &[&str]) -> Option<&Symbol> {
        let mut scope = self.current_scope();

        let (id, path_ids) = path.split_last().unwrap();
        for path in path_ids {
            match *path {
                "super" => {
                    scope = self.current_parent_scope();
                }
                id => match scope.children.get(id) {
                    Some(s) => scope = s,
                    None => {
                        return None;
                    }
                },
            }
        }

        scope.table.get(&id.to_string())
    }

    pub(super) fn value(&self, path: &[&str]) -> Option<usize> {
        self.lookup(path)
            .map(|s| match s {
                Symbol::Label(pc) => Some(pc.as_usize()),
                Symbol::Variable(val) => Some(*val),
                Symbol::Constant(val) => Some(*val),
            })
            .flatten()
    }

    fn current_scope(&self) -> &Scope {
        let mut t = &self.root;
        for child in &self.current {
            t = t.children.get(child).unwrap();
        }
        t
    }

    fn current_parent_scope(&self) -> &Scope {
        if self.current.is_empty() {
            return &self.root;
        }

        let mut t = &self.root;
        for child in self.current.split_last().unwrap().1 {
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

    fn add_child_scope(&mut self, name: &str) {
        self.current_scope_mut()
            .children
            .insert(name.to_string(), Scope::new());
    }

    fn enter(&mut self, name: &str) {
        assert!(
            self.current_scope().children.contains_key(name),
            format!("Can't enter unknown scope: {}", name)
        );
        self.current.push(name.to_string());
    }

    fn leave(&mut self) {
        assert!(!self.current.is_empty(), "Can't pop root scope");
        self.current.pop();
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::symbol_table::{Symbol, SymbolTable};
    use crate::core::codegen::CodegenResult;
    use crate::core::parser::{Identifier, Location};

    type TestResult = CodegenResult<'static, ()>;

    #[test]
    fn can_lookup_symbols() -> TestResult {
        let mut st = SymbolTable::new();
        reg(&mut st, "A", 1)?;
        reg(&mut st, "B", 2)?;
        st.add_child_scope("foo");
        st.enter("foo");
        reg(&mut st, "A", 100)?;

        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(100)));
        assert_eq!(st.lookup(&["super", "A"]), Some(&Symbol::Constant(1)));
        assert_eq!(
            st.lookup(&["super", "foo", "A"]),
            Some(&Symbol::Constant(100))
        );

        st.leave();
        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(1)));
        assert_eq!(st.lookup(&["foo", "A"]), Some(&Symbol::Constant(100)));
        assert_eq!(st.lookup(&["super", "A"]), Some(&Symbol::Constant(1)));

        assert_eq!(st.lookup(&["foo2", "A"]), None);
        st.add_child_scope("foo2");
        st.enter("foo2");
        reg(&mut st, "A", 200)?;
        assert_eq!(st.lookup(&["A"]), Some(&Symbol::Constant(200)));
        st.leave();
        assert_eq!(st.lookup(&["foo2", "A"]), Some(&Symbol::Constant(200)));

        Ok(())
    }

    fn reg(st: &mut SymbolTable, id: &'static str, val: usize) -> TestResult {
        st.register(&Identifier(id), Symbol::Constant(val), &loc())
    }

    fn loc() -> Location<'static> {
        Location {
            path: "test.asm",
            line: 1,
            column: 1,
        }
    }
}
