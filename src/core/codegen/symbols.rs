use crate::core::codegen::Symbol;
use crate::core::parser::{Identifier, IdentifierPath};
use std::collections::{HashMap, HashSet};

// Given a symbol table, this will make all symbol paths relative to the 'relative_scope'.
pub fn relativize_symbols<'a>(
    symbols: HashMap<IdentifierPath, HashMap<Identifier, &'a Symbol>>,
    relative_scope: &IdentifierPath,
) -> HashMap<IdentifierPath, HashMap<IdentifierPath, &'a Symbol>> {
    let mut result = HashMap::new();
    for (scope, symbols_in_scope) in symbols {
        let s = symbols_in_scope
            .into_iter()
            .map(|(id, sym)| (scope.join(id).make_relative_to(relative_scope), sym))
            .collect::<HashMap<_, _>>();
        result.insert(scope, s);
    }
    result
}

// Given a symbol table, this will reduce the paths lengths as long as they are not ambigious
// and it will remove any paths that just refer to other scopes (e.g. when scopes s1 and s1.s2 exist, 's1' will not have a 's2' symbol anymore)
pub fn simplify_symbols<'a>(
    mut symbols: HashMap<IdentifierPath, HashMap<IdentifierPath, &'a Symbol>>,
    relative_scope: &IdentifierPath,
) -> HashMap<IdentifierPath, HashMap<IdentifierPath, &'a Symbol>> {
    let mut result = HashMap::new();

    // First, move up to the parent and simplify
    let mut scope = relative_scope.clone();
    let mut existing_names = HashSet::new();
    while !scope.is_empty() {
        if let Some(scope_symbols) = symbols.remove(&scope) {
            let symbols = scope_symbols
                .into_iter()
                .map(|(id, sym)| {
                    let mut simplified = id.clone();
                    let mut can_simplify = simplified.starts_with_super();
                    while can_simplify && simplified.starts_with_super() {
                        simplified.pop_front();
                        if existing_names.contains(&simplified) {
                            can_simplify = false;
                        }
                    }
                    if can_simplify {
                        existing_names.insert(simplified.clone());
                        (simplified, sym)
                    } else {
                        // Could not simplify, just re-use the original id
                        existing_names.insert(id.clone());
                        (id, sym)
                    }
                })
                .collect();
            result.insert(scope.clone(), symbols);
        }
        scope = scope.parent();
    }

    // Then, copy all remaining child scopes untouched
    result.extend(symbols);

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{id, idpath};
    use once_cell::sync::OnceCell;

    #[test]
    fn relativize() {
        let s = symbols();
        let s = relativize_symbols(s, &idpath!("s1.s2"));

        let scope = s.get(&idpath!("s1")).unwrap();
        assert_eq!(scope.len(), 2);
        assert!(scope.contains_key(&idpath!("super.a")));
        assert!(scope.contains_key(&idpath!("super.c")));

        let scope = s.get(&idpath!("s1.s2")).unwrap();
        assert_eq!(scope.len(), 2);
        assert!(scope.contains_key(&idpath!("a")));
        assert!(scope.contains_key(&idpath!("b")));

        let scope = s.get(&idpath!("s1.s2.s3")).unwrap();
        assert_eq!(scope.len(), 1);
        assert!(scope.contains_key(&idpath!("s3.b")));
    }

    #[test]
    fn simplify() {
        let s = symbols();
        let s = relativize_symbols(s, &idpath!("s1.s2"));
        let s = simplify_symbols(s, &idpath!("s1.s2"));

        let scope = s.get(&idpath!("s1")).unwrap();
        assert_eq!(scope.len(), 2);
        assert!(scope.contains_key(&idpath!("super.a")));
        assert!(scope.contains_key(&idpath!("c")));

        let scope = s.get(&idpath!("s1.s2")).unwrap();
        assert_eq!(scope.len(), 2);
        assert!(scope.contains_key(&idpath!("a")));
        assert!(scope.contains_key(&idpath!("b")));

        let scope = s.get(&idpath!("s1.s2.s3")).unwrap();
        assert_eq!(scope.len(), 1);
        assert!(scope.contains_key(&idpath!("s3.b")));
    }

    fn symbols() -> HashMap<IdentifierPath, HashMap<Identifier, &'static Symbol>> {
        SYMBOL.get_or_init(|| Symbol::constant(1));

        // s1.a / s1.c
        // `---> s2.a / s2.b
        //       `---> s3.b

        let mut scopes = HashMap::new();

        let mut scope = HashMap::new();
        scope.insert(id!("a"), SYMBOL.get().unwrap());
        scope.insert(id!("c"), SYMBOL.get().unwrap());
        scopes.insert(idpath!("s1"), scope);

        let mut scope = HashMap::new();
        scope.insert(id!("a"), SYMBOL.get().unwrap());
        scope.insert(id!("b"), SYMBOL.get().unwrap());
        scopes.insert(idpath!("s1.s2"), scope);

        let mut scope = HashMap::new();
        scope.insert(id!("b"), SYMBOL.get().unwrap());
        scopes.insert(idpath!("s1.s2.s3"), scope);

        scopes
    }

    static SYMBOL: OnceCell<Symbol> = OnceCell::new();
}
