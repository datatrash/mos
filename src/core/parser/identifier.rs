use itertools::Itertools;
use std::fmt::Display;

#[macro_export]
macro_rules! id {
    ($s:expr) => {
        crate::core::parser::identifier::Identifier::from($s)
    };
}

#[macro_export]
macro_rules! idpath {
    ($s:expr) => {
        crate::core::parser::identifier::IdentifierPath::from($s)
    };
}

/// A Rust-style identifier that can be used to, well, identify things
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(String);

impl Identifier {
    pub fn new<S: Into<String>>(s: S) -> Self {
        let s = s.into();
        assert!(
            !s.contains('.'),
            "Identifiers may not contain periods. Use an IdentifierPath instead."
        );
        Self(s)
    }

    pub fn anonymous(index: usize) -> Self {
        Identifier::new(format!("$$scope_{}", index))
    }

    pub fn sup() -> Self {
        Identifier::new("super")
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    fn is_super(&self) -> bool {
        self.0.to_lowercase().eq("super")
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, other: &str) -> bool {
        self.0.as_str() == other
    }
}

impl From<&str> for Identifier {
    fn from(id: &str) -> Self {
        Identifier(id.to_string())
    }
}

impl<'a> From<&'a Identifier> for &'a str {
    fn from(id: &'a Identifier) -> Self {
        id.0.as_str()
    }
}

impl<'a> From<&'a Identifier> for Identifier {
    fn from(id: &'a Identifier) -> Self {
        id.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A path of multiple identifiers, usually written as being separated by dots (e.g. `foo.bar.baz`)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IdentifierPath(Vec<Identifier>);

impl Display for IdentifierPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|i| i.0.as_str()).collect_vec().join(".")
        )
    }
}

impl From<Identifier> for IdentifierPath {
    fn from(id: Identifier) -> Self {
        IdentifierPath::new(&[id])
    }
}

impl From<&Identifier> for IdentifierPath {
    fn from(id: &Identifier) -> Self {
        IdentifierPath::new(&[id.clone()])
    }
}

impl From<&IdentifierPath> for IdentifierPath {
    fn from(path: &IdentifierPath) -> Self {
        IdentifierPath::new(&path.0.clone())
    }
}

impl From<&str> for IdentifierPath {
    fn from(strs: &str) -> Self {
        IdentifierPath::new(
            &strs
                .split_terminator('.')
                .map(Identifier::from)
                .collect_vec(),
        )
    }
}

impl IdentifierPath {
    pub fn new(ids: &[Identifier]) -> Self {
        Self(ids.to_vec())
    }

    pub fn empty() -> Self {
        Self(vec![])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn push(&mut self, id: &Identifier) {
        self.0.push(id.clone());
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn pop_front(&mut self) {
        self.0.remove(0);
    }

    pub fn starts_with_super(&self) -> bool {
        self.0.first().map(|i| i.is_super()).unwrap_or_default()
    }

    pub fn parent(&self) -> IdentifierPath {
        let mut path = self.clone();
        path.pop();
        path
    }

    pub fn stem(&self) -> Option<&Identifier> {
        self.0.last()
    }

    pub fn join<I: Into<IdentifierPath>>(&self, other: I) -> IdentifierPath {
        let other = other.into();
        let mut p = self.0.clone();
        p.extend(other.0);
        IdentifierPath(p)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn has_parent<I: Into<IdentifierPath>>(&self, parent: I) -> bool {
        let parent = parent.into();

        // Parent should at the very least have one part less than the current identifier
        if parent.len() != self.0.len() - 1 {
            false
        } else {
            parent
                .0
                .iter()
                .enumerate()
                .all(|(idx, part)| self.0.get(idx) == Some(part))
        }
    }

    /// Is the identifier visible in 'scope'?
    /// This is the case if they share the same parent or if 'scope' is a child scope.
    /// Returns the scope in which the identifier is visible
    pub fn is_visible_in_scope<I: Into<IdentifierPath>>(&self, scope: I) -> Option<IdentifierPath> {
        let mut scope = scope.into();

        loop {
            if self.parent() == scope {
                return Some(scope);
            }
            if scope.is_empty() {
                return None;
            }

            scope = scope.parent();
        }
    }

    pub fn single(&self) -> &Identifier {
        assert_eq!(self.len(), 1);
        self.0.first().unwrap()
    }

    pub fn canonicalize(&self) -> IdentifierPath {
        let mut p = IdentifierPath::empty();
        for id in &self.0 {
            if id.is_super() {
                p.pop();
            } else {
                p.push(id);
            }
        }
        p
    }

    pub fn make_relative_to<I: Into<IdentifierPath>>(&self, scope: I) -> IdentifierPath {
        let mut scope = scope.into();

        match self.is_visible_in_scope(&scope) {
            Some(_) => {
                let mut result = IdentifierPath::empty();

                while scope != self.parent() && !scope.is_empty() {
                    result.push(&Identifier::sup());
                    scope = scope.parent();
                }

                // We are now at the parent level
                result.push(self.0.last().unwrap());
                result
            }
            None => {
                let mut result = self.clone();

                // 'self' is at a lower level than scope, so just pop off the shared path
                while !scope.is_empty() {
                    scope.pop_front();
                    result.pop_front();
                }

                result
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::IdentifierPath;

    #[test]
    fn canonicalize() {
        let path = IdentifierPath::from("a.super.b.foo");
        assert_eq!(path.canonicalize().to_string(), "b.foo");
    }

    #[test]
    fn make_relative_to() {
        assert_eq!(
            IdentifierPath::from("s1.foo").make_relative_to("s1.s2"),
            "super.foo".into()
        );
        assert_eq!(
            IdentifierPath::from("foo").make_relative_to("s1.s2"),
            "super.super.foo".into()
        );
        assert_eq!(
            IdentifierPath::from("s1.s2.foo").make_relative_to("s1.s2"),
            "foo".into()
        );
        assert_eq!(
            IdentifierPath::from("s1.s2.s3.foo").make_relative_to("s1.s2"),
            "s3.foo".into()
        );
    }

    #[test]
    fn has_parent() {
        assert_eq!(IdentifierPath::from("s1.s2").has_parent("s1"), true);
        assert_eq!(IdentifierPath::from("s1.s2").has_parent("foo"), false);
        assert_eq!(IdentifierPath::from("s1.s2").has_parent("s1.s2"), false);
    }

    #[test]
    fn is_visible_in_scope() {
        assert_eq!(
            IdentifierPath::from("id").is_visible_in_scope(""),
            Some(IdentifierPath::from(""))
        );
        assert_eq!(
            IdentifierPath::from("id").is_visible_in_scope("s1"),
            Some(IdentifierPath::from(""))
        );
        assert_eq!(
            IdentifierPath::from("s1.id").is_visible_in_scope("s1"),
            Some(IdentifierPath::from("s1"))
        );
        assert_eq!(
            IdentifierPath::from("s1.s2.id").is_visible_in_scope("s1"),
            None
        );
        assert_eq!(
            IdentifierPath::from("s1.s2.id").is_visible_in_scope("s1.s2.s3"),
            Some(IdentifierPath::from("s1.s2"))
        );
    }
}
