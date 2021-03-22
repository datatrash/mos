use itertools::Itertools;
use std::fmt::Display;

/// A Rust-style identifier that can be used to, well, identify things
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(String);

impl Identifier {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Self(s.into())
    }

    pub fn anonymous(index: usize) -> Self {
        Identifier::new(format!("$$scope_{}", index))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    fn is_super(&self) -> bool {
        self.0.to_lowercase().eq("super")
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

    pub fn parent(&self) -> IdentifierPath {
        let mut path = self.clone();
        path.pop();
        path
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
        parent
            .0
            .iter()
            .enumerate()
            .all(|(idx, part)| self.0.get(idx) == Some(part))
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
}

#[cfg(test)]
mod tests {
    use super::IdentifierPath;

    #[test]
    fn canonicalize() {
        let path = IdentifierPath::from("a.super.b.foo");
        assert_eq!(path.canonicalize().to_string(), "b.foo");
    }
}
