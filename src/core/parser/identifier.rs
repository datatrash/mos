use itertools::Itertools;
use std::fmt::{Debug, Display, Formatter};

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
#[derive(Clone, PartialEq, Eq, Hash)]
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

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn is_super(&self) -> bool {
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

impl PartialEq<&str> for Identifier {
    fn eq(&self, other: &&str) -> bool {
        other.eq(&self.0)
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "\"{}\"", self)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A path of multiple identifiers, usually written as being separated by dots (e.g. `foo.bar.baz`)
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IdentifierPath(Vec<Identifier>);

impl Debug for IdentifierPath {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "\"{}\"", self)
    }
}

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

    pub fn pop(&mut self) -> Option<Identifier> {
        self.0.pop()
    }

    pub fn split(mut self) -> (IdentifierPath, Identifier) {
        let id = self.pop().unwrap();
        (self, id)
    }

    pub fn pop_front(&mut self) -> Option<Identifier> {
        match self.0.len() {
            0 => None,
            _ => Some(self.0.remove(0)),
        }
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

    pub fn single(&self) -> &Identifier {
        assert_eq!(self.len(), 1);
        self.0.first().unwrap()
    }
}
