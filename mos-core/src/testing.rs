use crate::parser::code_map::{CodeMap, Span};
use crate::LINE_ENDING;
use itertools::Itertools;

pub fn empty_span() -> Span {
    let mut codemap = CodeMap::default();
    let f1 = codemap.add_file("test1.rs".to_string(), "abcd\nefghij\nqwerty".to_string());
    f1.span
}

// Cross-platform eq
pub fn xplat_eq<S: AsRef<str>, T: AsRef<str>>(actual: S, expected: T) {
    // Split the result into lines to work around cross-platform line ending normalization issues
    assert_eq!(
        actual.as_ref().lines().join(LINE_ENDING),
        expected.as_ref().lines().join(LINE_ENDING)
    );
}
