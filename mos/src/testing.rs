use itertools::Itertools;
use mos_core::LINE_ENDING;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

pub fn assert_unordered_eq<T>(a: &[T], b: &[T])
where
    T: Debug + Eq + Hash,
{
    let a: HashSet<_> = a.iter().collect();
    let b: HashSet<_> = b.iter().collect();

    assert_eq!(a, b)
}

// Cross-platform eq
pub fn xplat_eq<S: AsRef<str>, T: AsRef<str>>(actual: S, expected: T) {
    // Split the result into lines to work around cross-platform line ending normalization issues
    assert_eq!(
        actual.as_ref().lines().join(LINE_ENDING),
        expected.as_ref().lines().join(LINE_ENDING)
    );
}
