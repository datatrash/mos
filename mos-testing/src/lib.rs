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
