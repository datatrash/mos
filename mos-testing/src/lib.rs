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

pub fn enable_tracing<F: Fn(simple_logger::SimpleLogger) -> simple_logger::SimpleLogger>(
    customizer: F,
) {
    use simple_logger::*;
    let logger = SimpleLogger::new().with_level(log::LevelFilter::Off);
    let logger = customizer(logger);
    logger.init().unwrap();
}

pub fn enable_default_tracing() {
    use simple_logger::*;
    let _ = SimpleLogger::new()
        .with_level(log::LevelFilter::Off)
        .with_module_level("mos", log::LevelFilter::Trace)
        .init();
}
