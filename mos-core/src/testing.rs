use crate::parser::code_map::{CodeMap, Span};
use crate::LINE_ENDING;
use itertools::Itertools;

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

pub fn empty_span() -> Span {
    let mut codemap = CodeMap::new();
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
