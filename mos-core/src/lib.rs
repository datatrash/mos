/// Contains everything related to transforming the [AST](parser::ast) into actual code
pub mod codegen;

/// Contains code related to IO, file formats, and so on
pub mod io;

/// Parses source files and translates them into a stream of [parser::Token]
pub mod parser;

/// Everything related to Commodore handling
pub mod cbm;

/// Some testing helpers
#[cfg(test)]
pub mod testing;

/// The main error and result types
pub mod errors;
/// Source code formatting
pub mod formatting;

/// Path to the MOS user guide
pub const GUIDE_URL: &str = "https://mos.datatra.sh/guide";

#[cfg(windows)]
/// A platform-specific newline.
pub const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
/// A platform-specific newline
pub const LINE_ENDING: &str = "\n";
