/// Contains everything related to transforming the [AST](parser::ast) into actual code
pub mod codegen;

/// Contains code related to IO, file formats, and so on
pub mod io;

/// Parses source files and translates them into a stream of [parser::Token]
pub mod parser;

/// Everything related to Commodore handling
pub mod cbm;
