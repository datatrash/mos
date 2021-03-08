#![allow(dead_code)]
use crate::core::parser::ParseTree;

impl ParseTree {
    pub fn trace(&self) {
        log::trace!("==== ParseTree AST trace start ================================");
        for item in self.tokens() {
            log::trace!("{:#?}", item);
        }
        log::trace!("==== ParseTree AST trace end ==================================");
    }
}
