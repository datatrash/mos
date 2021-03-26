use crate::core::parser::ParseTree;

impl ParseTree {
    #[allow(dead_code)]
    pub fn trace(&self) {
        log::trace!("==== ParseTree AST trace start ================================");
        for item in self.main_file().tokens.iter() {
            log::trace!("{:#?}", item);
        }
        log::trace!("==== ParseTree AST trace end ==================================");
    }
}
