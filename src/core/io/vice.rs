use crate::core::codegen::{Symbol, SymbolTable};
use crate::LINE_ENDING;
use itertools::Itertools;

pub fn to_vice_symbols(table: &SymbolTable) -> String {
    table
        .symbols()
        .iter()
        .filter_map(|(path, symbol)| match symbol {
            Symbol::Label(pc) => Some(format!("al C:{:X} .{}", pc, path)),
            _ => None,
        })
        .sorted()
        .join(LINE_ENDING)
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::{CodegenResult, Symbol, SymbolTable};
    use crate::core::io::vice::to_vice_symbols;
    use crate::testing::empty_span;
    use itertools::Itertools;

    #[test]
    fn can_generate_vice_symbols() -> CodegenResult<()> {
        let mut st = SymbolTable::new();
        st.register("foo", Symbol::Label(0x1234.into()), &empty_span(), false)?;
        st.register(
            "scope.foo",
            Symbol::Label(0xCAFE.into()),
            &empty_span(),
            false,
        )?;
        assert_eq!(
            to_vice_symbols(&st).lines().collect_vec(),
            &["al C:1234 .foo", "al C:CAFE .scope.foo"]
        );
        Ok(())
    }
}
