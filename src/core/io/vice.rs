use crate::core::codegen::{SymbolTable, SymbolType};
use crate::LINE_ENDING;
use itertools::Itertools;

pub fn to_vice_symbols(table: &SymbolTable) -> String {
    table
        .iter()
        .filter_map(|(path, symbol)| match symbol.ty {
            SymbolType::Label => Some(format!("al C:{:X} .{}", symbol.data.as_i64(), path)),
            _ => None,
        })
        .sorted()
        .join(LINE_ENDING)
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::{Symbol, SymbolData, SymbolTable, SymbolType};
    use crate::core::io::vice::to_vice_symbols;
    use itertools::Itertools;

    #[test]
    fn can_generate_vice_symbols() {
        let mut st = SymbolTable::new();
        st.insert(
            "foo".into(),
            Symbol {
                data: SymbolData::Number(0x1234),
                ty: SymbolType::Label,
                read_only: false,
            },
        );
        st.insert(
            "scope.foo".into(),
            Symbol {
                data: SymbolData::Number(0xcafe),
                ty: SymbolType::Label,
                read_only: false,
            },
        );
        assert_eq!(
            to_vice_symbols(&st).lines().collect_vec(),
            &["al C:1234 .foo", "al C:CAFE .scope.foo"]
        );
    }
}
