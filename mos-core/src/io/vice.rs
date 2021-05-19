use crate::codegen::{Symbol, SymbolTable, SymbolType};
use crate::LINE_ENDING;
use itertools::Itertools;

pub fn to_vice_symbols(table: &SymbolTable<Symbol>) -> String {
    table
        .all()
        .into_iter()
        .filter_map(|(path, symbol)| match symbol.ty {
            SymbolType::Label => Some(format!("al C:{:X} .{}", symbol.data.as_i64(), path)),
            _ => None,
        })
        .sorted()
        .join(LINE_ENDING)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::SymbolData;
    use itertools::Itertools;

    #[test]
    fn can_generate_vice_symbols() {
        let mut st = SymbolTable::default();
        st.insert(
            st.root,
            "foo",
            Symbol {
                pass_idx: 0,
                data: SymbolData::Number(0x1234),
                ty: SymbolType::Label,
                read_only: false,
            },
        );
        assert_eq!(
            to_vice_symbols(&st).lines().collect_vec(),
            &["al C:1234 .foo"]
        );
    }
}
