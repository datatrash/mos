use crate::core::codegen::ProgramCounter;
use crate::core::parser::code_map::{CodeMap, LineCol, Span};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug)]
pub struct SourceMap {
    files: HashMap<String, Vec<SourceMapOffset>>,
}

#[derive(Debug, PartialEq)]
pub struct SourceMapOffset {
    pub begin: LineCol,
    pub end: LineCol,
    pub begin_pc: usize,
    pub end_pc: usize,
}

impl SourceMap {
    pub fn clear(&mut self) {
        self.files.clear();
    }

    pub fn add(&mut self, code_map: &CodeMap, span: Span, pc: ProgramCounter, len: usize) {
        let sl = code_map.look_up_span(span);
        let offset = SourceMapOffset {
            begin: sl.begin,
            end: sl.end,
            begin_pc: pc.as_usize(),
            end_pc: pc.as_usize() + len - 1,
        };
        match self.files.entry(sl.file.name().into()) {
            Entry::Occupied(mut e) => {
                e.get_mut().push(offset);
            }
            Entry::Vacant(e) => {
                e.insert(vec![offset]);
            }
        }
    }

    pub fn address_to_offset<PC: Into<ProgramCounter>>(
        &self,
        pc: PC,
    ) -> Option<(&str, &SourceMapOffset)> {
        let pc = pc.into().as_usize();
        for (filename, file_offsets) in &self.files {
            if let Some(o) = file_offsets
                .iter()
                .find(|offset| pc >= offset.begin_pc && pc <= offset.end_pc)
            {
                return Some((filename.as_str(), o));
            }
        }

        None
    }

    // A single line may map to multiple addresses, since it could be an import compiled with different parameters
    pub fn line_col_to_pcs<C: Into<Option<usize>>>(
        &self,
        filename: &str,
        line: usize,
        column: C,
    ) -> Vec<Range<ProgramCounter>> {
        let column = column.into();
        self.files
            .get(filename)
            .map(|offsets| {
                offsets
                    .iter()
                    .filter(|o| {
                        // Somewhere within begin and end, so fine regardless of column
                        if line > o.begin.line && line < o.end.line {
                            return true;
                        }

                        match column {
                            Some(column) => {
                                // Begin and end on same line
                                if line == o.begin.line && line == o.end.line {
                                    // So make sure our column is in the middle
                                    if column >= o.begin.column && column < o.end.column {
                                        return true;
                                    }
                                } else {
                                    // Begin and end on different lines, so we need to be either after the beginning column
                                    // or before the ending column
                                    if (line == o.begin.line && column >= o.begin.column)
                                        || (line == o.end.line && column < o.end.column)
                                    {
                                        return true;
                                    }
                                }
                            }
                            None => {
                                // No column specified, so we're fine if we're at the beginning line
                                if line == o.begin.line {
                                    return true;
                                }
                            }
                        }

                        false
                    })
                    .map(|offset| {
                        ProgramCounter::new(offset.begin_pc)..ProgramCounter::new(offset.end_pc)
                    })
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self {
            files: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::source_map::{SourceMap, SourceMapOffset};
    use crate::core::codegen::ProgramCounter;
    use crate::core::parser::code_map::{CodeMap, LineCol};
    use std::collections::HashMap;

    #[test]
    fn can_add() {
        let map = get_map();

        let mut files = HashMap::new();
        files.insert(
            "test.asm".to_string(),
            vec![
                SourceMapOffset {
                    begin: LineCol { line: 0, column: 0 },
                    end: LineCol { line: 1, column: 0 },
                    begin_pc: 0x1000,
                    end_pc: 0x1001,
                },
                SourceMapOffset {
                    begin: LineCol { line: 1, column: 0 },
                    end: LineCol { line: 2, column: 0 },
                    begin_pc: 0x1002,
                    end_pc: 0x1004,
                },
                SourceMapOffset {
                    begin: LineCol { line: 2, column: 0 },
                    end: LineCol { line: 2, column: 3 },
                    begin_pc: 0x1005,
                    end_pc: 0x1008,
                },
            ],
        );
        assert_eq!(map.files["test.asm"], files["test.asm"]);
    }

    #[test]
    fn can_lookup_offsets() {
        let map = get_map();
        assert_eq!(
            map.address_to_offset(0x1003),
            Some((
                "test.asm",
                &SourceMapOffset {
                    begin: LineCol { line: 1, column: 0 },
                    end: LineCol { line: 2, column: 0 },
                    begin_pc: 0x1002,
                    end_pc: 0x1004,
                }
            ))
        );
    }

    #[test]
    fn can_lookup_pc() {
        let map = get_map();
        assert_eq!(
            map.line_col_to_pcs("test.asm", 1, 2),
            vec![ProgramCounter::new(0x1002)..ProgramCounter::new(0x1004)]
        );
        assert_eq!(
            map.line_col_to_pcs("test.asm", 1, None),
            vec![ProgramCounter::new(0x1002)..ProgramCounter::new(0x1004)]
        );
        assert_eq!(map.line_col_to_pcs("test.asm", 3, 5), vec![]);
    }

    fn get_map() -> SourceMap {
        let mut map = SourceMap::default();
        let mut code_map = CodeMap::new();
        let test_file = code_map.add_file("test.asm".into(), "abc\ndef\nghi".into());
        map.add(&code_map, test_file.line_span(0), 0x1000.into(), 2);
        map.add(&code_map, test_file.line_span(1), 0x1002.into(), 3);
        map.add(&code_map, test_file.line_span(2), 0x1005.into(), 4);
        map
    }
}
