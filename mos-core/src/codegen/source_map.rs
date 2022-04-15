use crate::codegen::symbols::SymbolIndex;
use crate::codegen::ProgramCounter;
use crate::parser::code_map::{CodeMap, Span};
use std::ops::Range;

#[derive(Debug, Default)]
pub struct SourceMap {
    offsets: Vec<SourceMapOffset>,
}

#[derive(Debug, PartialEq)]
pub struct SourceMapOffset {
    pub scope: SymbolIndex,
    pub span: Span,
    pub pc: Range<usize>,
}

impl SourceMap {
    pub fn clear(&mut self) {
        self.offsets.clear();
    }

    pub fn offsets(&self) -> &Vec<SourceMapOffset> {
        &self.offsets
    }

    pub fn add(&mut self, scope: SymbolIndex, span: Span, pc: ProgramCounter, len: usize) {
        let offset = SourceMapOffset {
            scope,
            span,
            pc: pc.as_usize()..(pc.as_usize() + len),
        };
        self.offsets.push(offset);
    }

    pub fn address_to_offset<PC: Into<ProgramCounter>>(&self, pc: PC) -> Option<&SourceMapOffset> {
        let pc = pc.into().as_usize();
        self.offsets
            .iter()
            .find(|offset| pc >= offset.pc.start && pc < offset.pc.end)
    }

    // A single line may map to multiple addresses, since it could be an import compiled with different parameters
    pub fn line_col_to_offsets<C: Into<Option<usize>>>(
        &self,
        code_map: &CodeMap,
        filename: &str,
        line: usize,
        column: C,
    ) -> Vec<&SourceMapOffset> {
        let column = column.into();
        self.offsets
            .iter()
            .filter(|o| {
                let sl = code_map.look_up_span(o.span);
                if sl.file.name() != filename {
                    return false;
                }

                // Somewhere within begin and end, so fine regardless of column
                if line > sl.begin.line && line < sl.end.line {
                    return true;
                }

                match column {
                    Some(column) => {
                        // Begin and end on same line
                        if line == sl.begin.line && line == sl.end.line {
                            // So make sure our column is in the middle
                            if column >= sl.begin.column && column < sl.end.column {
                                return true;
                            }
                        } else {
                            // Begin and end on different lines, so we need to be either after the beginning column
                            // or before the ending column
                            if (line == sl.begin.line && column >= sl.begin.column)
                                || (line == sl.end.line && column < sl.end.column)
                            {
                                return true;
                            }
                        }
                    }
                    None => {
                        // No column specified, so we're fine if we're at the beginning line
                        if line == sl.begin.line {
                            return true;
                        }
                    }
                }

                false
            })
            .collect()
    }

    pub fn move_offsets(&mut self, scope: SymbolIndex, new_scope: SymbolIndex, new_span: Span) {
        log::trace!(
            "Trying to move offset from scope '{:?}' to scope '{:?}'",
            scope,
            new_scope
        );
        self.offsets.iter_mut().for_each(|offset| {
            if offset.scope == scope {
                log::trace!("Moved");
                offset.scope = new_scope;
                offset.span = new_span;
            }
        });
    }
}
