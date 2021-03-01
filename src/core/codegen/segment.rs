use std::ops::Range;

use crate::core::codegen::{CodegenError, CodegenResult, DetailedCodegenError, ProgramCounter};
use crate::core::parser::{ConfigMap, ConfigMapValidatorBuilder, ParseTree};
use crate::errors::{MosError, MosResult};
use codemap::Span;
use std::sync::Arc;

pub fn require_segment_options_fields(
    tree: Arc<ParseTree>,
    cfg: &ConfigMap,
    span: &Span,
) -> MosResult<()> {
    let errors = ConfigMapValidatorBuilder::default()
        .require_single_value("name")
        .require("start")
        .allowed("pc")
        .allowed("write")
        .validate(cfg, span);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(MosError::Multiple(
            errors
                .into_iter()
                .map(|e| e.into_mos_error(tree.clone()))
                .collect(),
        ))
    }
}

pub struct Segment {
    name: String,
    data: [u8; 65536],
    range: Option<Range<usize>>,
    pc: usize,
    options: SegmentOptions,
}

pub struct SegmentOptions {
    pub initial_pc: ProgramCounter,
    pub write: bool,
    pub target_address: ProgramCounter,
}

impl Default for SegmentOptions {
    fn default() -> Self {
        Self {
            initial_pc: 0x2000.into(),
            write: true,
            target_address: 0x2000.into(),
        }
    }
}

impl Segment {
    pub(crate) fn new<N: Into<String>>(name: N, options: SegmentOptions) -> Self {
        Self {
            name: name.into(),
            data: [0; 65536],
            range: None,
            pc: options.initial_pc.into(),
            options,
        }
    }

    pub(crate) fn options(&self) -> &SegmentOptions {
        &self.options
    }

    pub(crate) fn set_current_pc<PC: Into<ProgramCounter>>(&mut self, pc: PC) {
        self.pc = usize::from(pc.into());
    }

    pub(crate) fn set(&mut self, span: &Span, bytes: &[u8]) -> CodegenResult<ProgramCounter> {
        if (self.pc + bytes.len() - 1) > 0xffff {
            return Err(CodegenError::new(
                *span,
                DetailedCodegenError::SegmentOutOfRange(self.name.clone(), 0xffff.into()),
            ));
        }

        if self.range.is_none() {
            self.range = Some(self.pc..self.pc);
        }

        let range = self.range.as_mut().unwrap();

        if self.pc < range.start {
            range.start = self.pc;
        }

        let index = self.pc;
        let length = bytes.len();

        for (offset, byte) in bytes.iter().enumerate() {
            self.data[index + offset] = *byte;
        }

        self.pc += length;

        if self.pc > range.end {
            range.end = self.pc;
        }

        Ok(self.pc.into())
    }

    #[cfg(test)]
    pub(crate) fn data<R: Into<Range<usize>>>(&self, range: R) -> &[u8] {
        let range = range.into();
        &self.data[Range {
            start: range.start,
            end: range.end,
        }]
    }

    pub(crate) fn range(&self) -> &Option<Range<usize>> {
        &self.range
    }

    pub(crate) fn target_range(&self) -> Option<Range<usize>> {
        self.range.as_ref().map(|range| {
            let offset = self.options.target_address.as_i64() - self.options.initial_pc.as_i64();
            Range {
                start: (range.start as i64 + offset) as usize,
                end: (range.end as i64 + offset) as usize,
            }
        })
    }

    pub(crate) fn range_data(&self) -> &[u8] {
        match &self.range {
            Some(range) => &self.data[range.clone()],
            None => &[],
        }
    }

    pub(crate) fn current_pc(&self) -> ProgramCounter {
        self.pc.into()
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::segment::{Segment, SegmentOptions};
    use crate::core::codegen::CodegenResult;
    use codemap::{CodeMap, Span};

    #[test]
    fn can_add_data() -> CodegenResult<()> {
        let mut seg = Segment::new(
            "a",
            SegmentOptions {
                initial_pc: 0xc000.into(),
                target_address: 0xb000.into(),
                ..Default::default()
            },
        );
        let new_pc = seg.set(&loc(), &[1, 2, 3])?;
        assert_eq!(new_pc, 0xc003.into());
        assert_eq!(seg.current_pc(), new_pc);
        assert_eq!(seg.data(0xc000..0xc003), &[1, 2, 3]);
        assert_eq!(seg.range_data(), &[1, 2, 3]);
        assert_eq!(seg.range(), &Some(0xc000..0xc003));
        assert_eq!(seg.target_range(), Some(0xb000..0xb003));

        seg.set_current_pc(0x2000);
        let new_pc = seg.set(&loc(), &[4])?;
        assert_eq!(new_pc, 0x2001.into());
        assert_eq!(seg.current_pc(), new_pc);
        assert_eq!(seg.data(0xc000..0xc003), &[1, 2, 3]);
        assert_eq!(seg.data(0x2000..0x2001), &[4]);
        assert_eq!(seg.range(), &Some(0x2000..0xc003));
        assert_eq!(seg.target_range(), Some(0x1000..0xb003));

        Ok(())
    }

    #[test]
    fn cannot_exceed_max_range() -> CodegenResult<()> {
        let mut seg = Segment::new(
            "a",
            SegmentOptions {
                initial_pc: 0xffff.into(),
                target_address: 0xb000.into(),
                ..Default::default()
            },
        );

        // A byte at 0xffff should still be ok
        seg.set(&loc(), &[1])?;

        // Next byte should not
        let err = seg.set(&loc(), &[2]).err().unwrap();
        assert_eq!(err.to_string(), "segment 'a' is out of range: beyond $FFFF");

        Ok(())
    }

    #[test]
    fn cannot_exceed_max_range_with_multiple_bytes() {
        let mut seg = Segment::new(
            "a",
            SegmentOptions {
                initial_pc: 0xffff.into(),
                target_address: 0xb000.into(),
                ..Default::default()
            },
        );

        // Two bytes don't fit anymore
        let err = seg.set(&loc(), &[1, 2]).err().unwrap();
        assert_eq!(err.to_string(), "segment 'a' is out of range: beyond $FFFF");
    }

    fn loc() -> Span {
        let mut codemap = CodeMap::new();
        let f1 = codemap.add_file("test1.rs".to_string(), "abcd\nefghij\nqwerty".to_string());
        f1.span
    }
}
