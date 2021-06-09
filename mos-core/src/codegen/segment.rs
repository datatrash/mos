#![allow(dead_code)]
use crate::codegen::ProgramCounter;
use crate::parser::Identifier;
use once_cell::sync::OnceCell;
use std::ops::Range;

pub struct Segment {
    pc: ProgramCounter,
    data: Vec<u8>,
    range: Range<usize>,
    options: SegmentOptions,
}

pub struct SegmentOptions {
    pub bank: Option<Identifier>,
    pub initial_pc: ProgramCounter,
    pub write: bool,
    pub target_address: ProgramCounter,
}

impl SegmentOptions {
    pub fn bank_or_default(&self) -> Identifier {
        self.bank.clone().unwrap_or_else(|| "default".into())
    }
}

impl Default for SegmentOptions {
    fn default() -> Self {
        Self {
            bank: None,
            initial_pc: 0x2000.into(),
            write: true,
            target_address: 0x2000.into(),
        }
    }
}

static EMPTY_DATA: OnceCell<Vec<u8>> = OnceCell::new();

impl Segment {
    pub fn new(options: SegmentOptions) -> Self {
        let pc = options.initial_pc;
        let range = pc.as_empty_range();

        Self {
            pc,
            data: vec![],
            range,
            options,
        }
    }

    pub fn reset(&mut self) {
        self.pc = self.options.initial_pc;
        self.range = self.pc.as_empty_range();
        self.data = vec![];
    }

    pub fn pc(&self) -> ProgramCounter {
        self.pc
    }

    pub fn set_pc(&mut self, pc: impl Into<ProgramCounter>) {
        self.pc = pc.into();
    }

    pub fn target_pc(&self) -> ProgramCounter {
        ((self.pc.as_i64() + self.target_offset()) as usize).into()
    }

    pub fn options(&self) -> &SegmentOptions {
        &self.options
    }

    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn target_offset(&self) -> i64 {
        self.options.target_address.as_i64() - self.options.initial_pc.as_i64()
    }

    pub fn range_data(&self) -> &[u8] {
        if self.data.is_empty() {
            EMPTY_DATA.get_or_init(Vec::new)
        } else {
            &self.data[self.range()]
        }
    }

    pub fn emit(&mut self, bytes: &[u8]) -> bool {
        let start = self.pc;
        let end = self.pc + bytes.len();
        if start.as_usize() > 0xffff || end.as_usize() > 0x10000 {
            return false;
        }

        if start.as_usize() < self.range.start || self.data.is_empty() {
            self.range.start = start.as_usize();
            log::trace!("Extending start of range to: {}", self.range.start);
        }
        if end.as_usize() > self.range.end || self.data.is_empty() {
            self.range.end = end.as_usize();
            log::trace!("Extending end of range to: {}", self.range.end);
        }

        if self.data.is_empty() {
            self.data = [0; 65536].into();
        }

        self.data.splice(*start..*end, bytes.to_vec());
        self.pc = end;

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_emit_to_segment() {
        let mut seg = Segment::new(SegmentOptions {
            initial_pc: 0xc000.into(),
            target_address: 0xb000.into(),
            ..Default::default()
        });
        seg.emit(&[1, 2, 3]);
        assert_eq!(seg.pc(), 0xc003.into());
        assert_eq!(seg.data[0xc000..0xc003], [1, 2, 3]);
        assert_eq!(seg.range_data(), &[1, 2, 3]);
        assert_eq!(seg.range(), 0xc000..0xc003);
        assert_eq!(seg.target_offset(), -0x1000);

        seg.pc = 0x2000.into();
        seg.emit(&[4]);
        assert_eq!(seg.pc, 0x2001.into());
        assert_eq!(seg.data[0xc000..0xc003], [1, 2, 3]);
        assert_eq!(seg.data[0x2000..0x2001], [4]);
        assert_eq!(seg.range(), 0x2000..0xc003);
    }
}
