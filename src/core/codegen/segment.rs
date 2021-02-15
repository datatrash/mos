use std::ops::Range;

use crate::core::codegen::ProgramCounter;
use crate::core::parser::{ConfigMap, Location};
use crate::errors::MosError;

pub fn require_segment_options_fields(cfg: &ConfigMap, location: &Location) -> Vec<MosError> {
    let mut errors = cfg.require(&["name", "start"], location.clone());
    errors.extend(cfg.require_single_identifier(&["name"], location.clone()));
    errors
}

pub struct Segment {
    data: [u8; 65536],
    range: Option<Range<u16>>,
    pc: ProgramCounter,
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
            initial_pc: ProgramCounter::new(0x2000u16),
            write: true,
            target_address: ProgramCounter::new(0x2000u16),
        }
    }
}

impl Segment {
    pub(crate) fn new(options: SegmentOptions) -> Self {
        Self {
            data: [0; 65536],
            range: None,
            pc: options.initial_pc,
            options,
        }
    }

    pub(crate) fn options(&self) -> &SegmentOptions {
        &self.options
    }

    pub(crate) fn set_current_pc<PC: Into<ProgramCounter>>(&mut self, pc: PC) {
        self.pc = pc.into();
    }

    pub(crate) fn set(&mut self, bytes: &[u8]) -> ProgramCounter {
        if self.range.is_none() {
            self.range = Some(self.pc.as_u16()..self.pc.as_u16());
        }

        let range = self.range.as_mut().unwrap();

        if self.pc.as_u16() < range.start {
            range.start = self.pc.as_u16();
        }

        let index = self.pc.as_usize();
        let length = bytes.len();

        for (offset, byte) in bytes.iter().enumerate() {
            self.data[index + offset] = *byte;
        }

        self.pc += length as u16;

        if self.pc.as_u16() > range.end {
            range.end = self.pc.as_u16();
        }

        self.pc
    }

    pub(crate) fn data<R: Into<Range<u16>>>(&self, range: R) -> &[u8] {
        let range = range.into();
        &self.data[Range {
            start: range.start as usize,
            end: range.end as usize,
        }]
    }

    pub(crate) fn range(&self) -> &Option<Range<u16>> {
        &self.range
    }

    pub(crate) fn target_range(&self) -> Option<Range<u16>> {
        self.range.as_ref().map(|range| {
            let offset = self.options.target_address - self.options.initial_pc;
            Range {
                start: (range.start as i64 + offset) as u16,
                end: (range.end as i64 + offset) as u16,
            }
        })
    }

    pub(crate) fn range_data(&self) -> &[u8] {
        match &self.range {
            Some(range) => {
                &self.data[Range {
                    start: range.start as usize,
                    end: range.end as usize,
                }]
            }
            None => &[],
        }
    }

    pub(crate) fn current_pc(&self) -> ProgramCounter {
        self.pc
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::segment::{Segment, SegmentOptions};

    #[test]
    fn can_add_data() {
        let mut seg = Segment::new(SegmentOptions {
            initial_pc: 0xc000u16.into(),
            target_address: 0xb000u16.into(),
            ..Default::default()
        });
        let new_pc = seg.set(&[1, 2, 3]);
        assert_eq!(new_pc, 0xc003.into());
        assert_eq!(seg.current_pc(), new_pc);
        assert_eq!(seg.data(0xc000..0xc003), &[1, 2, 3]);
        assert_eq!(seg.range_data(), &[1, 2, 3]);
        assert_eq!(seg.range(), &Some(0xc000..0xc003));
        assert_eq!(seg.target_range(), Some(0xb000..0xb003));

        seg.set_current_pc(0x2000);
        let new_pc = seg.set(&[4]);
        assert_eq!(new_pc, 0x2001.into());
        assert_eq!(seg.current_pc(), new_pc);
        assert_eq!(seg.data(0xc000..0xc003), &[1, 2, 3]);
        assert_eq!(seg.data(0x2000..0x2001), &[4]);
        assert_eq!(seg.range(), &Some(0x2000..0xc003));
        assert_eq!(seg.target_range(), Some(0x1000..0xb003));
    }
}
