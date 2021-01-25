use std::ops::Range;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ProgramCounter(u16);

impl ProgramCounter {
    pub fn new(pc: u16) -> Self {
        Self(pc)
    }

    pub fn to_le_bytes(&self) -> [u8; 2] {
        self.0.to_le_bytes()
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl From<u16> for ProgramCounter {
    fn from(pc: u16) -> Self {
        Self(pc)
    }
}

impl From<ProgramCounter> for usize {
    fn from(pc: ProgramCounter) -> Self {
        pc.0 as usize
    }
}

impl From<&ProgramCounter> for usize {
    fn from(pc: &ProgramCounter) -> Self {
        pc.0 as usize
    }
}

pub struct Segment {
    data: [u8; 65536],
    range: Range<u16>,
    pc: ProgramCounter,
}

impl Segment {
    pub(crate) fn new<PC: Into<ProgramCounter>>(initial_pc: PC) -> Self {
        let initial_pc = initial_pc.into();
        Self {
            data: [0; 65536],
            range: Range {
                start: initial_pc.0,
                end: initial_pc.0,
            },
            pc: initial_pc,
        }
    }

    pub(crate) fn set_current_pc<PC: Into<ProgramCounter>>(&mut self, pc: PC) {
        self.pc = pc.into();
    }

    pub(crate) fn set(&mut self, bytes: &[u8]) -> ProgramCounter {
        if self.pc.0 < self.range.start {
            self.range.start = self.pc.0;
        }

        let index = self.pc.0 as usize;
        let length = bytes.len();

        for (offset, byte) in bytes.iter().enumerate() {
            self.data[index + offset] = *byte;
        }

        self.pc.0 += length as u16;

        if self.pc.0 > self.range.end {
            self.range.end = self.pc.0;
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

    pub(crate) fn range(&self) -> &Range<u16> {
        &self.range
    }

    pub(crate) fn range_data(&self) -> &[u8] {
        &self.data[Range {
            start: self.range.start as usize,
            end: self.range.end as usize,
        }]
    }

    pub(crate) fn current_pc(&self) -> ProgramCounter {
        self.pc
    }
}

#[cfg(test)]
mod tests {
    use crate::core::codegen::segment::Segment;

    #[test]
    fn can_add_data() {
        let mut seg = Segment::new(0xc000);
        let new_pc = seg.set(&[1, 2, 3]);
        assert_eq!(new_pc, 0xc003.into());
        assert_eq!(seg.current_pc(), new_pc);
        assert_eq!(seg.data(0xc000..0xc003), &[1, 2, 3]);
        assert_eq!(seg.range_data(), &[1, 2, 3]);
        assert_eq!(seg.range(), &(0xc000..0xc003));

        seg.set_current_pc(0x2000);
        let new_pc = seg.set(&[4]);
        assert_eq!(new_pc, 0x2001.into());
        assert_eq!(seg.current_pc(), new_pc);
        assert_eq!(seg.data(0xc000..0xc003), &[1, 2, 3]);
        assert_eq!(seg.data(0x2000..0x2001), &[4]);
        assert_eq!(seg.range(), &(0x2000..0xc003));
    }
}
