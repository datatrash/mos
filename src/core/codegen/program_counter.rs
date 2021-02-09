use std::fmt::{Display, Formatter};
use std::ops::{AddAssign, Sub};

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
    pub fn as_u16(&self) -> u16 {
        self.0 as u16
    }
    pub fn as_i64(&self) -> i64 {
        self.0 as i64
    }
}

impl AddAssign<u16> for ProgramCounter {
    fn add_assign(&mut self, rhs: u16) {
        self.0 += rhs;
    }
}

impl Sub for ProgramCounter {
    type Output = i64;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 as i64 - rhs.0 as i64
    }
}

impl Display for ProgramCounter {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "${:04x}", self.0)
    }
}

impl From<usize> for ProgramCounter {
    fn from(pc: usize) -> Self {
        Self(pc as u16)
    }
}

impl From<u16> for ProgramCounter {
    fn from(pc: u16) -> Self {
        Self(pc)
    }
}

impl From<i32> for ProgramCounter {
    fn from(pc: i32) -> Self {
        Self(pc as u16)
    }
}

impl From<i64> for ProgramCounter {
    fn from(pc: i64) -> Self {
        Self(pc as u16)
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

impl From<ProgramCounter> for i64 {
    fn from(pc: ProgramCounter) -> Self {
        pc.0 as i64
    }
}

impl From<&ProgramCounter> for i64 {
    fn from(pc: &ProgramCounter) -> Self {
        pc.0 as i64
    }
}
