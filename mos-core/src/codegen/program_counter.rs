use std::fmt::{Display, Formatter, UpperHex};
use std::ops::{Add, Deref, Range, Sub};

/// A simple newtype that wraps a program counter
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy, PartialOrd)]
pub struct ProgramCounter(usize);

impl ProgramCounter {
    pub fn new(pc: usize) -> Self {
        Self(pc)
    }

    pub fn to_le_bytes(self) -> [u8; 2] {
        (self.0 as u16).to_le_bytes()
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }

    pub fn as_u16(&self) -> u16 {
        self.0 as u16
    }

    pub fn as_i64(&self) -> i64 {
        self.0 as i64
    }

    pub fn as_empty_range(&self) -> Range<usize> {
        self.0..self.0
    }
}

impl Add<usize> for ProgramCounter {
    type Output = ProgramCounter;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl Add for ProgramCounter {
    type Output = ProgramCounter;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub for ProgramCounter {
    type Output = ProgramCounter;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Deref for ProgramCounter {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for ProgramCounter {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "${:04x}", self.0)
    }
}

impl UpperHex for ProgramCounter {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        UpperHex::fmt(&self.0, f)
    }
}

impl From<usize> for ProgramCounter {
    fn from(val: usize) -> Self {
        Self(val)
    }
}

impl From<ProgramCounter> for usize {
    fn from(val: ProgramCounter) -> Self {
        val.0
    }
}

impl From<i32> for ProgramCounter {
    fn from(val: i32) -> Self {
        Self(val as usize)
    }
}

impl From<i64> for ProgramCounter {
    fn from(val: i64) -> Self {
        Self(val as usize)
    }
}
