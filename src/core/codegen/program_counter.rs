use derive_more::{Add, Display, From, Into, Sub, UpperHex};

/// A simple newtype that wraps a program counter
#[derive(Debug, PartialEq, Clone, Copy, From, Add, Sub, Display, Into, UpperHex)]
pub struct ProgramCounter(usize);

impl ProgramCounter {
    pub fn new(pc: usize) -> Self {
        Self(pc)
    }

    pub fn to_le_bytes(&self) -> [u8; 2] {
        (self.0 as u16).to_le_bytes()
    }

    pub fn as_i64(&self) -> i64 {
        self.0 as i64
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
