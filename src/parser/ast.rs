use nom_locate::LocatedSpan;

use super::mnemonic;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub enum Operand<'a> {
    U8(u8),
    U16(u16),
    Label(&'a str),
}

impl<'a> Operand<'a> {
    pub fn try_u8(&self) -> Option<u8> {
        match &self {
            Operand::U8(val) => Some(*val),
            _ => None,
        }
    }

    pub fn try_u16(&self) -> Option<u16> {
        match &self {
            Operand::U16(val) => Some(*val),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AddressingMode<'a> {
    Absolute(Operand<'a>),
    AbsoluteXIndexed(Operand<'a>),
    AbsoluteYIndexed(Operand<'a>),
    Immediate(Operand<'a>),
    ImpliedOrAccumulator,
    Indirect(Operand<'a>),
    IndirectYIndexed(Operand<'a>),
    RelativeOrZp(Operand<'a>),
    XIndexedIndirect(Operand<'a>),
    ZpXIndexed(Operand<'a>),
    ZpYIndexed(Operand<'a>),
}

impl<'a> AddressingMode<'a> {
    pub fn value(&self) -> &Operand<'a> {
        match self {
            AddressingMode::Absolute(val) => val,
            AddressingMode::AbsoluteXIndexed(val) => val,
            AddressingMode::AbsoluteYIndexed(val) => val,
            AddressingMode::Immediate(val) => val,
            AddressingMode::Indirect(val) => val,
            AddressingMode::IndirectYIndexed(val) => val,
            AddressingMode::RelativeOrZp(val) => val,
            AddressingMode::XIndexedIndirect(val) => val,
            AddressingMode::ZpXIndexed(val) => val,
            AddressingMode::ZpYIndexed(val) => val,
            _ => panic!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LocatedAddressingMode<'a> {
    pub position: Span<'a>,
    pub data: AddressingMode<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Mnemonic {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

#[derive(Debug, PartialEq)]
pub struct LocatedMnemonic<'a> {
    pub position: Span<'a>,
    pub data: Mnemonic,
}

#[derive(Debug, PartialEq)]
pub struct Instruction<'a> {
    pub mnemonic: LocatedMnemonic<'a>,
    pub addressing_mode: LocatedAddressingMode<'a>,
}

impl<'a> Instruction<'a> {
    pub fn new(mnemonic: Mnemonic, am: AddressingMode<'a>) -> Self {
        Self {
            mnemonic: LocatedMnemonic {
                position: Span::new(""),
                data: mnemonic,
            },
            addressing_mode: LocatedAddressingMode {
                position: Span::new(""),
                data: am,
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Instruction(Instruction<'a>),
    Label(&'a str),
}
