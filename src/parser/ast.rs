use super::mnemonic;
use crate::parser::expressions::Expression;

pub type Span<'a> = &'a str;

#[derive(Debug, PartialEq)]
pub enum AddressingMode<'a> {
    AbsoluteOrRelativeOrZp(Expression<'a>),
    Immediate(Expression<'a>),
    ImpliedOrAccumulator,
    Indirect(Expression<'a>),
    IndirectYIndexed(Expression<'a>),
    XIndexed(Expression<'a>),
    XIndexedIndirect(Expression<'a>),
    YIndexed(Expression<'a>),
}

impl<'a> AddressingMode<'a> {
    pub fn value(&self) -> &Expression<'a> {
        match self {
            AddressingMode::AbsoluteOrRelativeOrZp(val) => val,
            AddressingMode::Immediate(val) => val,
            AddressingMode::Indirect(val) => val,
            AddressingMode::IndirectYIndexed(val) => val,
            AddressingMode::XIndexed(val) => val,
            AddressingMode::XIndexedIndirect(val) => val,
            AddressingMode::YIndexed(val) => val,
            _ => panic!(),
        }
    }
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
pub struct Instruction<'a> {
    pub mnemonic: Mnemonic,
    pub addressing_mode: AddressingMode<'a>,
}

impl<'a> Instruction<'a> {
    pub fn new(mnemonic: Mnemonic, addressing_mode: AddressingMode<'a>) -> Self {
        Self {
            mnemonic,
            addressing_mode,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Instruction(Instruction<'a>),
    Label(&'a str),
}
