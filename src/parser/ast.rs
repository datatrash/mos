use nom_locate::LocatedSpan;

use super::mnemonic;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;
#[derive(Debug, PartialEq)]
pub(crate) enum AddressedValue<'a> {
    U8(u8),
    U16(u16),
    Label(&'a str),
}

#[derive(Debug, PartialEq)]
pub(crate) enum AddressingMode<'a> {
    Absolute(AddressedValue<'a>),
    AbsoluteXIndexed(AddressedValue<'a>),
    AbsoluteYIndexed(AddressedValue<'a>),
    Immediate(AddressedValue<'a>),
    ImpliedOrAccumulator,
    Indirect(AddressedValue<'a>),
    IndirectYIndexed(AddressedValue<'a>),
    RelativeOrZp(AddressedValue<'a>),
    XIndexedIndirect(AddressedValue<'a>),
    ZpXIndexed(AddressedValue<'a>),
    ZpYIndexed(AddressedValue<'a>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct LocatedAddressingMode<'a> {
    pub(crate) position: Span<'a>,
    pub(crate) data: AddressingMode<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Mnemonic {
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
pub(crate) struct LocatedMnemonic<'a> {
    pub position: Span<'a>,
    pub data: Mnemonic
}

#[derive(Debug, PartialEq)]
pub(crate) struct Instruction<'a> {
    pub(crate) mnemonic: LocatedMnemonic<'a>,
    pub(crate) addressing_mode: LocatedAddressingMode<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Token<'a> {
    Instruction(Instruction<'a>),
    Label(&'a str),
}
