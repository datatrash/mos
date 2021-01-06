#![allow(dead_code, unused_imports)]

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map, all_consuming};
use nom::IResult;
use nom::multi::{many0, many1};
use nom::sequence::{preceded, tuple};
use nom_locate::{LocatedSpan, position};

use mnemonic::*;
use parse_tools::*;

mod mnemonic;
mod parse_tools;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
enum ImmediateValue {
    Constant(u8),
    Label(String),
}

#[derive(Debug, PartialEq)]
enum AddressingMode {
    Accumulator,
    Absolute,
    AbsoluteXIndexed,
    AbsoluteYIndexed,
    Immediate(ImmediateValue),
    Implied,
    XIndexedIndirect,
    IndirectYIndexed,
    Relative,
    Zp,
    ZpXIndexed,
    ZpYIndexed,
}

#[derive(Debug, PartialEq)]
struct LocatedAddressingMode<'a> {
    position: Span<'a>,
    data: AddressingMode,
}

#[derive(Debug, PartialEq)]
struct Instruction<'a> {
    mnemonic: LocatedMnemonic<'a>,
    addressing_mode: LocatedAddressingMode<'a>,
}

fn immediate_byte(data: Span) -> IResult<Span, u8> {
    alt((
        preceded(tag("$"), hex_u8),
        dec_u8
    ))(data)
}

fn addressing_mode(data: Span) -> IResult<Span, LocatedAddressingMode> {
    let (data, position) = position(data)?;
    let (data, mode) = alt((
        map( preceded(tag("#"), immediate_byte), |val: u8| AddressingMode::Immediate(ImmediateValue::Constant(val))),
        map( tag(""), |_| AddressingMode::Implied)
    ))(data)?;
    Ok((data, LocatedAddressingMode { position, data: mode }))
}

fn instruction(data: Span) -> IResult<Span, Instruction> {
    map(
        tuple((ws(Mnemonic::parse), ws(addressing_mode))),
        |(mnemonic, addressing_mode)| {
            Instruction { mnemonic, addressing_mode }
        }
    )(data)
}

fn parse(data: Span) -> IResult<Span, Vec<Instruction>> {
    all_consuming(many1(instruction))(data)
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn can_parse() {
        let asm = Span::new("lda #$0c\nbrk\nadc #1\ncmp #2");
        let result = parse(asm);
        let (remaining, parsed) = result.unwrap();
        assert_eq!(remaining.len(), 0);
        assert_eq!(parsed.len(), 4);
    }

    #[test]
    fn can_fail_parsing() {
        let asm = Span::new("invalid");
        let result = parse(asm);
        let _ = result.unwrap_err();
    }

    #[test]
    fn test_locations() {
        let i = parse(Span::new("lda #255")).unwrap().1;
        assert_eq!(i[0].mnemonic.position.get_column(), 1);
        assert_eq!(i[0].addressing_mode.position.get_column(), 5);
    }

    #[test]
    fn test_am_immediate_constant() {
        let i = parse(Span::new("lda #255")).unwrap().1;
        assert_eq!(i[0].mnemonic.data, Mnemonic::Lda);
        assert_eq!(i[0].addressing_mode.data, AddressingMode::Immediate(ImmediateValue::Constant(255)));
    }
}
