#![allow(dead_code, unused_imports)]

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{all_consuming, map};
use nom::multi::{many0, many1};
use nom::sequence::{preceded, terminated, tuple};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

use mnemonic::*;
use parse_tools::*;

mod mnemonic;
mod parse_tools;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
enum AddressedValue {
    U8(u8),
    U16(u16),
    Label(String),
}

#[derive(Debug, PartialEq)]
enum AddressingMode {
    Accumulator,
    Absolute(AddressedValue),
    AbsoluteXIndexed(AddressedValue),
    AbsoluteYIndexed(AddressedValue),
    Immediate(AddressedValue),
    Implied,
    Indirect(AddressedValue),
    IndirectYIndexed(AddressedValue),
    RelativeOrZp(AddressedValue),
    XIndexedIndirect(AddressedValue),
    ZpXIndexed(AddressedValue),
    ZpYIndexed(AddressedValue),
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

fn absolute_xy_indexed<'a>(input: Span<'a>, register: &'a str) -> IResult<Span<'a>, u16> {
    terminated(
        terminated(hexdec_u16, ws(tag_no_case(","))),
        ws(tag_no_case(register)),
    )(input)
}

fn absolute_x_indexed(input: Span) -> IResult<Span, u16> {
    absolute_xy_indexed(input, "x")
}

fn absolute_y_indexed(input: Span) -> IResult<Span, u16> {
    absolute_xy_indexed(input, "y")
}

fn zp_xy_indexed<'a>(input: Span<'a>, register: &'a str) -> IResult<Span<'a>, u8> {
    terminated(
        terminated(hexdec_u8, ws(tag_no_case(","))),
        ws(tag_no_case(register)),
    )(input)
}

fn zp_x_indexed(input: Span) -> IResult<Span, u8> {
    zp_xy_indexed(input, "x")
}

fn zp_y_indexed(input: Span) -> IResult<Span, u8> {
    zp_xy_indexed(input, "y")
}

fn constant_u8(input: Span) -> IResult<Span, u8> {
    hexdec_u8(input)
}

fn constant_u16(input: Span) -> IResult<Span, u16> {
    hexdec_u16(input)
}

fn indirect_u8(input: Span) -> IResult<Span, u8> {
    terminated(preceded(ws(tag("(")), constant_u8), ws(tag(")")))(input)
}

fn indirect_u16(input: Span) -> IResult<Span, u16> {
    terminated(preceded(ws(tag("(")), constant_u16), ws(tag(")")))(input)
}

fn x_indexed_indirect(input: Span) -> IResult<Span, u8> {
    terminated(
        preceded(
            ws(tag("(")),
            terminated(
                terminated(constant_u8, ws(tag_no_case(","))),
                ws(tag_no_case("x")),
            ),
        ),
        ws(tag(")")),
    )(input)
}

fn indirect_y_indexed(input: Span) -> IResult<Span, u8> {
    terminated(
        preceded(
            ws(tag("(")),
            terminated(terminated(constant_u8, ws(tag_no_case(")"))), ws(tag(","))),
        ),
        ws(tag_no_case("y")),
    )(input)
}

fn addressing_mode(input: Span) -> IResult<Span, LocatedAddressingMode> {
    let (input, position) = position(input)?;
    let (input, data) = alt((
        map(absolute_x_indexed, |val: u16| {
            AddressingMode::AbsoluteXIndexed(AddressedValue::U16(val))
        }),
        map(absolute_y_indexed, |val: u16| {
            AddressingMode::AbsoluteYIndexed(AddressedValue::U16(val))
        }),
        map(constant_u16, |val: u16| {
            AddressingMode::Absolute(AddressedValue::U16(val))
        }),
        map(indirect_u16, |val: u16| {
            AddressingMode::Indirect(AddressedValue::U16(val))
        }),
        map(x_indexed_indirect, |val: u8| {
            AddressingMode::XIndexedIndirect(AddressedValue::U8(val))
        }),
        map(indirect_y_indexed, |val: u8| {
            AddressingMode::IndirectYIndexed(AddressedValue::U8(val))
        }),
        map(preceded(tag("#"), constant_u8), |val: u8| {
            AddressingMode::Immediate(AddressedValue::U8(val))
        }),
        map(zp_x_indexed, |val: u8| {
            AddressingMode::ZpXIndexed(AddressedValue::U8(val))
        }),
        map(zp_y_indexed, |val: u8| {
            AddressingMode::ZpYIndexed(AddressedValue::U8(val))
        }),
        map(constant_u8, |val: u8| {
            AddressingMode::RelativeOrZp(AddressedValue::U8(val))
        }),
        map(tag(""), |_| AddressingMode::Implied),
    ))(input)?;
    Ok((input, LocatedAddressingMode { position, data }))
}

fn instruction(input: Span) -> IResult<Span, Instruction> {
    map(
        tuple((ws(Mnemonic::parse), ws(addressing_mode))),
        |(mnemonic, addressing_mode)| Instruction {
            mnemonic,
            addressing_mode,
        },
    )(input)
}

fn parse(input: Span) -> IResult<Span, Vec<Instruction>> {
    all_consuming(many1(instruction))(input)
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
    fn test_am_absolute_constant() {
        let i = parse(Span::new("lda $fce2")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::Absolute(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_absolute_x_indexed_constant() {
        let i = parse(Span::new("lda $fce2   , x")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::AbsoluteXIndexed(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_absolute_y_indexed_constant() {
        let i = parse(Span::new("lda $fce2   , Y")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::AbsoluteYIndexed(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_immediate_constant() {
        let i = parse(Span::new("lda #255")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::Immediate(AddressedValue::U8(255))
        );
    }

    #[test]
    fn test_am_implied() {
        let i = parse(Span::new("rol")).unwrap().1;
        assert_eq!(i[0].addressing_mode.data, AddressingMode::Implied);
    }

    #[test]
    fn test_am_indirect_constant() {
        let i = parse(Span::new("jsr   (  $fce2 )")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::Indirect(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_x_indexed_indirect() {
        let i = parse(Span::new("sta ( $  fb , x  )")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::XIndexedIndirect(AddressedValue::U8(0xfb))
        );
    }

    #[test]
    fn test_am_indirect_y_indexed() {
        let i = parse(Span::new("sta ( $  fb ) , y")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::IndirectYIndexed(AddressedValue::U8(0xfb))
        );
    }

    #[test]
    fn test_am_relative_or_zp() {
        let i = parse(Span::new("bne $3f")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::RelativeOrZp(AddressedValue::U8(0x3f))
        );
    }

    #[test]
    fn test_am_zp_x_indexed() {
        let i = parse(Span::new("dec $3f , x")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::ZpXIndexed(AddressedValue::U8(0x3f))
        );
    }

    #[test]
    fn test_am_zp_y_indexed() {
        let i = parse(Span::new("stx $3f , y")).unwrap().1;
        assert_eq!(
            i[0].addressing_mode.data,
            AddressingMode::ZpYIndexed(AddressedValue::U8(0x3f))
        );
    }
}
