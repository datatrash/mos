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

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Instruction(Instruction<'a>),
}

fn absolute_xy_indexed<'a>(
    input: Span<'a>,
    register: &'a str,
) -> IResult<Span<'a>, AddressedValue> {
    map(
        terminated(
            terminated(hexdec_u16, ws(tag_no_case(","))),
            ws(tag_no_case(register)),
        ),
        |val: u16| AddressedValue::U16(val),
    )(input)
}

fn absolute_x_indexed(input: Span) -> IResult<Span, AddressedValue> {
    absolute_xy_indexed(input, "x")
}

fn absolute_y_indexed(input: Span) -> IResult<Span, AddressedValue> {
    absolute_xy_indexed(input, "y")
}

fn zp_xy_indexed<'a>(input: Span<'a>, register: &'a str) -> IResult<Span<'a>, AddressedValue> {
    map(
        terminated(
            terminated(hexdec_u8, ws(tag_no_case(","))),
            ws(tag_no_case(register)),
        ),
        |val: u8| AddressedValue::U8(val),
    )(input)
}

fn zp_x_indexed(input: Span) -> IResult<Span, AddressedValue> {
    zp_xy_indexed(input, "x")
}

fn zp_y_indexed(input: Span) -> IResult<Span, AddressedValue> {
    zp_xy_indexed(input, "y")
}

fn constant_u8(input: Span) -> IResult<Span, AddressedValue> {
    map(hexdec_u8, |val: u8| AddressedValue::U8(val))(input)
}

fn constant_u16(input: Span) -> IResult<Span, AddressedValue> {
    map(hexdec_u16, |val: u16| AddressedValue::U16(val))(input)
}

fn indirect_u8(input: Span) -> IResult<Span, AddressedValue> {
    terminated(preceded(ws(tag("(")), constant_u8), ws(tag(")")))(input)
}

fn indirect_u16(input: Span) -> IResult<Span, AddressedValue> {
    terminated(preceded(ws(tag("(")), constant_u16), ws(tag(")")))(input)
}

fn x_indexed_indirect(input: Span) -> IResult<Span, AddressedValue> {
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

fn indirect_y_indexed(input: Span) -> IResult<Span, AddressedValue> {
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
        map(absolute_x_indexed, |val| {
            AddressingMode::AbsoluteXIndexed(val)
        }),
        map(absolute_y_indexed, |val| {
            AddressingMode::AbsoluteYIndexed(val)
        }),
        map(constant_u16, |val| AddressingMode::Absolute(val)),
        map(indirect_u16, |val| AddressingMode::Indirect(val)),
        map(x_indexed_indirect, |val| {
            AddressingMode::XIndexedIndirect(val)
        }),
        map(indirect_y_indexed, |val| {
            AddressingMode::IndirectYIndexed(val)
        }),
        map(preceded(tag("#"), constant_u8), |val| {
            AddressingMode::Immediate(val)
        }),
        map(zp_x_indexed, |val| AddressingMode::ZpXIndexed(val)),
        map(zp_y_indexed, |val| AddressingMode::ZpYIndexed(val)),
        map(constant_u8, |val| AddressingMode::RelativeOrZp(val)),
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

fn parse(input: Span) -> IResult<Span, Vec<Token>> {
    all_consuming(many1(map(instruction, |i| Token::Instruction(i))))(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn can_parse() {
        let asm = Span::new("lda #$0c brk adc #1 cmp #2");
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
        let i = test_instruction("lda #255");
        assert_eq!(i.mnemonic.position.get_column(), 1);
        assert_eq!(i.addressing_mode.position.get_column(), 5);
    }

    #[test]
    fn test_am_absolute_constant() {
        let i = test_instruction("lda $fce2");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Absolute(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_absolute_x_indexed_constant() {
        let i = test_instruction("lda $fce2   , x");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::AbsoluteXIndexed(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_absolute_y_indexed_constant() {
        let i = test_instruction("lda $fce2   , Y");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::AbsoluteYIndexed(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_immediate_constant() {
        let i = test_instruction("lda #255");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Immediate(AddressedValue::U8(255))
        );
    }

    #[test]
    fn test_am_implied() {
        let i = test_instruction("rol");
        assert_eq!(i.addressing_mode.data, AddressingMode::Implied);
    }

    #[test]
    fn test_am_indirect_constant() {
        let i = test_instruction("jsr   (  $fce2 )");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Indirect(AddressedValue::U16(64738))
        );
    }

    #[test]
    fn test_am_x_indexed_indirect() {
        let i = test_instruction("sta ( $  fb , x  )");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::XIndexedIndirect(AddressedValue::U8(0xfb))
        );
    }

    #[test]
    fn test_am_indirect_y_indexed() {
        let i = test_instruction("sta ( $  fb ) , y");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::IndirectYIndexed(AddressedValue::U8(0xfb))
        );
    }

    #[test]
    fn test_am_relative_or_zp() {
        let i = test_instruction("bne $3f");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::RelativeOrZp(AddressedValue::U8(0x3f))
        );
    }

    #[test]
    fn test_am_zp_x_indexed() {
        let i = test_instruction("dec $3f , x");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::ZpXIndexed(AddressedValue::U8(0x3f))
        );
    }

    #[test]
    fn test_am_zp_y_indexed() {
        let i = test_instruction("stx $3f , y");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::ZpYIndexed(AddressedValue::U8(0x3f))
        );
    }

    fn test_instruction(input: &str) -> Instruction {
        let mut tokens = parse(Span::new(input)).unwrap().1;
        let token = tokens.pop().unwrap();
        match token {
            Token::Instruction(i) => i,
            //_ => panic!(),
        }
    }
}
