#![allow(dead_code, unused_imports)]

pub use ast::*;

use mnemonic::*;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, tag_no_case};
use nom::character::complete::{char, newline};
use nom::combinator::{all_consuming, eof, map, value};
use nom::error::ParseError;
use nom::lib::std::ops::Add;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;
use nom_locate::position;
use numbers::*;
use whitespace::*;

mod ast;
mod mnemonic;
mod numbers;
mod whitespace;

fn addressed_value_u8(input: Span) -> IResult<Span, AddressedValue> {
    alt((
        map(hexdec_u8, AddressedValue::U8),
        map(identifier, |val: Span| {
            AddressedValue::Label(val.fragment())
        }),
    ))(input)
}

fn addressed_value_u16(input: Span) -> IResult<Span, AddressedValue> {
    alt((
        map(hexdec_u16, AddressedValue::U16),
        map(identifier, |val: Span| {
            AddressedValue::Label(val.fragment())
        }),
    ))(input)
}

fn absolute_xy_indexed<'a>(
    input: Span<'a>,
    register: &'a str,
) -> IResult<Span<'a>, AddressedValue<'a>> {
    terminated(
        terminated(addressed_value_u16, ws(tag_no_case(","))),
        ws(tag_no_case(register)),
    )(input)
}

fn absolute_x_indexed(input: Span) -> IResult<Span, AddressedValue> {
    absolute_xy_indexed(input, "x")
}

fn absolute_y_indexed(input: Span) -> IResult<Span, AddressedValue> {
    absolute_xy_indexed(input, "y")
}

fn zp_xy_indexed<'a>(input: Span<'a>, register: &'a str) -> IResult<Span<'a>, AddressedValue<'a>> {
    terminated(
        terminated(addressed_value_u8, ws(tag_no_case(","))),
        ws(tag_no_case(register)),
    )(input)
}

fn zp_x_indexed(input: Span) -> IResult<Span, AddressedValue> {
    zp_xy_indexed(input, "x")
}

fn zp_y_indexed(input: Span) -> IResult<Span, AddressedValue> {
    zp_xy_indexed(input, "y")
}

fn indirect_u8(input: Span) -> IResult<Span, AddressedValue> {
    terminated(preceded(ws(tag("(")), addressed_value_u8), ws(tag(")")))(input)
}

fn indirect_u16(input: Span) -> IResult<Span, AddressedValue> {
    terminated(preceded(ws(tag("(")), addressed_value_u16), ws(tag(")")))(input)
}

fn x_indexed_indirect(input: Span) -> IResult<Span, AddressedValue> {
    terminated(
        preceded(
            ws(tag("(")),
            terminated(
                terminated(addressed_value_u8, ws(tag_no_case(","))),
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
            terminated(
                terminated(addressed_value_u8, ws(tag_no_case(")"))),
                ws(tag(",")),
            ),
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
        map(x_indexed_indirect, |val| {
            AddressingMode::XIndexedIndirect(val)
        }),
        map(indirect_y_indexed, |val| {
            AddressingMode::IndirectYIndexed(val)
        }),
        map(preceded(tag("#"), addressed_value_u8), |val| {
            AddressingMode::Immediate(val)
        }),
        map(zp_x_indexed, AddressingMode::ZpXIndexed),
        map(zp_y_indexed, AddressingMode::ZpYIndexed),
        map(indirect_u16, AddressingMode::Indirect),
        map(hexdec_u8, |val| {
            AddressingMode::RelativeOrZp(AddressedValue::U8(val))
        }),
        map(addressed_value_u16, AddressingMode::Absolute),
        map(tag(""), |_| AddressingMode::ImpliedOrAccumulator),
    ))(input)?;
    Ok((input, LocatedAddressingMode { position, data }))
}

fn instruction(input: Span) -> IResult<Span, Instruction> {
    map(
        tuple((ws(parse_mnemonic), ws(addressing_mode))),
        |(mnemonic, addressing_mode)| Instruction {
            mnemonic,
            addressing_mode,
        },
    )(input)
}

fn label(input: Span) -> IResult<Span, Span> {
    map(ws(terminated(identifier, char(':'))), |id| id)(input)
}

pub(crate) fn parse<'a, S: Into<Span<'a>>>(input: S) -> IResult<Span<'a>, Vec<Token<'a>>> {
    all_consuming(many1(alt((
        map(tuple((instruction, eof_or_eol())), |(i, _)| {
            Token::Instruction(i)
        }),
        map(label, |s| Token::Label(s.fragment())),
    ))))(input.into())
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn cannot_parse_multiple_statements_on_line() {
        let result = parse("lda #$0c brk adc #1 cmp #2");
        let _ = result.unwrap_err();
    }

    #[test]
    fn can_parse_with_comments() {
        let result = parse("lda #$0c /*foo!*/\nbrk");
        let (remaining, parsed) = result.unwrap();
        assert_eq!(remaining.len(), 0);
        assert_eq!(parsed.len(), 2);
    }

    #[test]
    fn can_fail_parsing() {
        let result = parse("invalid");
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
    fn test_am_absolute_constant_label() {
        let i = test_instruction("lda foo");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Absolute(AddressedValue::Label("foo"))
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
    fn test_am_absolute_x_indexed_constant_label() {
        let i = test_instruction("lda foo   , x");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::AbsoluteXIndexed(AddressedValue::Label("foo"))
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
    fn test_am_absolute_y_indexed_constant_label() {
        let i = test_instruction("lda foo   , Y");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::AbsoluteYIndexed(AddressedValue::Label("foo"))
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
    fn test_am_immediate_constant_label() {
        let i = test_instruction("lda #foo");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Immediate(AddressedValue::Label("foo"))
        );
    }

    #[test]
    fn test_am_implied() {
        let i = test_instruction("rol");
        assert_eq!(i.addressing_mode.data, AddressingMode::ImpliedOrAccumulator);
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
    fn test_am_indirect_constant_local() {
        let i = test_instruction("jsr   (  foo )");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Indirect(AddressedValue::Label("foo"))
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
    fn test_am_x_indexed_indirect_label() {
        let i = test_instruction("sta ( foo , x  )");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::XIndexedIndirect(AddressedValue::Label("foo"))
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
    fn test_am_indirect_y_indexed_label() {
        let i = test_instruction("sta ( foo ) , y");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::IndirectYIndexed(AddressedValue::Label("foo"))
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
    fn test_am_relative_or_zp_label() {
        let i = test_instruction("bne foo");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::Absolute(AddressedValue::Label("foo"))
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
    fn test_am_zp_x_indexed_label() {
        let i = test_instruction("dec foo , x");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::AbsoluteXIndexed(AddressedValue::Label("foo"))
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

    #[test]
    fn test_am_zp_y_indexed_label() {
        let i = test_instruction("stx foo , y");
        assert_eq!(
            i.addressing_mode.data,
            AddressingMode::AbsoluteYIndexed(AddressedValue::Label("foo"))
        );
    }

    #[test]
    fn test_label() {
        let tokens = parse("      my_label:  nop").unwrap().1;
        let token = tokens.into_iter().nth(0).unwrap();
        match token {
            Token::Label(i) => assert_eq!(i, "my_label"),
            _ => panic!(),
        }
    }

    fn test_instruction(input: &str) -> Instruction {
        let mut tokens = parse(Span::new(input)).unwrap().1;
        let token = tokens.pop().unwrap();
        match token {
            Token::Instruction(i) => i,
            _ => panic!(),
        }
    }
}
