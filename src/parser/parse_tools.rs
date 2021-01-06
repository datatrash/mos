use nom::bytes::complete::{tag, take_while_m_n};
use nom::character::complete::multispace0;
use nom::combinator::map_res;
use nom::error::ParseError;
use nom::sequence::{delimited, preceded};
use nom::IResult;

use super::Span;
use nom::branch::alt;

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn map_hex_u8(input: Span) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input.fragment(), 16)
}

pub(super) fn hex_u8(input: Span) -> IResult<Span, u8> {
    map_res(take_while_m_n(1, 2, is_hex_digit), map_hex_u8)(input)
}

fn map_hex_u16(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.fragment(), 16)
}

pub(super) fn hex_u16(input: Span) -> IResult<Span, u16> {
    map_res(take_while_m_n(3, 4, is_hex_digit), map_hex_u16)(input)
}

fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}

fn from_dec_u8(input: Span) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input.fragment(), 10)
}

fn from_dec_u16(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.fragment(), 10)
}

pub(super) fn dec_u8(input: Span) -> IResult<Span, u8> {
    map_res(take_while_m_n(1, 3, is_dec_digit), from_dec_u8)(input)
}

pub(super) fn dec_u16(input: Span) -> IResult<Span, u16> {
    map_res(take_while_m_n(1, 5, is_dec_digit), from_dec_u16)(input)
}

pub(super) fn hexdec_u8(input: Span) -> IResult<Span, u8> {
    alt((preceded(ws(tag("$")), hex_u8), dec_u8))(input)
}

pub(super) fn hexdec_u16(input: Span) -> IResult<Span, u16> {
    alt((preceded(ws(tag("$")), hex_u16), dec_u16))(input)
}

pub(super) fn ws<'a, F: 'a, O, E: ParseError<Span<'a>>>(
    inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
where
    F: Fn(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

#[cfg(test)]
mod tests {
    use super::Span;
    use super::*;

    #[test]
    fn can_parse_hex_u8() {
        assert_eq!(hex_u8(Span::new("f")).unwrap().1, 15);
        assert_eq!(hex_u8(Span::new("7f")).unwrap().1, 127);
    }

    #[test]
    fn can_parse_hex_u16() {
        assert_eq!(hex_u16(Span::new("f")).unwrap().1, 15);
        assert_eq!(hex_u16(Span::new("7f")).unwrap().1, 127);
        assert_eq!(hex_u16(Span::new("400")).unwrap().1, 1024);
        assert_eq!(hex_u16(Span::new("fce2")).unwrap().1, 64738);
    }

    #[test]
    fn can_parse_dec_u8() {
        assert_eq!(dec_u8(Span::new("5")).unwrap().1, 5);
        assert_eq!(dec_u8(Span::new("25")).unwrap().1, 25);
        assert_eq!(dec_u8(Span::new("255")).unwrap().1, 255);
    }

    #[test]
    fn can_parse_dec_u16() {
        assert_eq!(dec_u16(Span::new("5")).unwrap().1, 5);
        assert_eq!(dec_u16(Span::new("25")).unwrap().1, 25);
        assert_eq!(dec_u16(Span::new("255")).unwrap().1, 255);
        assert_eq!(dec_u16(Span::new("2555")).unwrap().1, 2555);
        assert_eq!(dec_u16(Span::new("25555")).unwrap().1, 25555);
    }
}
