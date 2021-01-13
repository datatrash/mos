use super::Span;
use crate::parser::whitespace::ws;
use crate::parser::ParseResult;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while, take_while_m_n};
use nom::character::complete::{
    alpha1, alphanumeric1, digit1, hex_digit1, multispace0, newline, space1,
};
use nom::character::complete::{char, multispace1};
use nom::combinator::{eof, map, map_opt, map_res, recognize, value};
use nom::error::{context, Error, ErrorKind, ParseError};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{FindSubstring, IResult, InputLength, InputTake};

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn try_map_dec_u8(input: Span) -> Option<u8> {
    u8::from_str_radix(input, 10).ok()
}
fn try_map_hex_u8(input: Span) -> Option<u8> {
    u8::from_str_radix(input, 16).ok()
}

// Grab the entire number, but fail if it's not a u8
pub(super) fn hex_u8(input: Span) -> ParseResult<Span, u8> {
    map_opt(hex_digit1, try_map_hex_u8)(input)
}

fn map_hex_u16(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 16)
}

// Fail if the number is less than 3 characters (because then the user has explicitly wanted to get a u8)
pub(super) fn hex_u16(input: Span) -> ParseResult<Span, u16> {
    map_res(take_while_m_n(3, 4, is_hex_digit), map_hex_u16)(input)
}

fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}

fn from_dec_u8(input: Span) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 10)
}

fn from_dec_u16(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 10)
}

pub(super) fn dec_u8(input: Span) -> ParseResult<Span, u8> {
    map_opt(digit1, try_map_dec_u8)(input)
}

pub(super) fn dec_u16(input: Span) -> ParseResult<Span, u16> {
    map_res(take_while_m_n(1, 5, is_dec_digit), from_dec_u16)(input)
}

pub(super) fn hexdec_u8(input: Span) -> ParseResult<Span, u8> {
    alt((preceded(ws(tag("$")), hex_u8), dec_u8))(input)
}

pub(super) fn hexdec_u16(input: Span) -> ParseResult<Span, u16> {
    alt((preceded(ws(tag("$")), hex_u16), dec_u16))(input)
}

#[cfg(test)]
mod tests {
    use super::Span;
    use super::*;
    use nom::character::complete::{alpha1, newline, none_of, not_line_ending};
    use nom::combinator::{map, not};
    use nom::multi::{many0, many1};
    use nom::sequence::terminated;

    #[test]
    fn can_parse_hex_u8() {
        assert_eq!(hex_u8("f").unwrap().1, 15);
        assert_eq!(hex_u8("7f").unwrap().1, 127);
        assert_eq!(hex_u8("7ff").is_err(), true);
    }

    #[test]
    fn can_parse_hex_u16() {
        assert_eq!(hex_u16("00f").unwrap().1, 15);
        assert_eq!(hex_u16("07f").unwrap().1, 127);
        assert_eq!(hex_u16("400").unwrap().1, 1024);
        assert_eq!(hex_u16("fce2").unwrap().1, 64738);
    }

    #[test]
    fn can_parse_dec_u8() {
        assert_eq!(dec_u8("5").unwrap().1, 5);
        assert_eq!(dec_u8("25").unwrap().1, 25);
        assert_eq!(dec_u8("255").unwrap().1, 255);
        assert_eq!(dec_u8("2555").is_err(), true);
    }

    #[test]
    fn can_parse_dec_u16() {
        assert_eq!(dec_u16("5").unwrap().1, 5);
        assert_eq!(dec_u16("25").unwrap().1, 25);
        assert_eq!(dec_u16("255").unwrap().1, 255);
        assert_eq!(dec_u16("2555").unwrap().1, 2555);
        assert_eq!(dec_u16("25555").unwrap().1, 25555);
    }
}
