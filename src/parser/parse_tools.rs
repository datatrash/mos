use super::Span;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while_m_n};
use nom::character::complete::multispace0;
use nom::character::complete::{char, multispace1};
use nom::combinator::{map, map_res, value};
use nom::error::ParseError;
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;

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

fn cpp_comment<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>, E> {
    map(value((), pair(tag("//"), is_not("\n\r"))), |_| {
        Span::new("")
    })(input)
}

fn c_comment<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>, E> {
    map(
        value((), tuple((tag("/*"), take_until("*/"), tag("*/")))),
        |_| Span::new(""),
    )(input)
}

pub(super) fn ws<'a, F: 'a, O, E: 'a + ParseError<Span<'a>>>(
    inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
where
    F: Fn(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    let left = many0(alt((multispace1, cpp_comment, c_comment)));
    let right = many0(alt((multispace1, cpp_comment, c_comment)));
    delimited(left, inner, right)
}

#[cfg(test)]
mod tests {
    use super::Span;
    use super::*;
    use nom::character::complete::{alpha1, none_of, not_line_ending};
    use nom::combinator::{map, not};
    use nom::multi::many0;

    #[test]
    fn can_ignore_whitespace() {
        let input = Span::new("   foo   \n   bar");
        let parser: IResult<Span, Vec<Span>> = many0(alt((ws(tag("foo")), ws(tag("bar")))))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.fragment().clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_ignore_cpp_comments() {
        let input = Span::new("   foo // hello  \n   bar");
        let parser: IResult<Span, Vec<Span>> = many0(alt((ws(tag("foo")), ws(tag("bar")))))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.fragment().clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_ignore_nested_c_comments() {
        let input = Span::new("   foo /* he /* llo */   \n   bar");
        let parser: IResult<Span, Vec<Span>> = many0(alt((ws(tag("foo")), ws(tag("bar")))))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.fragment().clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_parse_hex_u8() {
        assert_eq!(hex_u8(Span::new("f")).unwrap().1, 15);
        assert_eq!(hex_u8(Span::new("7f")).unwrap().1, 127);
    }

    #[test]
    fn can_parse_hex_u16() {
        assert_eq!(hex_u16(Span::new("00f")).unwrap().1, 15);
        assert_eq!(hex_u16(Span::new("07f")).unwrap().1, 127);
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
