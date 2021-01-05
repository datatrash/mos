use nom::IResult;
use nom::combinator::map_res;
use nom::bytes::complete::take_while_m_n;
use nom::error::ParseError;
use nom::sequence::delimited;
use nom::character::complete::multispace0;

use super::Span;

pub fn from_hex(input: Span) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input.fragment(), 16)
}

pub fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

pub(super) fn hex_u8(input: Span) -> IResult<Span, u8> {
    map_res(
        take_while_m_n(1, 2, is_hex_digit),
        from_hex
    )(input)
}

pub fn from_dec(input: Span) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input.fragment(), 10)
}

pub fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}

pub(super) fn dec_u8(input: Span) -> IResult<Span, u8> {
    map_res(
        take_while_m_n(1, 3, is_dec_digit),
        from_dec
    )(input)
}

pub(super) fn ws<'a, F: 'a, O, E: ParseError<Span<'a>>>(inner: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
    where
        F: Fn(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    delimited(
        multispace0,
        inner,
        multispace0
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Span;

    #[test]
    fn can_parse_hex_u8() {
        assert_eq!(hex_u8(Span::new("f")).unwrap().1, 15);
        assert_eq!(hex_u8(Span::new("7f")).unwrap().1, 127);
    }

    #[test]
    fn can_parse_dec_u8() {
        assert_eq!(dec_u8(Span::new("5")).unwrap().1, 5);
        assert_eq!(dec_u8(Span::new("25")).unwrap().1, 25);
        assert_eq!(dec_u8(Span::new("255")).unwrap().1, 255);
    }
}