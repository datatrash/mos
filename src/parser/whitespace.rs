use super::Span;
use crate::parser::ParseResult;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while, take_while_m_n};
use nom::character::complete::{alpha1, alphanumeric1, hex_digit1, multispace0, newline, space1};
use nom::character::complete::{char, multispace1};
use nom::combinator::{eof, map, map_opt, map_res, recognize, value};
use nom::error::{context, Error, ErrorKind, ParseError};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{FindSubstring, IResult, InputLength, InputTake};

fn cpp_comment(input: Span) -> ParseResult<Span, Span> {
    map(value((), pair(tag("//"), is_not("\n\r"))), |_| "")(input)
}

fn inside_c_comment(input: Span) -> ParseResult<Span, Span> {
    // Once we're inside a C comment, we don't care about anything except perhaps another /*
    let (input, _) = take_until("/*")(input)?;

    // Found another /*, so let's consume it
    let (input, _) = tag("/*")(input)?;

    // Found another /*, so now we either recurse or we go on until we're at the closing */
    let (input, _) = map(
        value(
            (),
            pair(alt((inside_c_comment, take_until("*/"))), tag("*/")),
        ),
        |_| "",
    )(input)?;

    // Ignore any trailing characters until we're up to the next (one level up) */, so the outer function can deal with that
    take_until("*/")(input)
}

fn c_comment(input: Span) -> ParseResult<Span, Span> {
    map(
        value(
            (),
            tuple((
                tag("/*"),
                alt((inside_c_comment, take_until("*/"))),
                tag("*/"),
            )),
        ),
        |_| "",
    )(input)
}

pub(super) fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(Span<'a>) -> ParseResult<Span<'a>, O>
where
    F: FnMut(Span<'a>) -> ParseResult<Span<'a>, O>,
{
    let left = many0(alt((space1, cpp_comment, c_comment)));
    let right = many0(alt((space1, cpp_comment, c_comment)));
    delimited(left, inner, right)
}

pub(super) fn identifier(input: Span) -> ParseResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

pub(super) fn eof_or_eol<'a>() -> impl FnMut(Span<'a>) -> ParseResult<Span<'a>, Span<'a>> {
    let eol = map(many1(newline), |_| "");
    alt((eol, eof))
}

#[allow(clippy::clone_double_ref)]
#[cfg(test)]
mod tests {
    use super::Span;
    use super::*;
    use nom::character::complete::{alpha1, newline, none_of, not_line_ending};
    use nom::combinator::{map, not};
    use nom::multi::{many0, many1};
    use nom::sequence::terminated;

    #[test]
    fn can_ignore_whitespace() {
        let input = "   foo   \n   bar";
        let parser: ParseResult<Span, Vec<Span>> = many0(alt((
            terminated(ws(tag("foo")), eof_or_eol()),
            ws(tag("bar")),
        )))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_ignore_cpp_comments() {
        let input = "   foo // hello  \n   bar";
        let parser: ParseResult<Span, Vec<Span>> = many0(alt((
            terminated(ws(tag("foo")), eof_or_eol()),
            ws(tag("bar")),
        )))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_ignore_unnested_c_comments() {
        let input = "   foo /* hello */   \n   bar";
        let parser: ParseResult<Span, Vec<Span>> = many0(alt((
            terminated(ws(tag("foo")), eof_or_eol()),
            ws(tag("bar")),
        )))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_ignore_nested_c_comments() {
        let input = "   foo /* /*  /*hello */ */*/   \n   bar";
        let parser: ParseResult<Span, Vec<Span>> = many0(alt((
            terminated(ws(tag("foo")), eof_or_eol()),
            ws(tag("bar")),
        )))(input);
        let fragments: Vec<&str> = parser
            .unwrap()
            .1
            .into_iter()
            .map(|span| span.clone())
            .collect();
        assert_eq!(fragments, vec!["foo", "bar"]);
    }

    #[test]
    fn can_parse_identifier() {
        assert_eq!(
            delimited(multispace1, identifier, multispace1)("      hello_there  ")
                .unwrap()
                .1,
            "hello_there"
        );
    }
}
