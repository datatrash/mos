use std::cell::RefCell;

use crate::parser2::mnemonic::mnemonic;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, tag_no_case, take_till1, take_until};
use nom::character::complete::{
    alpha1, alphanumeric1, anychar, char, digit1, hex_digit1, newline, space1,
};
use nom::combinator::{all_consuming, map, map_opt, not, opt, recognize, rest};

use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

mod ast;
mod mnemonic;

pub use ast::*;

fn expect<'a, F, E, T>(
    mut parser: F,
    error_msg: E,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<Option<T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
    E: ToString,
{
    move |input| {
        let i = input.clone();
        match parser(input) {
            Ok((remaining, out)) => Ok((remaining, Some(out))),
            Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => {
                let err = Error {
                    location: Location::from(&i),
                    message: error_msg.to_string(),
                };
                i.extra.report_error(err);
                Ok((i, None))
            }
            Err(err) => Err(err),
        }
    }
}

fn cpp_comment(input: LocatedSpan) -> IResult<LocatedSpan> {
    recognize(pair(tag("//"), is_not("\n\r")))(input)
}

fn inside_c_comment(input: LocatedSpan) -> IResult<LocatedSpan> {
    // Once we're inside a C comment, we don't care about anything except perhaps another /*
    let (input, _) = take_until("/*")(input)?;

    // Found another /*, so let's consume it
    let (input, _) = tag("/*")(input)?;

    // Found another /*, so now we either recurse or we go on until we're at the closing */
    let (input, _) = expect(
        pair(alt((inside_c_comment, take_until("*/"))), tag("*/")),
        "missing closing */",
    )(input)?;

    // Ignore any trailing characters until we're up to the next (one level up) */, so the outer function can deal with that
    take_until("*/")(input)
}

fn c_comment(input: LocatedSpan) -> IResult<LocatedSpan> {
    recognize(tuple((
        tag("/*"),
        expect(
            tuple((alt((inside_c_comment, take_until("*/"))), tag("*/"))),
            "missing closing */",
        ),
    )))(input)
}

fn whitespace_or_comment<'a>() -> impl FnMut(LocatedSpan<'a>) -> IResult<Vec<Comment>> {
    map(
        many0(alt((
            map(space1, |_| None),
            map(c_comment, |span| {
                Some(Comment::CStyle(span.fragment().to_owned().into()))
            }),
            map(cpp_comment, |span| {
                Some(Comment::CppStyle(span.fragment().to_owned().into()))
            }),
        ))),
        |comments| comments.into_iter().flatten().collect::<Vec<_>>(),
    )
}

fn ws<'a, F>(inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<Token>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<Token>,
{
    map(
        tuple((whitespace_or_comment(), inner, whitespace_or_comment())),
        |(l, i, r)| {
            if l.is_empty() && r.is_empty() {
                i
            } else {
                Token::Ws((l, Box::new(i), r))
            }
        },
    )
}

fn identifier(input: LocatedSpan) -> IResult<Token> {
    let id = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));

    map(id, |span: LocatedSpan| {
        Token::Identifier(Identifier(span.fragment().to_string()))
    })(input)
}

fn addressing_mode<'a, F>(inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<Token>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<Token>,
{
    let inner_with_parens = delimited(
        char('('),
        expect(inner, "expected expression after `(`"),
        expect(char(')'), "missing `)`"),
    );

    let am = tuple((
        inner_with_parens,
        opt(expect(
            alt((
                map(tuple((char(','), tag_no_case("x"))), |_| Some(Register::X)),
                map(tuple((char(','), tag_no_case("y"))), |_| Some(Register::Y)),
                map(tuple((char(','), alpha1)), |(_, i)| {
                    let err = Error {
                        location: Location::from(&i),
                        message: format!("invalid register: {}", i),
                    };
                    i.extra.report_error(err);
                    None
                }),
            )),
            "expected register X or Y",
        )),
    ));

    map(am, |(inner, register)| {
        Token::IndirectAddressing((
            Box::new(inner.unwrap_or(Token::Error)),
            register.flatten().flatten(),
        ))
    })
}

fn operand(input: LocatedSpan) -> IResult<Token> {
    alt((identifier, number))(input)
}

fn instruction(input: LocatedSpan) -> IResult<Token> {
    let location = Location::from(&input);

    let instruction = tuple((
        mnemonic,
        expect(
            opt(alt((ws(operand), ws(addressing_mode(ws(operand)))))),
            "expected single operand after opcode",
        ),
    ));

    map(instruction, move |(mnemonic, operand)| {
        let operand = operand.flatten().map(Box::new);
        Token::Instruction((location, mnemonic, operand))
    })(input)
}

fn error(input: LocatedSpan) -> IResult<Token> {
    map(
        take_till1(|c| c == ')' || c == '\n' || c == '\r'),
        |span: LocatedSpan| {
            let err = Error {
                location: Location::from(&span),
                message: format!("unexpected `{}`", span.fragment()),
            };
            span.extra.report_error(err);
            Token::Error
        },
    )(input)
}

fn label(input: LocatedSpan) -> IResult<Token> {
    map(
        tuple((identifier, expect(char(':'), "labels should end with ':'"))),
        |(id, _)| match id {
            Token::Identifier(id) => Token::Label(id),
            _ => panic!(),
        },
    )(input)
}

fn expr(input: LocatedSpan) -> IResult<Token> {
    alt((instruction, label, error))(input)
}

fn source_file(input: LocatedSpan) -> IResult<Vec<Token>> {
    terminated(
        many0(map(pair(ws(expr), many0(newline)), |(expr, _)| expr)),
        preceded(expect(not(anychar), "expected EOF"), rest),
    )(input)
}

fn number(input: LocatedSpan) -> IResult<Token> {
    map(
        alt((
            preceded(
                char('$'),
                map_opt(expect(hex_digit1, "expected hexadecimal value"), |input| {
                    input.map(|i| usize::from_str_radix(i.fragment(), 16).ok())
                }),
            ),
            map_opt(digit1, |input: LocatedSpan| {
                Some(usize::from_str_radix(input.fragment(), 10).ok())
            }),
        )),
        |val| Token::Number(val.unwrap()),
    )(input)
}

pub fn parse(source: &str) -> (Vec<Token>, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = LocatedSpan::new_extra(source, State { errors: &errors });
    let (_, expr) = all_consuming(source_file)(input).expect("parser cannot fail");
    (expr, errors.into_inner())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn print_ast() {
        println!("{:?}", parse("label: lda (foo),x\nsta $fc"));
    }

    #[test]
    fn pretty_print() {
        for token in parse("label: lda (foo),x/* test */\nsta bar // some comment\nbrk").0 {
            println!("{}", token);
        }
    }
}
