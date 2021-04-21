use crate::core::parser::{Number, NumberType};
use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::character::complete::{char, hex_digit1};
use nom::combinator::{map, recognize};
use nom::multi::many1;
use nom::sequence::tuple;

pub fn parse_number(input: &str) -> Option<(&str, i64)> {
    fn value<T: Clone>(value: T) -> impl FnMut(&str) -> nom::IResult<&str, T> {
        move |input| Ok((input, value.clone()))
    }

    let mut parse = alt((
        tuple((
            map(char('$'), |_| NumberType::Hex),
            recognize(many1(hex_digit1)),
        )),
        tuple((
            map(char('%'), |_| NumberType::Bin),
            recognize(many1(is_a("01"))),
        )),
        tuple((
            |input| value(NumberType::Dec)(input),
            recognize(many1(is_a("0123456789"))),
        )),
    ));

    match parse(input) {
        Ok((input, (ty, data))) => {
            let number = Number::from_type(ty, data).value();
            Some((input, number))
        }
        _ => None,
    }
}
