use crate::parser::numbers::{hexdec_u16, hexdec_u8};
use crate::parser::whitespace::{identifier, ws};
use crate::parser::Span;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, multispace0};
use nom::combinator::{map, value};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq)]
pub enum Expression<'a> {
    U8(u8),
    U16(u16),
    Label(&'a str),
    Parens(Box<Expression<'a>>),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Sub(Box<Expression<'a>>, Box<Expression<'a>>),
    Mul(Box<Expression<'a>>, Box<Expression<'a>>),
    Div(Box<Expression<'a>>, Box<Expression<'a>>),
}

pub enum ResolvedExpression {
    U8(u8),
    U16(u16),
}

fn eval<'a>(
    expr: &Expression<'a>,
    available_labels: &HashMap<&'a str, u16>,
) -> (i32, Vec<&'a str>) {
    match expr {
        Expression::U8(val) => (*val as i32, vec![]),
        Expression::U16(val) => (*val as i32, vec![]),
        Expression::Label(label) => match available_labels.get(label) {
            Some(result) => (*result as i32, vec![]),
            None => (1, vec![label]),
        },
        Expression::Parens(expr) => eval(expr, available_labels),
        Expression::Add(lhs, rhs)
        | Expression::Sub(lhs, rhs)
        | Expression::Mul(lhs, rhs)
        | Expression::Div(lhs, rhs) => {
            let (lhs_result, lhs_missing_labels) = eval(lhs, available_labels);
            let (rhs_result, rhs_missing_labels) = eval(rhs, available_labels);
            let mut missing_labels = vec![];
            missing_labels.extend(lhs_missing_labels);
            missing_labels.extend(rhs_missing_labels);

            match expr {
                Expression::Add(_, _) => (lhs_result + rhs_result, missing_labels),
                Expression::Sub(_, _) => (lhs_result - rhs_result, missing_labels),
                Expression::Mul(_, _) => (lhs_result * rhs_result, missing_labels),
                Expression::Div(_, _) => (lhs_result / rhs_result, missing_labels),
                _ => panic!(),
            }
        }
    }
}

impl<'a> Expression<'a> {
    pub fn evaluate(
        &self,
        available_labels: &HashMap<&'a str, u16>,
    ) -> (ResolvedExpression, Vec<&'a str>) {
        let (result, missing_labels) = eval(&self, available_labels);
        (ResolvedExpression::U16(result as u16), missing_labels)
    }
}

enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use self::Expression::*;
        match *self {
            U8(val) => write!(format, "{}", val),
            U16(val) => write!(format, "{}", val),
            Label(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Parens(ref expr) => write!(format, "[{}]", expr),
        }
    }
}

impl<'a> Debug for Expression<'a> {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use self::Expression::*;
        match *self {
            U8(val) => write!(format, "{}", val),
            U16(val) => write!(format, "{}", val),
            Label(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "[{:?} + {:?}]", left, right),
            Sub(ref left, ref right) => write!(format, "[{:?} - {:?}]", left, right),
            Mul(ref left, ref right) => write!(format, "[{:?} * {:?}]", left, right),
            Div(ref left, ref right) => write!(format, "[{:?} / {:?}]", left, right),
            Parens(ref expr) => write!(format, "[{:?}]", expr),
        }
    }
}

fn parens(input: Span) -> IResult<Span, Expression> {
    delimited(
        multispace0,
        delimited(
            tag("["),
            map(expression, |e| Expression::Parens(Box::new(e))),
            tag("]"),
        ),
        multispace0,
    )(input)
}

fn factor(input: Span) -> IResult<Span, Expression> {
    alt((
        map(hexdec_u8, |u8| Expression::U8(u8)),
        map(hexdec_u16, |u16| Expression::U16(u16)),
        map(identifier, |val: Span| Expression::Label(val.fragment())),
        parens,
    ))(input)
}

fn fold_expressions<'a>(
    initial: Expression<'a>,
    remainder: Vec<(Operation, Expression<'a>)>,
) -> Expression<'a> {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (oper, expr) = pair;

        match oper {
            Operation::Add => Expression::Add(Box::new(acc), Box::new(expr)),
            Operation::Sub => Expression::Sub(Box::new(acc), Box::new(expr)),
            Operation::Mul => Expression::Mul(Box::new(acc), Box::new(expr)),
            Operation::Div => Expression::Div(Box::new(acc), Box::new(expr)),
        }
    })
}

fn term(input: Span) -> IResult<Span, Expression> {
    let (input, initial) = factor(input)?;
    let (input, remainder) = many0(alt((
        |input| {
            let (input, mul) = preceded(ws(tag("*")), factor)(input)?;
            Ok((input, (Operation::Mul, mul)))
        },
        |input| {
            let (input, div) = preceded(ws(tag("/")), factor)(input)?;
            Ok((input, (Operation::Div, div)))
        },
    )))(input)?;

    Ok((input, fold_expressions(initial, remainder)))
}

pub fn expression(input: Span) -> IResult<Span, Expression> {
    let (input, initial) = term(input)?;
    let (input, remainder) = many0(alt((
        |input| {
            let (input, add) = preceded(ws(tag("+")), term)(input)?;
            Ok((input, (Operation::Add, add)))
        },
        |input| {
            let (input, sub) = preceded(ws(tag("-")), term)(input)?;
            Ok((input, (Operation::Sub, sub)))
        },
    )))(input)?;

    Ok((input, fold_expressions(initial, remainder)))
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::{expression, Expression, ResolvedExpression};
    use crate::parser::Span;
    use std::collections::HashMap;

    #[test]
    fn parse_add() {
        let exp = expression(Span::new("1 + $ff")).unwrap().1;
        assert_eq!(format!("{:?}", exp), "[1 + 255]");
    }

    #[test]
    fn parse_sub() {
        let exp = expression(Span::new("16384 - 2")).unwrap().1;
        assert_eq!(format!("{:?}", exp), "[16384 - 2]");
    }

    #[test]
    fn parse_mul() {
        let exp = expression(Span::new("1 * 2")).unwrap().1;
        assert_eq!(format!("{:?}", exp), "[1 * 2]");
    }

    #[test]
    fn parse_div() {
        let exp = expression(Span::new("1 / 2")).unwrap().1;
        assert_eq!(format!("{:?}", exp), "[1 / 2]");
    }

    #[test]
    fn parse_complex() {
        let exp = expression(Span::new("[5 * foo] / bar + baz")).unwrap().1;
        assert_eq!(format!("{:?}", exp), "[[[[5 * foo]] / bar] + baz]");
    }

    #[test]
    fn basic_eval() {
        let exp = expression(Span::new("[5 * 8] / 2 - 6")).unwrap().1;
        match exp.evaluate(&HashMap::new()).0 {
            ResolvedExpression::U16(val) => assert_eq!(val, 14),
            _ => panic!(),
        }
    }

    #[test]
    fn eval_complex() {
        let exp = expression(Span::new("[5 * foo] / bar - baz")).unwrap().1;
        let (_, missing_labels) = exp.evaluate(&HashMap::new());
        assert_eq!(missing_labels.contains(&"foo"), true);
        assert_eq!(missing_labels.contains(&"bar"), true);
        assert_eq!(missing_labels.contains(&"baz"), true);

        let mut labels = HashMap::new();
        labels.insert("foo", 8);
        labels.insert("bar", 2);
        labels.insert("baz", 6);
        let (result, missing_labels) = exp.evaluate(&labels);
        assert_eq!(missing_labels.is_empty(), true);
        match result {
            ResolvedExpression::U16(val) => assert_eq!(val, 14),
            _ => panic!(),
        }
    }
}
