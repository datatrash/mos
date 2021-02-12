use std::path::Path;
use std::rc::Rc;

use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, tag_no_case, take_till1, take_until};
use nom::character::complete::{alpha1, alphanumeric1, char, hex_digit1, space1};
use nom::combinator::{all_consuming, map, opt, recognize, rest};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{pair, tuple};

pub use ast::*;
pub use config_map::*;
pub use mnemonic::*;

use crate::core::parser::mnemonic::mnemonic;
use crate::errors::{MosError, MosResult};

mod ast;
mod config_map;
mod mnemonic;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError<'a> {
    #[error("{message}")]
    ExpectedError {
        location: Location<'a>,
        message: String,
    },
    #[error("{message}")]
    UnexpectedError {
        location: Location<'a>,
        message: String,
    },
}

impl<'a> From<ParseError<'a>> for MosError {
    fn from(err: ParseError<'a>) -> Self {
        match err {
            ParseError::ExpectedError { location, message } => Self::Parser {
                location: location.into(),
                message,
            },
            ParseError::UnexpectedError { location, message } => Self::Parser {
                location: location.into(),
                message,
            },
        }
    }
}

fn value<'a, T: Clone>(value: T) -> impl FnMut(LocatedSpan<'a>) -> IResult<T> {
    move |input| Ok((input, value.clone()))
}

pub fn map_once<I, O1, O2, E, F, G>(
    mut first: F,
    second: G,
) -> impl FnOnce(I) -> nom::IResult<I, O2, E>
where
    F: nom::Parser<I, O1, E>,
    G: FnOnce(O1) -> O2,
{
    move |input: I| {
        let (input, o1) = first.parse(input)?;
        Ok((input, second(o1)))
    }
}

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
                let err = ParseError::ExpectedError {
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

fn trivia_impl<'a>() -> impl FnMut(LocatedSpan<'a>) -> IResult<Comment> {
    move |input: LocatedSpan<'a>| {
        let (input, comment) = alt((
            map(space1, |span: LocatedSpan| {
                Comment::Whitespace(span.fragment().to_owned().into())
            }),
            map(c_comment, |span| {
                Comment::CStyle(span.fragment().to_owned().into())
            }),
            map(cpp_comment, |span| {
                Comment::CppStyle(span.fragment().to_owned().into())
            }),
        ))(input)?;

        Ok((input, comment))
    }
}

fn multiline_trivia<'a>(input: LocatedSpan<'a>) -> IResult<Trivia<'a>> {
    let location = Location::from(&input);

    map_once(
        many1(alt((
            trivia_impl(),
            map(tuple((opt(char('\r')), char('\n'))), |_| Comment::NewLine),
        ))),
        move |comments| Trivia { location, comments },
    )(input)
}

fn trivia<'a>(input: LocatedSpan<'a>) -> IResult<Trivia<'a>> {
    let location = Location::from(&input);

    map_once(many1(trivia_impl()), move |comments| Trivia {
        location,
        comments,
    })(input)
}

fn ws_impl<'a, T, F>(
    mut inner: F,
    multiline: bool,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<Located<'a, T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    move |input: LocatedSpan<'a>| {
        let location = Location::from(&input);

        let (input, trivia) = if multiline {
            opt(multiline_trivia)(input)
        } else {
            opt(trivia)(input)
        }?;
        let (input, data) = inner(input)?;
        let result = Located::from_trivia(location, data, trivia);

        Ok((input, result))
    }
}

fn multiline_ws<'a, T, F>(inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<Located<'a, T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    ws_impl(inner, true)
}

fn ws<'a, T, F>(inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<Located<'a, T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    ws_impl(inner, false)
}

fn located<'a, T, F>(mut inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<Located<T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    move |input: LocatedSpan<'a>| {
        let location = Location::from(&input);
        let (input, inner) = inner(input)?;
        Ok((input, Located::from(location, inner)))
    }
}

fn identifier_name(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        move |id: LocatedSpan| {
            let id = Identifier(id.fragment());
            Located::from(location, Token::IdentifierName(id))
        },
    )(input)
}

fn identifier_value(input: LocatedSpan) -> IResult<Located<ExpressionFactor>> {
    let location = Location::from(&input);

    let id_location = location.clone();
    let id = tuple((
        opt(ws(map(alt((char('<'), char('>'))), move |m| {
            let modifier = match m {
                '<' => AddressModifier::LowByte,
                '>' => AddressModifier::HighByte,
                _ => panic!(),
            };
            Located::from(id_location.clone(), modifier)
        }))),
        ws(separated_list1(char('.'), identifier_name)),
    ));

    map_once(id, move |(modifier, identifier_path)| {
        let identifier_path = identifier_path.map(|ids| {
            let location = ids.first().unwrap().location.clone();
            let path = ids
                .iter()
                .map(|lt| lt.data.as_identifier().clone())
                .collect_vec();
            Located::from(location, IdentifierPath::new(&path))
        });
        let modifier = modifier.map(|m| m.flatten());

        Located::from(
            location,
            ExpressionFactor::IdentifierValue {
                path: identifier_path.flatten(),
                modifier,
            },
        )
    })(input)
}

fn register_suffix<'a>(
    input: LocatedSpan<'a>,
    reg: &'a str,
    map_to: Register,
) -> IResult<'a, Located<'a, Token<'a>>> {
    let location = Location::from(&input);

    map_once(
        tuple((ws(char(',')), ws(tag_no_case(reg)))),
        move |(comma, register)| {
            let register = register.map(|_r| map_to);
            Located::from(location, Token::RegisterSuffix { comma, register })
        },
    )(input)
}

fn register_x_suffix(input: LocatedSpan) -> IResult<Located<Token>> {
    register_suffix(input, "x", Register::X)
}

fn register_y_suffix(input: LocatedSpan) -> IResult<Located<Token>> {
    register_suffix(input, "y", Register::Y)
}

fn operand(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    let loc_imm = location.clone();
    let am_imm = map(tuple((ws(char('#')), expression)), move |(imm, expr)| {
        let lchar = Some(imm.map_into(|_| '#'));
        Located::from(
            loc_imm.clone(),
            Token::Operand(Operand {
                expr: Box::new(expr),
                lchar,
                rchar: None,
                addressing_mode: AddressingMode::Immediate,
                suffix: None,
            }),
        )
    });

    let optional_suffix = || opt(alt((register_x_suffix, register_y_suffix)));

    let loc_abs = location.clone();
    let am_abs = map(
        tuple((expression, optional_suffix())),
        move |(expr, suffix)| {
            Located::from(
                loc_abs.clone(),
                Token::Operand(Operand {
                    expr: Box::new(expr),
                    lchar: None,
                    rchar: None,
                    addressing_mode: AddressingMode::AbsoluteOrZP,
                    suffix: suffix.map(Box::new),
                }),
            )
        },
    );

    let loc_ind = location.clone();
    let am_ind = map(
        tuple((
            ws(char('(')),
            ws(expression),
            ws(char(')')),
            optional_suffix(),
        )),
        move |(lchar, expr, rchar, suffix)| {
            Located::from(
                loc_ind.clone(),
                Token::Operand(Operand {
                    expr: Box::new(expr.flatten()),
                    lchar: Some(lchar),
                    rchar: Some(rchar),
                    addressing_mode: AddressingMode::OuterIndirect,
                    suffix: suffix.map(Box::new),
                }),
            )
        },
    );

    let loc_outer_ind = location;
    let am_outer_ind = map(
        tuple((
            ws(char('(')),
            ws(expression),
            optional_suffix(),
            ws(char(')')),
        )),
        move |(lchar, expr, suffix, rchar)| {
            Located::from(
                loc_outer_ind.clone(),
                Token::Operand(Operand {
                    expr: Box::new(expr.flatten()),
                    lchar: Some(lchar),
                    rchar: Some(rchar),
                    addressing_mode: AddressingMode::Indirect,
                    suffix: suffix.map(Box::new),
                }),
            )
        },
    );

    alt((am_imm, am_abs, am_ind, am_outer_ind))(input)
}

fn instruction(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    let instruction = tuple((ws(mnemonic), opt(operand)));

    map_once(instruction, move |(mnemonic, operand)| {
        let instruction = Instruction {
            mnemonic,
            operand: operand.map(Box::new),
        };
        Located::from(location, Token::Instruction(instruction))
    })(input)
}

fn error(input: LocatedSpan) -> IResult<Located<Token>> {
    map(
        ws(take_till1(|c| {
            c == ')' || c == '}' || c == '\n' || c == '\r'
        })),
        |span| {
            let err = ParseError::UnexpectedError {
                location: Location::from(&span.data),
                message: format!("unexpected '{}'", span.data.fragment()),
            };
            span.data.extra.report_error(err);
            Located::from(Location::from(&span.data), Token::Error)
        },
    )(input)
}

fn label(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            ws(identifier_name),
            expect(ws(char(':')), "labels should end with ':'"),
        )),
        move |(id, colon)| {
            let id = id.flatten().map_into(|i| i.into_identifier());
            Located::from(location, Token::Label { id, colon })
        },
    )(input)
}

fn data(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            alt((
                map(ws(tag_no_case(".byte")), |t| t.map(|_| DataSize::Byte)),
                map(ws(tag_no_case(".word")), |t| t.map(|_| DataSize::Word)),
                map(ws(tag_no_case(".dword")), |t| t.map(|_| DataSize::Dword)),
            )),
            expect(arg_list, "expected expression"),
        )),
        move |(size, values)| {
            Located::from(
                location,
                Token::Data {
                    values: values.unwrap_or_default(),
                    size,
                },
            )
        },
    )(input)
}

fn varconst<'a, 'b>(
    input: LocatedSpan<'a>,
    tag: &'b str,
    ty: VariableType,
) -> IResult<'a, Located<'a, Token<'a>>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            ws(tag_no_case(tag)),
            ws(identifier_name),
            ws(char('=')),
            expression,
        )),
        move |(tag, id, eq, value)| {
            let ty = tag.map(|_| ty);
            let id = id.flatten().map_into(|id| id.into_identifier());
            Located::from(
                location,
                Token::VariableDefinition {
                    ty,
                    id,
                    eq,
                    value: Box::new(value),
                },
            )
        },
    )(input)
}

fn variable_definition(input: LocatedSpan) -> IResult<Located<Token>> {
    varconst(input, ".var", VariableType::Variable)
}

fn const_definition(input: LocatedSpan) -> IResult<Located<Token>> {
    varconst(input, ".const", VariableType::Constant)
}

fn pc_definition(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((ws(char('*')), ws(char('=')), ws(expression))),
        move |(star, eq, value)| {
            Located::from(
                location,
                Token::ProgramCounterDefinition {
                    star,
                    eq,
                    value: value.flatten(),
                },
            )
        },
    )(input)
}

fn config_definition(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            ws(tag_no_case(".define")),
            ws(identifier_name),
            expect(
                config_map::config_map,
                "unable to parse configuration object",
            ),
        )),
        move |(tag, id, cfg)| {
            let id = Box::new(id.flatten());
            Located::from(
                location,
                Token::Definition {
                    tag: tag.map_into(|_| ".define"),
                    id,
                    value: cfg.map(Box::new),
                },
            )
        },
    )(input)
}

fn braces(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((multiline_ws(char('{')), located(opt(tokens)), ws(char('}')))),
        move |(lparen, inner, rparen)| {
            let inner = inner.map_into(|vec| vec.unwrap_or_else(Vec::new));
            Located::from(
                location,
                Token::Braces {
                    lparen,
                    inner,
                    rparen,
                },
            )
        },
    )(input)
}

fn segment(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            ws(tag_no_case(".segment")),
            ws(identifier_name),
            opt(map(braces, Box::new)),
        )),
        move |(tag, id, inner)| {
            let id = id.flatten();
            Located::from(
                location.clone(),
                Token::Segment {
                    tag: tag.map_into(|_| ".segment"),
                    id: Box::new(id),
                    inner,
                },
            )
        },
    )(input)
}

fn if_(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            ws(tag_no_case(".if")),
            expression,
            braces,
            opt(tuple((ws(tag_no_case("else")), braces))),
        )),
        move |(tag_if, value, if_, else_)| {
            let tag_if = tag_if.map_into(|_| ".if");
            let (tag_else, else_) = match else_ {
                Some((tag_else, else_)) => (Some(tag_else.map_into(|_| "else")), Some(else_)),
                None => (None, None),
            };
            Located::from(
                location,
                Token::If {
                    tag_if,
                    value,
                    if_: Box::new(if_),
                    tag_else,
                    else_: else_.map(Box::new),
                },
            )
        },
    )(input)
}

fn align(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((ws(tag_no_case(".align")), ws(expression))),
        move |(tag, value)| {
            let tag = tag.map_into(|_| ".align");
            let value = value.flatten();
            Located::from(location, Token::Align { tag, value })
        },
    )(input)
}

fn statement(input: LocatedSpan) -> IResult<Located<Token>> {
    alt((
        braces,
        instruction,
        variable_definition,
        const_definition,
        pc_definition,
        config_definition,
        label,
        data,
        segment,
        if_,
        align,
        end_of_line,
    ))(input)
}

fn statement_or_error(input: LocatedSpan) -> IResult<Located<Token>> {
    alt((statement, error))(input)
}

fn end_of_line(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(ws(tuple((opt(char('\r')), char('\n')))), move |triv| {
        let triv = triv.map(|_| '\n');
        Located::from(location, Token::EolTrivia(triv))
    })(input)
}

fn tokens(input: LocatedSpan) -> IResult<Vec<Located<Token>>> {
    many0(statement)(input)
}

fn tokens_or_error(input: LocatedSpan) -> IResult<Vec<Located<Token>>> {
    many0(statement_or_error)(input)
}

fn eof(input: LocatedSpan) -> IResult<Located<Token>> {
    map(ws(rest), move |rest| rest.map_into(|_| Token::Eof))(input)
}

fn source_file(input: LocatedSpan) -> IResult<Vec<Located<Token>>> {
    map(tuple((tokens_or_error, eof)), |(tokens, eof)| {
        let mut result = tokens;
        result.push(eof);
        result
    })(input)
}

fn number(input: LocatedSpan) -> IResult<Located<ExpressionFactor>> {
    let location = Location::from(&input);

    map(
        alt((
            tuple((
                map(ws(char('$')), |ty| ty.map_into(|_| NumberType::Hex)),
                ws(map(recognize(many1(hex_digit1)), |n| {
                    let location = Location::from(&n);
                    Located::from(
                        location,
                        i64::from_str_radix(n.fragment(), 16).ok().unwrap(),
                    )
                })),
            )),
            tuple((
                map(ws(char('%')), |ty| ty.map_into(|_| NumberType::Bin)),
                ws(map(recognize(many1(is_a("01"))), |n| {
                    let location = Location::from(&n);
                    Located::from(location, i64::from_str_radix(n.fragment(), 2).ok().unwrap())
                })),
            )),
            tuple((
                value(Located::from(location, NumberType::Dec)),
                ws(map(recognize(many1(is_a("0123456789"))), |n| {
                    let location = Location::from(&n);
                    Located::from(
                        location,
                        i64::from_str_radix(n.fragment(), 10).ok().unwrap(),
                    )
                })),
            )),
        )),
        |(ty, value)| value.map_into(move |value| ExpressionFactor::Number { ty, value }),
    )(input)
}

fn expression_parens(input: LocatedSpan) -> IResult<Located<ExpressionFactor>> {
    let location = Location::from(&input);

    map_once(
        tuple((ws(char('[')), ws(expression), ws(char(']')))),
        move |(lparen, inner, rparen)| {
            let inner = Box::new(inner.flatten());
            Located::from(
                location,
                ExpressionFactor::ExprParens {
                    lparen,
                    inner,
                    rparen,
                },
            )
        },
    )(input)
}

fn current_pc(input: LocatedSpan) -> IResult<Located<ExpressionFactor>> {
    let location = Location::from(&input);

    map_once(ws(char('*')), move |star| {
        Located::from(location, ExpressionFactor::CurrentProgramCounter(star))
    })(input)
}

fn arg_list(input: LocatedSpan) -> IResult<Vec<ArgItem>> {
    map(
        tuple((
            many0(tuple((ws(expression), ws(char(','))))),
            ws(expression),
        )),
        |(list, last)| {
            let list = list
                .into_iter()
                .map(|(expr, comma)| (expr.flatten(), Some(comma)))
                .collect::<Vec<ArgItem>>();
            let mut result: Vec<ArgItem> = vec![];
            result.extend(list);
            result.push((last.flatten(), None));
            result
        },
    )(input)
}

fn fn_call(input: LocatedSpan) -> IResult<Located<ExpressionFactor>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            ws(identifier_name),
            ws(char('(')),
            opt(arg_list),
            ws(char(')')),
        )),
        move |(name, lparen, args, rparen)| {
            let name = Box::new(name.flatten());
            let args = args.unwrap_or_else(Vec::new);
            Located::from(
                location.clone(),
                ExpressionFactor::FunctionCall {
                    name,
                    lparen,
                    args,
                    rparen,
                },
            )
        },
    )(input)
}

fn expression_factor(input: LocatedSpan) -> IResult<Located<Expression>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            opt(ws(char('!'))),
            opt(ws(char('-'))),
            alt((
                number,
                fn_call,
                identifier_value,
                current_pc,
                expression_parens,
            )),
        )),
        move |(tag_not, tag_neg, factor)| {
            let mut flags = ExpressionFactorFlags::empty();
            if tag_not.is_some() {
                flags.set(ExpressionFactorFlags::NOT, true);
            }
            if tag_neg.is_some() {
                flags.set(ExpressionFactorFlags::NEG, true);
            }
            Located::from(
                location,
                Expression::Factor {
                    factor: Box::new(factor),
                    flags,
                    tag_not,
                    tag_neg,
                },
            )
        },
    )(input)
}

fn fold_expressions<'a>(
    initial: Located<'a, Expression<'a>>,
    remainder: Vec<(Located<'a, BinaryOp>, Located<'a, Expression<'a>>)>,
) -> Located<'a, Expression<'a>> {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (op, expr) = pair;

        Located::from(
            acc.location.clone(),
            Expression::BinaryExpression(BinaryExpression {
                op,
                lhs: Box::new(acc),
                rhs: Box::new(expr),
            }),
        )
    })
}

fn expression_term(input: LocatedSpan) -> IResult<Located<Expression>> {
    let (input, initial) = expression_factor(input)?;

    let (input, remainder) = many0(tuple((
        ws(alt((
            map(tag("*"), |_| BinaryOp::Mul),
            map(tag("/"), |_| BinaryOp::Div),
            map(tag("<<"), |_| BinaryOp::Shl),
            map(tag(">>"), |_| BinaryOp::Shr),
            map(tag("^"), |_| BinaryOp::Xor),
        ))),
        expression_factor,
    )))(input)?;

    Ok((input, fold_expressions(initial, remainder)))
}

pub fn expression(input: LocatedSpan) -> IResult<Located<Expression>> {
    let (input, initial) = expression_term(input)?;

    let (input, remainder) = many0(tuple((
        ws(alt((
            map(tag("+"), |_| BinaryOp::Add),
            map(tag("-"), |_| BinaryOp::Sub),
            map(tag("=="), |_| BinaryOp::Eq),
            map(tag("!="), |_| BinaryOp::Ne),
            map(tag(">="), |_| BinaryOp::GtEq),
            map(tag("<="), |_| BinaryOp::LtEq),
            map(tag(">"), |_| BinaryOp::Gt),
            map(tag("<"), |_| BinaryOp::Lt),
            map(tag("&&"), |_| BinaryOp::And),
            map(tag("||"), |_| BinaryOp::Or),
        ))),
        expression_term,
    )))(input)?;

    Ok((input, fold_expressions(initial, remainder)))
}

pub fn parse<'a>(filename: &'a Path, source: &'a str) -> MosResult<Vec<Located<'a, Token<'a>>>> {
    let state = State::new(filename);
    let errors = state.errors.clone();
    let input = LocatedSpan::new_extra(source, state);
    let (_, expr) = all_consuming(source_file)(input).expect("parser cannot fail");

    let errors = Rc::try_unwrap(errors).ok().unwrap().into_inner();
    if errors.is_empty() {
        Ok(expr)
    } else {
        Err(errors.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_instruction() {
        check("lda #123", "LDA #123");
    }

    #[test]
    fn parse_multiple_lines() {
        check("lda #123\nsta $d020", "LDA #123\nSTA $d020");
        check("nop\n  nop", "NOP\n  NOP");
    }

    #[test]
    fn parse_expression() {
        check("lda #1 + 2", "LDA #1 + 2");
        check("lda #1     +    2", "LDA #1     +    2");
        check("lda #[1   +   2]", "LDA #[1   +   2]");
        check("lda #[1   +   2   ]", "LDA #[1   +   2   ]");
        check("lda #[   1   +   2   ]", "LDA #[   1   +   2   ]");
        check("lda #1 ^ 4", "LDA #1 ^ 4");
        check("lda #1 << 4", "LDA #1 << 4");
        check("lda #1 >> 4", "LDA #1 >> 4");
        check("lda #1 || 2", "LDA #1 || 2");
        check("lda #1 && 2", "LDA #1 && 2");
        check("lda #-foo", "LDA #-foo");
        check("lda #!foo", "LDA #!foo");
        check("lda  %11101", "LDA  %11101");
        check(
            "lda  %11101   +   [  $ff  * -12367 ] / foo",
            "LDA  %11101   +   [  $ff  * -12367 ] / foo",
        );
    }

    #[test]
    fn parse_equality() {
        check("lda #1 == 2", "LDA #1 == 2");
        check("lda #1 != 2", "LDA #1 != 2");
        check("lda #1 < 2", "LDA #1 < 2");
        check("lda #1 > 2", "LDA #1 > 2");
        check("lda #1 <= 2", "LDA #1 <= 2");
        check("lda #1 >= 2", "LDA #1 >= 2");
    }

    #[test]
    fn parse_identifier_paths() {
        check("lda a", "LDA a");
        check("lda   super.a", "LDA   super.a");
    }

    #[test]
    fn can_handle_leading_trailing_whitespace() {
        check("   lda #123", "   LDA #123");
        check("   \nlda #123", "   \nLDA #123");
        check("   \n\nlda #123", "   \n\nLDA #123");
        check("\n\n  ", "\n\n  ");
        check("lda #123\n\n   ", "LDA #123\n\n   ");
        check("   \nlda #123\n\n   ", "   \nLDA #123\n\n   ");
        check("   \n   ", "   \n   ");
        check("lda #123   \n   ", "LDA #123   \n   ");
    }

    #[test]
    fn parse_braces() {
        check("{  }", "{  }");
        check("{   lda #123   }", "{   LDA #123   }");
        check(
            r"
            {
                lda #123
                lda #234
            }
        ",
            r"
            {
                LDA #123
                LDA #234
            }
        ",
        );
    }

    #[test]
    fn parse_if() {
        check("  .if   foo { nop }", "  .IF   foo { NOP }");
        check("  .if   foo\n{\nnop\n}", "  .IF   foo\n{\nNOP\n}");
        check(
            "   .if   foo { nop }   else { brk }",
            "   .IF   foo { NOP }   ELSE { BRK }",
        );
        check(".if defined(foo) { nop }", ".IF defined(foo) { NOP }");
        check(".if !defined(foo) { nop }", ".IF !defined(foo) { NOP }");
    }

    #[test]
    fn parse_align() {
        check("   .align   123", "   .ALIGN   123");
    }

    #[test]
    fn parse_addressing_modes() {
        check("lda #123", "LDA #123");
        check("lda 12345", "LDA 12345");
        check("lda 12345, x", "LDA 12345, X");
        check("lda 12345 ,  x", "LDA 12345 ,  X");
        check("lda 12345, y", "LDA 12345, Y");
        check("lda (123), x", "LDA (123), X");
        check("lda (123, x)", "LDA (123, X)");
        check("lda (   123   ,   x   )", "LDA (   123   ,   X   )");
    }

    #[test]
    fn parse_variable_definitions() {
        check("  .var foo   = 123", "  .VAR foo   = 123");
        check("  .const foo   = 123", "  .CONST foo   = 123");
    }

    #[test]
    fn parse_segment_definitions() {
        check(
            r"  .define   segment
            {
            name = hello
            start = 4096
            }",
            r"  .DEFINE   segment
            {
            name = hello
            start = 4096
            }",
        );
    }

    #[test]
    fn use_segment() {
        check(".segment   foo", ".SEGMENT   foo");
        check("  .segment   foo   { nop }", "  .SEGMENT   foo   { NOP }");
    }

    #[test]
    fn parse_current_pc() {
        check("lda *", "LDA *");
        check("lda * - 3", "LDA * - 3");
    }

    #[test]
    fn set_current_pc() {
        check("  *   =   $1000", "  *   =   $1000");
    }

    #[test]
    fn parse_address_modifiers() {
        check("lda #<foo", "LDA #<foo");
        check("lda #>foo", "LDA #>foo");
    }

    #[test]
    fn parse_data() -> MosResult<()> {
        check(
            ".byte 123\n.word foo\n.dword 12345678\n.word 1 + 2,   3, 4 * 4",
            ".BYTE 123\n.WORD foo\n.DWORD 12345678\n.WORD 1 + 2,   3, 4 * 4",
        );
        Ok(())
    }

    #[test]
    fn parse_label() {
        check("   foo:   nop", "   foo:   NOP");
    }

    #[test]
    fn parse_fn_call() {
        let factor = invoke("func()", |span| fn_call(span));
        assert_eq!(factor.to_string(), "func()");

        let factor = invoke("func   (   a)", |span| fn_call(span));
        assert_eq!(factor.to_string(), "func   (   a)");

        let factor = invoke("func   (a   ,   b   )", |span| fn_call(span));
        assert_eq!(factor.to_string(), "func   (a   ,   b   )");
    }

    fn check(src: &str, expected: &str) {
        let expr = match parse(&Path::new("test.asm"), src) {
            Ok(expr) => expr,
            Err(e) => panic!("Errors: {:?}", e),
        };
        let actual = expr.into_iter().map(|e| format!("{}", e)).join("");
        assert_eq!(actual, expected.to_string());
    }

    fn invoke<'a, O: 'a, F: FnOnce(LocatedSpan<'a>) -> IResult<Located<'a, O>>>(
        src: &'a str,
        parser: F,
    ) -> O {
        let state = State::new(&Path::new("test.asm"));
        let input = LocatedSpan::new_extra(src, state);
        let result = parser(input);
        if result.is_err() {
            panic!(format!("{}", result.err().unwrap()));
        }
        result.ok().unwrap().1.data
    }
}
