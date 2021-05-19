use super::*;
use nom::combinator::map;
use nom::multi::many0;

/// Tries to parse a single key-value pair within the map
fn kvp(input: LocatedSpan) -> IResult<Token> {
    let value = alt((config_map, |input| {
        map(expression, |expr| Token::Expression(expr.data))(input)
    }));

    map_once(
        tuple((mws(identifier_name), mws(char('=')), mws(value))),
        move |(key, eq, value)| {
            let key = key.map(|k| k.as_str().to_string());
            let value = Box::new(value);
            Token::ConfigPair { key, eq, value }
        },
    )(input)
}

/// Tries to parse a config map
pub fn config_map(input: LocatedSpan) -> IResult<Token> {
    map_once(
        tuple((mws(char('{')), many0(kvp), mws(char('}')))),
        move |(lparen, inner, rparen)| {
            Token::Config(Block {
                lparen,
                inner,
                rparen,
            })
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::config_map::config_map;
    use super::{LocatedSpan, State};
    use crate::parser::source::InMemoryParsingSource;
    use crate::parser::ParserInstance;
    use std::sync::{Arc, Mutex};

    #[test]
    fn parse_config_object() {
        check(
            r"/*   */   
            {
            num =    123
            path =   a.b
            nested =  {
                nested_id   = nested_v
            }
        }",
            r"/*   */   
            {
            num =    123
            path =   a.b
            nested =  {
                nested_id   = nested_v
            }
        }",
        );
    }

    fn check(source: &str, expected: &str) {
        let state = State::new(
            InMemoryParsingSource::new()
                .add("test.asm", &source.clone())
                .into(),
        );
        let state = Arc::new(Mutex::new(state));
        let current_file = state.lock().unwrap().add_file("test.asm").unwrap();
        let instance = ParserInstance::new(state, current_file);
        let input = LocatedSpan::new_extra(&source, instance);
        let (_, expr) = config_map(input).ok().unwrap();
        assert_eq!(format!("{}", expr), expected.to_string());
    }
}
