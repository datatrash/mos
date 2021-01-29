#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;

use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::delimited;

use crate::core::parser::*;

#[derive(Debug, Clone)]
pub struct ConfigMap<'a> {
    items: HashMap<&'a str, Located<'a, Token<'a>>>,
    errors: Rc<RefCell<Vec<ParseError<'a>>>>,
}

impl<'a> ConfigMap<'a> {
    pub fn new(items: Vec<Located<'a, Token<'a>>>) -> Self {
        let items = items
            .into_iter()
            .map(|pair| {
                let (k, v) = match pair.data {
                    Token::ConfigPair(k, v) => (k.data, v),
                    _ => panic!(),
                };

                let k = match k {
                    Token::IdentifierName(id) => id.0,
                    _ => panic!(),
                };
                (k, *v)
            })
            .collect();

        Self {
            items,
            errors: Rc::new(RefCell::new(vec![])),
        }
    }

    pub fn items<'b>(&'b self) -> &'b HashMap<&'a str, Located<'a, Token<'a>>> {
        &self.items
    }

    pub fn identifier(&self, key: &str) -> MosResult<Option<Located<'a, Identifier<'a>>>> {
        match self.items.get(key) {
            Some(lt) => match &lt.data {
                Token::IdentifierName(id) => {
                    Ok(Some(Located::from(lt.location.clone(), id.clone())))
                }
                _ => Err(ParseError::UnexpectedError {
                    location: lt.location.clone(),
                    message: "expected identifier".to_string(),
                }
                .into()),
            },
            _ => Ok(None),
        }
    }

    pub fn expression(&self, key: &str) -> MosResult<Option<Located<'a, Expression<'a>>>> {
        match self.items.get(key) {
            Some(lt) => match &lt.data {
                Token::Expression(expr) => {
                    Ok(Some(Located::from(lt.location.clone(), expr.clone())))
                }
                _ => Err(ParseError::UnexpectedError {
                    location: lt.location.clone(),
                    message: "expected expression".to_string(),
                }
                .into()),
            },
            _ => Ok(None),
        }
    }

    pub fn nested(&self, key: &str) -> MosResult<Option<&ConfigMap<'a>>> {
        match self.items.get(key) {
            Some(lt) => match &lt.data {
                Token::Config(map) => Ok(Some(map)),
                _ => Err(ParseError::UnexpectedError {
                    location: lt.location.clone(),
                    message: "expected nested configuration".to_string(),
                }
                .into()),
            },
            _ => Ok(None),
        }
    }
}

impl<'a> PartialEq for ConfigMap<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.items
            .iter()
            .all(|(k, v)| other.items.get(k) == Some(v))
    }
}

fn kvp(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    let value = alt((
        config_map,
        identifier_name,
        map(expression, |expr| {
            Located::from(expr.location, Token::Expression(expr.data))
        }),
    ));

    map(
        tuple((ws(terminated(ws(identifier_name), char('='))), ws(value))),
        move |(k, v)| {
            Located::from(
                location.clone(),
                Token::ConfigPair(Box::new(k), Box::new(v)),
            )
        },
    )(input)
}

pub fn config_map(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map(
        delimited(
            terminated(char('{'), emptiness()),
            many0(terminated(ws(kvp), emptiness())),
            preceded(emptiness(), char('}')),
        ),
        move |inner| Located::from(location.clone(), Token::Config(ConfigMap::new(inner))),
    )(input)
}

#[cfg(test)]
mod tests {
    use crate::core::parser::config_map::{config_map, ConfigMap};
    use crate::core::parser::{Expression, Identifier, LocatedSpan, NumberType, State, Token};
    use crate::errors::MosResult;

    #[test]
    fn parse_config_object() -> MosResult<()> {
        let cfg = parse(
            r"{
            num = 123
            id = v
            nested =  {
                nested_id   = nested_v
            }
        }",
        );
        assert_eq!(
            cfg.expression("num")?.unwrap().data,
            Expression::Number(123, NumberType::Dec)
        );
        assert_eq!(cfg.identifier("id")?.unwrap().data, Identifier("v"));
        assert_eq!(
            cfg.nested("nested")?
                .unwrap()
                .identifier("nested_id")?
                .unwrap()
                .data,
            Identifier("nested_v")
        );

        assert_eq!(cfg.expression("nested").is_err(), true);
        assert_eq!(cfg.identifier("num").is_err(), true);
        assert_eq!(cfg.nested("num").is_err(), true);

        Ok(())
    }

    fn parse(source: &str) -> ConfigMap {
        let input = LocatedSpan::new_extra(source, State::new("test.asm"));
        let (_, expr) = config_map(input).expect("parser cannot fail");
        match expr.data {
            Token::Config(cfg) => cfg,
            _ => panic!(),
        }
    }
}
