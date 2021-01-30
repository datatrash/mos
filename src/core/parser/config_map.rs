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

        Self { items }
    }

    pub fn keys<'b>(&'b self) -> Vec<&'b &'a str> {
        self.items.keys().collect_vec()
    }

    pub fn value<'b>(&'b self, key: &'b str) -> &'b Located<Token> {
        self.items.get(key).unwrap()
    }

    pub fn require<'b>(
        &self,
        keys: &[&str],
        location: Location<'b>,
        errors: Rc<RefCell<Vec<ParseError<'b>>>>,
    ) {
        for key in keys {
            if !self.items.contains_key(key) {
                let e = ParseError::ExpectedError {
                    location: location.clone(),
                    message: format!("required field: {}", key),
                };
                errors.borrow_mut().push(e);
            }
        }
    }

    pub fn identifier(&self, key: &str) -> Located<&Identifier> {
        self.try_identifier(key)
            .unwrap_or_else(|| panic!("Expected required field: {}", key))
    }

    pub fn try_identifier(&self, key: &str) -> Option<Located<&Identifier>> {
        let lt = self.items.get(key);
        lt.map(|lt| match &lt.data {
            Token::IdentifierName(id) => Some(Located::from(lt.location.clone(), id)),
            _ => None,
        })
        .flatten()
    }

    pub fn expression(&self, key: &str) -> Located<&Expression> {
        self.try_expression(key)
            .unwrap_or_else(|| panic!("Expected required field: {}", key))
    }

    pub fn try_expression(&self, key: &str) -> Option<Located<&Expression>> {
        let lt = self.items.get(key);
        lt.map(|lt| match &lt.data {
            Token::Expression(expr) => Some(Located::from(lt.location.clone(), expr)),
            _ => None,
        })
        .flatten()
    }

    pub fn nested(&self, key: &str) -> Located<&ConfigMap> {
        self.try_nested(key)
            .unwrap_or_else(|| panic!("Expected required field: {}", key))
    }

    pub fn try_nested(&self, key: &str) -> Option<Located<&ConfigMap>> {
        let lt = self.items.get(key);
        lt.map(|lt| match &lt.data {
            Token::Config(cfg) => Some(Located::from(lt.location.clone(), cfg)),
            _ => None,
        })
        .flatten()
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
            cfg.expression("num").data,
            &Expression::Number(123, NumberType::Dec)
        );
        assert_eq!(cfg.identifier("id").data, &Identifier("v"));
        assert_eq!(
            cfg.nested("nested").data.identifier("nested_id").data,
            &Identifier("nested_v")
        );

        assert_eq!(cfg.try_expression("nested").is_none(), true);
        assert_eq!(cfg.try_identifier("num").is_none(), true);
        assert_eq!(cfg.try_nested("num").is_none(), true);

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
