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

                let k = k.as_identifier().0;
                (k, *v)
            })
            .collect();

        Self { items }
    }

    pub fn keys<'b>(&'b self) -> Vec<&'b &'a str> {
        self.items.keys().collect_vec()
    }

    pub fn value<'b>(&'b self, key: &'b str) -> &'b Located<Token> {
        self.try_value(key).unwrap()
    }

    pub fn try_value<'b>(&'b self, key: &'b str) -> Option<&'b Located<Token>> {
        self.items.get(key)
    }

    pub fn require<'b>(&self, keys: &[&str], location: Location<'b>) -> Vec<MosError> {
        keys.iter()
            .filter_map(|key| match self.items.contains_key(key) {
                true => None,
                false => Some(
                    ParseError::ExpectedError {
                        location: location.clone(),
                        message: format!("required field: {}", key),
                    }
                    .into(),
                ),
            })
            .collect()
    }

    pub fn require_single_identifier<'b>(
        &self,
        keys: &[&str],
        location: Location<'b>,
    ) -> Vec<MosError> {
        keys.iter()
            .filter_map(|key| match self.try_value_as_identifier_path(key) {
                Some(path) => {
                    if path.len() != 1 {
                        Some(
                            ParseError::ExpectedError {
                                location: location.clone(),
                                message: format!("expected single identifier: {}", key),
                            }
                            .into(),
                        )
                    } else {
                        None
                    }
                }
                None => Some(
                    ParseError::ExpectedError {
                        location: location.clone(),
                        message: format!("required field: {}", key),
                    }
                    .into(),
                ),
            })
            .collect()
    }

    pub fn value_as_identifier_path<'b>(&'b self, key: &'b str) -> &'b IdentifierPath<'b> {
        match self.value(key).data.as_factor() {
            ExpressionFactor::IdentifierValue(path, _) => path,
            _ => panic!(),
        }
    }

    pub fn try_value_as_identifier_path<'b>(
        &'b self,
        key: &'b str,
    ) -> Option<&'b IdentifierPath<'b>> {
        match self.try_value(key) {
            Some(lt) => match lt.data.try_as_factor() {
                Some(ExpressionFactor::IdentifierValue(path, _)) => Some(&path),
                _ => None,
            },
            _ => None,
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
    use crate::core::parser::{
        ExpressionFactor, Identifier, IdentifierPath, LocatedSpan, NumberType, State, Token,
    };
    use crate::errors::MosResult;

    #[test]
    fn parse_config_object() -> MosResult<()> {
        let cfg = parse(
            r"{
            num = 123
            path = a.b
            nested =  {
                nested_id   = nested_v
            }
        }",
        );
        assert_eq!(
            cfg.value("num").data.as_factor(),
            &ExpressionFactor::Number(123, NumberType::Dec)
        );
        assert_eq!(
            cfg.value("path").data.as_factor(),
            &ExpressionFactor::IdentifierValue(
                IdentifierPath::new(&[Identifier("a"), Identifier("b")]),
                None,
            )
        );

        assert_eq!(
            cfg.value("nested")
                .data
                .as_config_map()
                .value("nested_id")
                .data
                .as_factor(),
            &ExpressionFactor::IdentifierValue(
                IdentifierPath::new(&[Identifier("nested_v")]),
                None,
            )
        );

        assert_eq!(cfg.try_value("foo").is_none(), true);

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
