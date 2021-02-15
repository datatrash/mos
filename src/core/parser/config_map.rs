use std::collections::{HashMap, HashSet};

use nom::combinator::map;
use nom::multi::many0;

use crate::core::parser::*;

/// A ConfigMap stores generic key-value pairs that are used for things like segment definitions
///
/// Internally this is just a HashMap storing actual [Token]s, but provides a few convenience methods on top of that.
#[derive(Debug, Clone)]
pub struct ConfigMap<'a> {
    items: HashMap<&'a str, Located<'a, Token<'a>>>,
}

impl<'a> ConfigMap<'a> {
    /// Create a new ConfigMap based on the provided [Token::ConfigPair] and [Token::EolTrivia] tokens.
    pub fn new(items: Vec<Located<'a, Token<'a>>>) -> Self {
        let items = items
            .into_iter()
            .filter_map(|pair| {
                let kvp = match pair.data {
                    Token::ConfigPair { key, value, .. } => Some((key.data, value)),
                    Token::EolTrivia(_) => None,
                    _ => panic!(),
                };

                kvp.map(|(k, v)| {
                    let k = k.as_identifier().0;
                    (k, *v)
                })
            })
            .collect();

        Self { items }
    }

    /// Get a reference to a key that must exist.
    pub fn value<'b>(&'b self, key: &'b str) -> &'b Located<Token> {
        self.try_value(key).unwrap()
    }

    /// Get a reference to a key that may not exist.
    pub fn try_value<'b>(&'b self, key: &'b str) -> Option<&'b Located<Token>> {
        self.items.get(key)
    }

    /// Get a reference to the [IdentifierPath] contained within a key that must exist.
    pub fn value_as_identifier_path<'b>(&'b self, key: &'b str) -> &'b IdentifierPath<'b> {
        match self.value(key).data.as_factor() {
            ExpressionFactor::IdentifierValue { path, .. } => &path.data,
            _ => panic!(),
        }
    }

    /// Get a reference to the [IdentifierPath] contained within a key that may not exist.
    pub fn try_value_as_identifier_path<'b>(
        &'b self,
        key: &'b str,
    ) -> Option<&'b IdentifierPath<'b>> {
        match self.try_value(key) {
            Some(lt) => match lt.data.try_as_factor() {
                Some(ExpressionFactor::IdentifierValue { path, .. }) => Some(&path.data),
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

pub struct ConfigMapValidatorBuilder {
    allowed: HashSet<String>,
    required: HashSet<String>,
    required_single: HashSet<String>,
}

impl Default for ConfigMapValidatorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ConfigMapValidatorBuilder {
    fn new() -> Self {
        Self {
            allowed: HashSet::new(),
            required: HashSet::new(),
            required_single: HashSet::new(),
        }
    }

    pub fn allowed(mut self, key: &str) -> Self {
        self.allowed.insert(key.into());
        self
    }

    pub fn require(mut self, key: &str) -> Self {
        self.allowed.insert(key.into());
        self.required.insert(key.into());
        self
    }

    pub fn require_single_value(mut self, key: &str) -> Self {
        self.allowed.insert(key.into());
        self.required.insert(key.into());
        self.required_single.insert(key.into());
        self
    }

    pub fn validate(self, cfg: &ConfigMap, location: &Location) -> Vec<MosError> {
        // Check if the provided keys are present. If not, errors will be generated based on the provided location.
        let required_fields = self
            .required
            .iter()
            .filter_map(|key| match cfg.items.contains_key(key.as_str()) {
                true => None,
                false => Some(
                    ParseError::ExpectedError {
                        location: location.clone(),
                        message: format!("required field: {}", key),
                    }
                    .into(),
                ),
            })
            .collect_vec();

        // Check if the provided keys are present and only contain a single identifier (and no deeper nested path).
        // If not, errors will be generated based on the provided location.
        let required_single_fields = self
            .required_single
            .iter()
            .filter_map(|key| match cfg.try_value_as_identifier_path(key.as_str()) {
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
                None => None,
            })
            .collect_vec();

        // Check fields that aren't valid in this config map at all
        let incorrect_fields = cfg
            .items
            .iter()
            .filter_map(|(key, _)| match self.allowed.contains(*key) {
                true => None,
                false => Some(
                    ParseError::UnexpectedError {
                        location: location.clone(),
                        message: format!("unexpected field: {}", key),
                    }
                    .into(),
                ),
            })
            .collect_vec();

        let mut result = required_fields;
        result.extend(required_single_fields);
        result.extend(incorrect_fields);
        result
    }
}

/// Tries to parse a single key-value pair within the map
fn kvp(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    let value = alt((
        config_map,
        map(expression, |expr| {
            Located::new(expr.location, Token::Expression(expr.data))
        }),
    ));

    map_once(
        tuple((ws(identifier_name), ws(char('=')), ws(value))),
        move |(key, eq, value)| {
            let key = Box::new(key.flatten());
            let value = Box::new(value.flatten());
            Located::new(location, Token::ConfigPair { key, eq, value })
        },
    )(input)
}

/// Tries to parse a config map
pub fn config_map(input: LocatedSpan) -> IResult<Located<Token>> {
    let location = Location::from(&input);

    map_once(
        tuple((
            multiline_ws(char('{')),
            located(many0(alt((kvp, end_of_line)))),
            ws(char('}')),
        )),
        move |(lparen, inner, rparen)| {
            Located::new(
                location,
                Token::Config {
                    lparen,
                    inner,
                    rparen,
                },
            )
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::core::parser::config_map::config_map;
    use crate::core::parser::{LocatedSpan, State};
    use crate::errors::MosResult;

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
        let input = LocatedSpan::new_extra(source, State::new(&Path::new("test.asm")));
        let (_, expr) = config_map(input).expect("parser cannot fail");
        assert_eq!(format!("{}", expr), expected.to_string());
    }
}
