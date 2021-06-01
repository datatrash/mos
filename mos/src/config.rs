use crate::commands::{BuildOptions, TestOptions};
use crate::errors::{MosError, MosResult};
use mos_core::formatting::FormattingOptions;
use serde::Deserialize;

#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
pub struct Config {
    pub build: BuildOptions,
    pub formatting: FormattingOptions,
    pub test: TestOptions,
}

impl Config {
    pub fn from_toml(toml: &str) -> MosResult<Config> {
        toml::from_str(toml).map_err(MosError::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse_build_options() -> MosResult<()> {
        assert_eq!(
            Config::from_toml(
                r#"[build]
entry = "main.asm"
target-directory = "target"
symbols = []
        "#
            )?,
            Config {
                ..Default::default()
            }
        );

        Ok(())
    }

    #[test]
    fn can_parse_formatting_options() -> MosResult<()> {
        assert_eq!(
            Config::from_toml(
                r"[formatting]
mnemonics.casing = 'lowercase'
mnemonics.register-casing = 'lowercase'
braces.position = 'same-line'
whitespace.indent = 4
whitespace.label-margin = 20
whitespace.code-margin = 30
        "
            )?,
            Config {
                ..Default::default()
            }
        );

        Ok(())
    }
}
