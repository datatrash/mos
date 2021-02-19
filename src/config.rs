use serde::Deserialize;

use crate::commands::FormattingOptions;
use crate::errors::{MosError, MosResult};

#[derive(Deserialize)]
pub struct Config {
    pub formatting: FormattingOptions,
}

impl Config {
    pub fn from_toml(toml: &str) -> MosResult<Config> {
        toml::from_str(toml).map_err(MosError::from)
    }
}

#[cfg(test)]
mod tests {
    use crate::config::Config;
    use crate::errors::MosResult;

    #[test]
    fn can_read_config() -> MosResult<()> {
        let toml = r"
        [formatting]
        whitespace.indent = 4
        ";

        let cfg = Config::from_toml(toml)?;
        assert_eq!(cfg.formatting.whitespace.indent, 4);

        Ok(())
    }
}
