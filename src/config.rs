use crate::errors::{MosError, MosResult};
use crate::formatting::FormattingOptions;
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct Config {
    pub formatting: Option<FormattingOptions>,
}

impl Config {
    pub fn from_toml(toml: &str) -> MosResult<Config> {
        toml::from_str(toml).map_err(MosError::from)
    }
}
