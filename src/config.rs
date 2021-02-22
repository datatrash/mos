use serde::Deserialize;

use crate::commands::FormattingOptions;
use crate::errors::{MosError, MosResult};

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
