use crate::commands::BuildOptions;
use crate::errors::{MosError, MosResult};
use crate::formatting::FormattingOptions;
use serde::Deserialize;

#[derive(Default, Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct Config {
    pub build: BuildOptions,
    pub formatting: FormattingOptions,
}

impl Config {
    pub fn from_toml(toml: &str) -> MosResult<Config> {
        toml::from_str(toml).map_err(MosError::from)
    }
}
