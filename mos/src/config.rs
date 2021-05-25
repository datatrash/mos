use crate::commands::{BuildOptions, TestOptions};
use crate::errors::{MosError, MosResult};
use mos_core::formatting::FormattingOptions;
use serde::Deserialize;

#[derive(Clone, Default, Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
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
