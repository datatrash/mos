use crate::commands::generate_code;
use crate::config::Config;
use crate::errors::MosResult;
use clap::App;
use serde::Deserialize;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct TestOptions {
    pub name: Option<String>,
}

impl Default for TestOptions {
    fn default() -> Self {
        Self { name: None }
    }
}

pub fn test_app() -> App<'static> {
    App::new("test").about("Runs unit test(s)")
}

pub fn test_command(use_color: bool, root: &Path, cfg: &Config) -> MosResult<()> {
    let mut cfg = cfg.clone();
    cfg.build.target_directory = PathBuf::from(&cfg.build.target_directory)
        .join("tests")
        .to_str()
        .unwrap()
        .into();

    // Run a build to enumerate all test cases
    // TODO: Shortcut when running a single test
    let ctx = generate_code(root, &cfg)?;

    use ansi_term::Colour::Green;
    let msg_ok = if use_color {
        Green.paint("ok")
    } else {
        "ok".into()
    };

    #[allow(clippy::for_kv_map)]
    for (test_name, _data) in ctx.test_cases() {
        cfg.test.name = Some(test_name.clone());
        let _ctx = generate_code(root, &cfg)?;
        log::info!("test '{}' ... {}", test_name, msg_ok);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::BuildOptions;
    use anyhow::Result;
    use std::path::PathBuf;

    #[test]
    fn can_invoke_single_test() -> Result<()> {
        let entry = test_cli_build().join("some-tests.asm");
        let cfg = Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            test: TestOptions {
                name: Some("test a".into()),
            },
            ..Default::default()
        };
        test_command(true, root().as_path(), &cfg)?;
        Ok(())
    }

    fn root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..")
    }

    fn target() -> PathBuf {
        root().join(PathBuf::from("target"))
    }

    fn test_cli_build() -> PathBuf {
        root()
            .join(PathBuf::from("mos"))
            .join(PathBuf::from("test-data").join(PathBuf::from("test")))
    }
}
