use crate::config::Config;
use crate::errors::MosResult;
use crate::test_runner::{enumerate_test_cases, CycleResult, TestRunner};
use clap::App;
use mos_core::parser::source::{FileSystemParsingSource, ParsingSource};
use serde::Deserialize;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

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
    let src: Arc<Mutex<dyn ParsingSource>> = FileSystemParsingSource::new().into();
    let input_path = root.join(PathBuf::from(&cfg.build.entry));
    let test_cases = enumerate_test_cases(src.clone(), &input_path)?;

    let msg_ok = if use_color {
        ansi_term::Colour::Green.paint("ok")
    } else {
        "ok".into()
    };
    let msg_failed = if use_color {
        ansi_term::Colour::Red.paint("failed")
    } else {
        "failed".into()
    };

    for test_case in test_cases {
        let mut runner = TestRunner::new(src.clone(), &input_path, &test_case)?;
        let (num_cycles, success) = match runner.run()? {
            CycleResult::Running => panic!(),
            CycleResult::TestFailed(num_cycles, _) => (num_cycles, false),
            CycleResult::TestSuccess(num_cycles) => (num_cycles, true),
        };
        let cycles = if use_color {
            ansi_term::Colour::Yellow.paint(format!(" ({} cycles)", num_cycles))
        } else {
            format!(" ({} cycles)", num_cycles).into()
        };
        let msg = if success { &msg_ok } else { &msg_failed };
        log::info!("test '{}' ... {}{}", test_case, msg, cycles);
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
    fn can_invoke_ok_test() -> Result<()> {
        let entry = test_cli_build().join("some-tests.asm");
        let cfg = Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            test: TestOptions {
                name: Some("ok".into()),
            },
            ..Default::default()
        };
        test_command(true, root().as_path(), &cfg)?;
        Ok(())
    }

    #[test]
    fn can_invoke_failing_test() -> Result<()> {
        let entry = test_cli_build().join("some-tests.asm");
        let cfg = Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            test: TestOptions {
                name: Some("fail".into()),
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
