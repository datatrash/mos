use crate::commands::paint;
use crate::config::Config;
use crate::errors::MosResult;
use crate::test_runner::{enumerate_test_cases, ExecuteResult, TestRunner};
use ansi_term::Colour;
use clap::App;
use mos_core::errors::span_loc_to_error_string;
use mos_core::parser::source::{FileSystemParsingSource, ParsingSource};
use serde::Deserialize;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct TestOptions {
    pub name: Option<String>,
    pub filter: Option<String>,
}

impl Default for TestOptions {
    fn default() -> Self {
        Self {
            name: None,
            filter: None,
        }
    }
}

pub fn test_app() -> App<'static> {
    App::new("test").about("Runs unit test(s)")
}

pub fn test_command(use_color: bool, root: &Path, cfg: &Config) -> MosResult<i32> {
    let src: Arc<Mutex<dyn ParsingSource>> = FileSystemParsingSource::new().into();
    let input_path = root.join(PathBuf::from(&cfg.build.entry));
    let mut test_cases = enumerate_test_cases(src.clone(), &input_path)?;

    if let Some(test_name) = &cfg.test.name {
        test_cases = test_cases
            .into_iter()
            .filter(|c| &c.to_string() == test_name)
            .collect();
    }
    if let Some(test_filter) = &cfg.test.filter {
        test_cases = test_cases
            .into_iter()
            .filter(|c| c.to_string().contains(test_filter))
            .collect();
    }

    let mut failed = vec![];
    let mut num_passed = 0;
    for test_case in test_cases {
        let mut runner = TestRunner::new(src.clone(), &input_path, &test_case)?;
        let (num_cycles, failure) = match runner.run()? {
            ExecuteResult::Running => panic!(),
            ExecuteResult::TestFailed(num_cycles, failure) => (num_cycles, Some(failure)),
            ExecuteResult::TestSuccess(num_cycles) => (num_cycles, None),
        };
        let cycles = if use_color {
            paint(
                use_color,
                Colour::Yellow,
                format!(" ({} cycles)", num_cycles),
            )
        } else {
            format!(" ({} cycles)", num_cycles).into()
        };

        let msg = match failure {
            Some(failure) => {
                failed.push((test_case.to_string(), failure));
                paint(use_color, Colour::Red, "failed")
            }
            None => {
                num_passed += 1;
                paint(use_color, Colour::Green, "ok")
            }
        };
        log::info!("test '{}' ... {}{}", test_case, msg, cycles);
    }

    let test_result = if !failed.is_empty() {
        paint(use_color, Colour::Red, "FAILED")
    } else {
        paint(use_color, Colour::Green, "ok")
    };

    log::info!("");
    if !failed.is_empty() {
        log::info!("failed tests:");
        log::info!("");
        for (failed_test_name, failure) in &failed {
            let flags = failure.cpu.get_status_register();
            let fmt = |f: char, b: u8| if b != 0 { f } else { '-' };
            let flags = vec![
                fmt('N', flags & 128),
                fmt('V', flags & 64),
                '-',
                fmt('B', flags & 16),
                fmt('D', flags & 8),
                fmt('I', flags & 4),
                fmt('Z', flags & 2),
                fmt('C', flags & 1),
            ];
            let flags: String = flags.into_iter().collect();

            let location = failure
                .location
                .as_ref()
                .map(|location| {
                    span_loc_to_error_string(location, &Some(std::env::current_dir().unwrap()))
                })
                .unwrap_or_default();

            log::info!("---- {} ----", failed_test_name);
            log::info!(
                "{}",
                paint(
                    use_color,
                    Colour::Red,
                    format!("{}assertion failed: '{}'", location, failure.message)
                )
            );
            log::info!(
                "PC = {}, SP = {}, flags = {}, A = {}, X = {}, Y = {}",
                paint(
                    use_color,
                    Colour::Yellow,
                    format!("${:04X}", failure.cpu.get_program_counter())
                ),
                paint(
                    use_color,
                    Colour::Yellow,
                    format!("${:04X}", failure.cpu.get_stack_pointer())
                ),
                paint(use_color, Colour::Yellow, flags),
                paint(
                    use_color,
                    Colour::Yellow,
                    format!("${:02X}", failure.cpu.get_accumulator())
                ),
                paint(
                    use_color,
                    Colour::Yellow,
                    format!("${:02X}", failure.cpu.get_x_register())
                ),
                paint(
                    use_color,
                    Colour::Yellow,
                    format!("${:02X}", failure.cpu.get_y_register())
                )
            );
        }
        log::info!("");
        log::info!("failed test summary:");
        for (failed_test_name, _) in &failed {
            log::info!("    {}", failed_test_name);
        }
        log::info!("");
    }
    log::info!(
        "test result: {}. {} passed; {} failed",
        test_result,
        num_passed,
        failed.len()
    );

    if !failed.is_empty() {
        Ok(1)
    } else {
        Ok(0)
    }
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
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(test_command(true, root().as_path(), &cfg)?, 0);
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
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(test_command(true, root().as_path(), &cfg)?, 1);
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
