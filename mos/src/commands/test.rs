use crate::config::Config;
use crate::diagnostic_emitter::{DiagnosticEmitter, MosResult};
use crate::test_runner::{enumerate_test_cases, format_cpu_details, ExecuteResult, TestRunner};
use crate::utils::paint;
use crate::Args;
use ansi_term::Colour;
use mos_core::parser::source::{FileSystemParsingSource, ParsingSource};
use serde::Deserialize;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

/// Runs unit test(s)
#[derive(argh::FromArgs, PartialEq, Eq, Debug)]
#[argh(subcommand, name = "test")]
pub struct TestArgs {}

#[derive(Debug, Default, Clone, Deserialize, PartialEq, Eq)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
pub struct TestOptions {
    pub name: Option<String>,
    pub filter: Option<String>,
}

pub fn test_command(args: &Args, root: &Path, cfg: &Config) -> MosResult<i32> {
    let use_color = !args.no_color;
    let src: Arc<Mutex<dyn ParsingSource>> = FileSystemParsingSource::new().into();
    let input_path = root.join(PathBuf::from(&cfg.build.entry));
    let mut test_cases = enumerate_test_cases(src.clone(), &input_path)?;

    if let Some(test_name) = &cfg.test.name {
        test_cases.retain(|(_, c)| &c.to_string() == test_name);
    }
    if let Some(test_filter) = &cfg.test.filter {
        test_cases.retain(|(_, c)| c.to_string().contains(test_filter));
    }

    let mut failed = vec![];
    let mut num_passed = 0;
    for (_, test_case) in test_cases {
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
        let mut emitter = DiagnosticEmitter::stdout(args);
        for (failed_test_name, failure) in &failed {
            log::info!("test: {}", failed_test_name);
            emitter.emit_diagnostics(&failure.diagnostic);
            log::info!("{}", format_cpu_details(&failure.cpu, use_color));
            if !failure.traces.is_empty() {
                log::info!("traces:");
                for trace in &failure.traces {
                    log::info!("- {}", &trace);
                }
            }
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
    use crate::{ErrorStyle, Subcommand};
    use anyhow::Result;
    use std::path::PathBuf;

    #[test]
    fn can_invoke_ok_test() -> Result<()> {
        let entry = test_cli_build().join("some-tests.asm");
        let cfg = Config {
            build: BuildOptions {
                entry: entry.to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            test: TestOptions {
                name: Some("ok".into()),
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(test_command(&test_args(), root().as_path(), &cfg)?, 0);
        Ok(())
    }

    #[test]
    fn can_invoke_failing_test() -> Result<()> {
        let entry = test_cli_build().join("some-tests.asm");
        let cfg = Config {
            build: BuildOptions {
                entry: entry.to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            test: TestOptions {
                name: Some("fail".into()),
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(test_command(&test_args(), root().as_path(), &cfg)?, 1);
        Ok(())
    }

    fn test_args() -> Args {
        Args {
            subcommand: Subcommand::Test(TestArgs {}),
            no_color: false,
            verbosity: 0,
            error_style: ErrorStyle::Short,
        }
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
