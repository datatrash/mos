use crate::commands::generate_code;
use crate::config::Config;
use crate::errors::MosResult;
use clap::App;
use emulator_6502::{Interface6502, MOS6502};
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

    for test_name in ctx.test_cases().keys() {
        cfg.test.name = Some(test_name.clone());
        let ctx = generate_code(root, &cfg)?;
        let case = ctx.test_cases().get(test_name).unwrap();

        let mut ram = BasicRam::new();
        for segment in ctx.segments().values() {
            ram.load_program(segment.range().start, segment.range_data());
        }
        let mut cpu = MOS6502::new();
        cpu.set_program_counter(case.emitted_at.unwrap().as_u16());

        // Execute until RTS
        let mut num_cycles = 0;
        let mut result = false;
        loop {
            if cpu.get_program_counter() == 0 {
                // BRK caused the PC to jump to zero, so let's bail
                num_cycles -= 1; // ignore the BRK
                break;
            }

            if cpu.get_remaining_cycles() == 0
                && ram.ram[cpu.get_program_counter() as usize] == 0x60
            {
                // RTS, test succeeded
                result = true;
                break;
            }

            cpu.cycle(&mut ram);
            num_cycles += 1;
        }

        let cycles = if use_color {
            ansi_term::Colour::Yellow.paint(format!(" ({} cycles)", num_cycles))
        } else {
            format!(" ({} cycles)", num_cycles).into()
        };
        let msg = if result { &msg_ok } else { &msg_failed };
        log::info!("test '{}' ... {}{}", test_name, msg, cycles);
    }

    Ok(())
}

struct BasicRam {
    ram: Vec<u8>,
}

impl BasicRam {
    fn new() -> Self {
        Self {
            ram: vec![0; 65536],
        }
    }

    fn load_program(&mut self, start: usize, data: &[u8]) {
        self.ram[start..start + data.len()].clone_from_slice(data);
    }
}

impl Interface6502 for BasicRam {
    fn read(&mut self, address: u16) -> u8 {
        self.ram[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.ram[address as usize] = data
    }
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
                name: Some("ok".into()),
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
