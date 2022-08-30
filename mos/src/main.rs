//! MOS is a toolkit for building applications that target the MOS 6502 CPU.

use std::path::{Path, PathBuf};

use fs_err as fs;

use crate::commands::*;
use crate::config::Config;
use crate::diagnostic_emitter::*;

/// Contains the available CLI commands and their associated logic
mod commands;
/// Configuration file handling
mod config;
/// Debug Adapter Protocol implementation
mod debugger;
/// Error handling
mod diagnostic_emitter;
/// Language Server Protocol implementation
mod lsp;
/// MemoryAccessor trait
mod memory_accessor;
/// Unit Test runner
mod test_runner;
/// Miscellaneous utility methods
mod utils;

#[derive(argh::FromArgs, PartialEq, Eq, Debug)]
/// mos - https://mos.datatra.sh
pub struct Args {
    #[argh(subcommand)]
    subcommand: Subcommand,
    /// disables colorized output
    #[argh(switch)]
    no_color: bool,
    /// logging verbosity
    #[argh(switch, short = 'v')]
    verbosity: u8,
    /// error style
    #[argh(option, short = 'e', default = "ErrorStyle::Rich")]
    error_style: ErrorStyle,
}

#[derive(PartialEq, Eq, Debug, strum::EnumString)]
pub enum ErrorStyle {
    Short,
    Medium,
    Rich,
}

#[derive(argh::FromArgs, PartialEq, Eq, Debug)]
#[argh(subcommand)]
pub enum Subcommand {
    Init(InitArgs),
    Build(BuildArgs),
    Format(FormatArgs),
    Test(TestArgs),
    Lsp(LspArgs),
    Version(VersionArgs),
}

fn mos_toml_path<P: Into<PathBuf>>(
    root: Option<&Path>,
    starting_path: P,
) -> MosResult<Option<PathBuf>> {
    let starting_path = starting_path.into().canonicalize().unwrap();
    let mut path = starting_path.as_path();
    loop {
        let toml = path.join("mos.toml");
        if toml.exists() {
            return Ok(Some(toml));
        }

        // Are we at the root? Then we didn't find anything.
        if Some(path) == root {
            return Ok(None);
        }

        match path.parent() {
            Some(parent) => {
                path = parent;
            }
            None => {
                return Ok(None);
            }
        }
    }
}

fn run(args: &Args) -> MosResult<()> {
    let mos_toml = mos_toml_path(None, &Path::new("."))?;
    let (root, cfg) = match mos_toml {
        Some(path) => {
            log::trace!("Using configuration from: {}", &path.to_str().unwrap());
            let toml = fs::read_to_string(&path)?;
            (
                path.parent().unwrap().to_path_buf(),
                Config::from_toml(&toml)?,
            )
        }
        None => {
            log::trace!("No configuration file found. Using defaults.");
            (PathBuf::from("."), Config::default())
        }
    };

    match &args.subcommand {
        Subcommand::Build(_) => build_command(&root, &cfg),
        Subcommand::Format(_) => format_command(&cfg),
        Subcommand::Init(_) => init_command(&root, &cfg),
        Subcommand::Lsp(subargs) => lsp_command(subargs),
        Subcommand::Test(_) => {
            let exit_code = test_command(args, &root, &cfg)?;
            if exit_code > 0 {
                std::process::exit(exit_code);
            } else {
                Ok(())
            }
        }
        Subcommand::Version(_) => version_command(),
    }
}

fn main() {
    #[cfg(windows)]
    ansi_term::enable_ansi_support().unwrap();

    let args: Args = argh::from_env();

    loggerv::Logger::new()
        .verbosity((1 + args.verbosity) as u64) // show 'info' by default
        .colors(!args.no_color)
        .module_path(false)
        .init()
        .unwrap();

    if let Err(e) = run(&args) {
        match &args.subcommand {
            Subcommand::Lsp(_) => {
                // Don't try to emit to stdout since it's probably gone by this time in the LSP
            }
            _ => DiagnosticEmitter::stdout(&args).emit(e),
        }
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use fs_err as fs;
    use tempfile::tempdir;

    use crate::diagnostic_emitter::MosResult;
    use crate::mos_toml_path;

    #[test]
    fn can_locate_mos_toml() -> MosResult<()> {
        let root = tempdir()?;
        fs::create_dir_all(root.path().join("test/test2/test3"))?;
        let file = fs::File::create(root.path().join("test/mos.toml"))?;

        let is_present = |path: &str| {
            let path = Path::new(path);
            mos_toml_path(Some(root.path()), root.path().join(path))
                .ok()
                .flatten()
                .is_some()
        };

        assert!(is_present("./test/test2/test3"));
        assert!(is_present("./test/test2"));
        assert!(is_present("./test"));
        assert!(!is_present("."));

        drop(file);
        root.close()?;

        Ok(())
    }
}
