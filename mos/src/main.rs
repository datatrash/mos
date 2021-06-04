//! MOS is a toolkit for building applications that target the MOS 6502 CPU.

use std::path::{Path, PathBuf};

use clap::{App, AppSettings, Arg, ArgMatches};
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

fn get_app() -> App<'static> {
    App::new("mos")
        .about("https://mos.datatra.sh")
        .version(option_env!("RELEASE_VERSION").unwrap_or("unknown"))
        .global_setting(AppSettings::ColoredHelp)
        .arg(
            Arg::new("verbose")
                .short('v')
                .multiple(true)
                .takes_value(false)
                .about("Sets the level of verbosity"),
        )
        .arg(
            Arg::new("no-color")
                .long("no-color")
                .about("Disables colorized output"),
        )
        .arg(
            Arg::new("error-style")
                .short('e')
                .long("error-style")
                .takes_value(true)
                .about("The style of error output")
                .value_name("style")
                .possible_values(&["short", "medium", "rich"])
                .default_value("rich"),
        )
        .subcommand(build_app())
        .subcommand(format_app())
        .subcommand(init_app())
        .subcommand(lsp_app())
        .subcommand(test_app())
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

fn run(args: &ArgMatches) -> MosResult<()> {
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

    match args.subcommand() {
        Some(("build", _)) => build_command(&root, &cfg),
        Some(("format", _)) => format_command(&cfg),
        Some(("init", _)) => init_command(&root, &cfg),
        Some(("lsp", subargs)) => lsp_command(subargs),
        Some(("test", _)) => {
            let exit_code = test_command(args, &root, &cfg)?;
            if exit_code > 0 {
                std::process::exit(exit_code);
            } else {
                Ok(())
            }
        }
        _ => {
            let _ = get_app().print_help()?;
            Ok(())
        }
    }
}

fn main() {
    #[cfg(windows)]
    ansi_term::enable_ansi_support().unwrap();

    let args = get_app().get_matches();
    let no_color = args.is_present("no-color");

    loggerv::Logger::new()
        .verbosity(1 + args.occurrences_of("verbose")) // show 'info' by default
        .colors(!no_color)
        .module_path(false)
        .init()
        .unwrap();

    if let Err(e) = run(&args) {
        DiagnosticEmitter::stdout(&args).emit(e);
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use fs_err as fs;
    use tempfile::tempdir;

    use crate::diagnostic_emitter::MosResult;
    use crate::{get_app, mos_toml_path};

    #[test]
    fn can_invoke_build() {
        let args = get_app().get_matches_from(vec!["mos", "build"]);
        assert_eq!(args.subcommand_name(), Some("build"));
    }

    #[test]
    fn can_invoke_format() {
        let args = get_app().get_matches_from(vec!["mos", "format"]);
        assert_eq!(args.subcommand_name(), Some("format"));
    }

    #[test]
    fn can_invoke_init() {
        let args = get_app().get_matches_from(vec!["mos", "init"]);
        assert_eq!(args.subcommand_name(), Some("init"));
    }

    #[test]
    fn can_invoke_subcommand_with_verbose_logging() {
        let args = get_app().get_matches_from(vec!["mos", "-vvv", "format"]);
        assert_eq!(args.subcommand_name(), Some("format"));
    }

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

        assert_eq!(is_present("./test/test2/test3"), true);
        assert_eq!(is_present("./test/test2"), true);
        assert_eq!(is_present("./test"), true);
        assert_eq!(is_present("."), false);

        drop(file);
        root.close()?;

        Ok(())
    }
}
