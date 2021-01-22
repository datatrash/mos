use clap::{App, AppSettings, Arg};

use crate::commands::*;
use crate::core::parser;

mod commands;
mod core;
mod errors;

fn get_app() -> App<'static> {
    App::new("mos")
        .version(git_version::git_version!())
        .global_setting(AppSettings::ColoredHelp)
        .global_setting(AppSettings::ArgRequiredElseHelp)
        .arg(
            Arg::new("verbose")
                .short('v')
                .multiple(true)
                .about("Sets the level of verbosity"),
        )
        .arg(
            Arg::new("no-color")
                .long("no-color")
                .about("Disables colorized output"),
        )
        .subcommand(build_app())
        .subcommand(format_app())
}

fn main() {
    #[cfg(windows)]
    ansi_term::enable_ansi_support().unwrap();

    let args = get_app().get_matches();

    loggerv::Logger::new()
        .verbosity(args.occurrences_of("v"))
        .colors(!args.is_present("no-color"))
        .module_path(false)
        .init()
        .unwrap();

    let result = match args.subcommand() {
        Some(("build", args)) => build_command(args),
        Some(("format", args)) => format_command(args),
        _ => panic!("Unknown subcommand"),
    };

    match result {
        Ok(()) => (),
        Err(e) => {
            log::error!("{}", e.format(true));
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::get_app;

    #[test]
    fn can_invoke_build() {
        let args = get_app().get_matches_from(vec!["mos", "build", "test.asm"]);
        assert_eq!(args.subcommand_name(), Some("build"));

        match args.subcommand() {
            Some(("build", args)) => {
                assert_eq!(
                    args.values_of("input").unwrap().collect::<Vec<_>>(),
                    ["test.asm"]
                );
            }
            _ => panic!(),
        }
    }

    #[test]
    fn can_invoke_format() {
        let args = get_app().get_matches_from(vec!["mos", "format", "test.asm"]);
        assert_eq!(args.subcommand_name(), Some("format"));

        match args.subcommand() {
            Some(("format", args)) => {
                assert_eq!(
                    args.values_of("input").unwrap().collect::<Vec<_>>(),
                    ["test.asm"]
                );
            }
            _ => panic!(),
        }
    }
}
