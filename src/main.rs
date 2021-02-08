use clap::{App, AppSettings, Arg};

use crate::commands::*;
use crate::core::parser;

mod commands;
mod core;
mod errors;

#[cfg(windows)]
pub const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
pub const LINE_ENDING: &str = "\n";

#[cfg(test)]
pub fn enable_tracing<F: Fn(simple_logger::SimpleLogger) -> simple_logger::SimpleLogger>(
    customizer: F,
) {
    use simple_logger::*;
    let logger = SimpleLogger::new().with_level(log::LevelFilter::Off);
    let logger = customizer(logger);
    logger.init().unwrap();
}

#[cfg(test)]
pub fn enable_default_tracing() {
    use simple_logger::*;
    SimpleLogger::new()
        .with_level(log::LevelFilter::Off)
        .with_module_level("mos", log::LevelFilter::Trace)
        .init()
        .unwrap();
}

fn get_app() -> App<'static> {
    App::new("mos")
        .about("https://mos.datatra.sh")
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
    use itertools::Itertools;

    use crate::get_app;

    #[test]
    fn can_invoke_build() {
        let args = get_app().get_matches_from(vec!["mos", "build", "test.asm"]);
        assert_eq!(args.subcommand_name(), Some("build"));

        match args.subcommand() {
            Some(("build", args)) => {
                assert_eq!(args.values_of("input").unwrap().collect_vec(), ["test.asm"]);
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
                assert_eq!(args.values_of("input").unwrap().collect_vec(), ["test.asm"]);
            }
            _ => panic!(),
        }
    }
}
