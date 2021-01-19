use std::io::{Read, Write};

use anyhow::Result;
use clap::{App, AppSettings, Arg};
use fs_err as fs;
use log::error;

use crate::core::codegen::{codegen, CodegenOptions, ProgramCounter};
use crate::core::errors::AsmError;
use crate::core::parser;

mod core;
mod formatter;

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
        .subcommand(App::new("build")
            .about("Build")
            .arg(
                Arg::new("input")
                    .about("Sets the input file to use")
                    .required(true)
                    .multiple(true)
            )
        )
        .subcommand(App::new("format")
            .about("Source code formatter")
            .arg(
                Arg::new("input")
                    .about("Sets the input file(s) to use")
                    .required(true)
                    .multiple(true)
            )
        )
}

fn main() -> Result<()> {
    #[cfg(windows)]
    ansi_term::enable_ansi_support().unwrap();

    let args = get_app().get_matches();

    loggerv::Logger::new()
        .verbosity(args.occurrences_of("v"))
        .colors(!args.is_present("no-color"))
        .module_path(false)
        .init()
        .unwrap();

    let input = args.value_of("INPUT").unwrap();
    let mut file = fs::File::open(input)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let (ast, errors) = parser::parse(source.as_str());
    if !errors.is_empty() {
        for error in errors {
            match error {
                AsmError::Parser { location, message } => {
                    error!(
                        "{}:{}:{}: error: {}",
                        input, location.line, location.column, message
                    );
                }
                AsmError::Unknown => error!("Unknown error"),
            }
        }
        std::process::exit(1)
    }

    let generated_code = codegen(
        ast,
        CodegenOptions {
            pc: ProgramCounter::new(0xc000),
        },
    )?;
    let segment = generated_code.segment("Default").unwrap();

    let mut out = fs::File::create("target/out.prg")?;
    out.write_all(&segment.start_pc.to_le_bytes())?;
    out.write_all(&segment.data)?;

    Ok(())
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
                assert_eq!(args.values_of("input").unwrap().collect::<Vec<_>>(), ["test.asm"]);
            },
            _ => panic!()
        }
    }

    #[test]
    fn can_invoke_format() {
        let args = get_app().get_matches_from(vec!["mos", "format", "test.asm"]);
        assert_eq!(args.subcommand_name(), Some("format"));

        match args.subcommand() {
            Some(("format", args)) => {
                assert_eq!(args.values_of("input").unwrap().collect::<Vec<_>>(), ["test.asm"]);
            },
            _ => panic!()
        }
    }
}
