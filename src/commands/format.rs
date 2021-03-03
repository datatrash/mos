use crate::config::Config;
use crate::core::parser::parse_or_err;
use crate::errors::MosResult;
use crate::formatting::format;
use clap::{App, Arg, ArgMatches};
use fs_err::{read_to_string, OpenOptions};
use itertools::Itertools;
use std::io::Write;

pub fn format_app() -> App<'static> {
    App::new("format").about("Formats input file(s)").arg(
        Arg::new("input")
            .about("Sets the input file(s) to use")
            .required(true)
            .multiple(true),
    )
}

pub fn format_command(args: &ArgMatches, cfg: Option<Config>) -> MosResult<()> {
    let formatting_options = cfg.map(|cfg| cfg.formatting).unwrap_or_default();

    let input_names = args.values_of("input").unwrap().collect_vec();

    for input_name in input_names {
        let source = read_to_string(input_name)?;
        let tree = parse_or_err(input_name.as_ref(), &source)?;
        let formatted = format(tree, formatting_options.unwrap_or_default());
        let formatted = formatted.replace("\n", crate::LINE_ENDING);
        let mut output_file = OpenOptions::new()
            .truncate(true)
            .write(true)
            .open(input_name)?;
        output_file.write_all(formatted.as_bytes())?;
    }

    Ok(())
}
