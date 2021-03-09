use crate::config::Config;
use crate::core::parser::parse_or_err;
use crate::errors::MosResult;
use crate::formatting::format;
use clap::App;
use fs_err::{read_to_string, OpenOptions};
use std::io::Write;

pub fn format_app() -> App<'static> {
    App::new("format").about("Formats input file(s)")
}

pub fn format_command(cfg: &Config) -> MosResult<()> {
    let input_names = vec![cfg.build.entry.clone()];

    for input_name in input_names {
        let source = read_to_string(&input_name)?;
        let tree = parse_or_err(input_name.as_ref(), &source)?;
        let formatted = format(tree, cfg.formatting);
        let formatted = formatted.replace("\n", crate::LINE_ENDING);
        let mut output_file = OpenOptions::new()
            .truncate(true)
            .write(true)
            .open(input_name)?;
        output_file.write_all(formatted.as_bytes())?;
    }

    Ok(())
}
