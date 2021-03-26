use crate::config::Config;
use crate::core::parser::parse_or_err;
use crate::core::parser::source::FileSystemParsingSource;
use crate::errors::MosResult;
use crate::formatting::format;
use clap::App;
use fs_err::OpenOptions;
use std::io::Write;

pub fn format_app() -> App<'static> {
    App::new("format").about("Formats input file(s)")
}

pub fn format_command(cfg: &Config) -> MosResult<()> {
    let input_name = cfg.build.entry.clone();
    let tree = parse_or_err(input_name.as_ref(), FileSystemParsingSource::new().into())?;

    for file in tree.files.keys() {
        let formatted = format(file, tree.clone(), cfg.formatting);
        let formatted = formatted.replace("\n", crate::LINE_ENDING);
        let mut output_file = OpenOptions::new().truncate(true).write(true).open(file)?;
        output_file.write_all(formatted.as_bytes())?;
    }

    Ok(())
}
