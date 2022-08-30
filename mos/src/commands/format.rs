use crate::config::Config;
use crate::diagnostic_emitter::MosResult;
use fs_err::OpenOptions;
use mos_core::errors::map_io_error;
use mos_core::formatting::format;
use mos_core::parser::parse_or_err;
use mos_core::parser::source::FileSystemParsingSource;
use mos_core::LINE_ENDING;
use std::io::Write;

/// Formats input file(s)
#[derive(argh::FromArgs, PartialEq, Eq, Debug)]
#[argh(subcommand, name = "format")]
pub struct FormatArgs {}

pub fn format_command(cfg: &Config) -> MosResult<()> {
    let input_name = cfg.build.entry.clone();
    let tree = parse_or_err(input_name.as_ref(), FileSystemParsingSource::new().into())?;

    for file in tree.files.keys() {
        let formatted = format(file, tree.clone(), cfg.formatting);
        let formatted = formatted.replace('\n', LINE_ENDING);
        let mut output_file = OpenOptions::new()
            .truncate(true)
            .write(true)
            .open(file)
            .map_err(map_io_error)?;
        output_file
            .write_all(formatted.as_bytes())
            .map_err(map_io_error)?;
    }

    Ok(())
}
