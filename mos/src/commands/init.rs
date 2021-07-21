use crate::config::Config;
use crate::diagnostic_emitter::MosResult;
use codespan_reporting::diagnostic::Diagnostic;
use fs_err as fs;
use mos_core::errors::{map_io_error, Diagnostics};
use std::path::Path;

/// Creates a new MOS project configuration file
#[derive(argh::FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "init")]
pub struct InitArgs {}

pub fn init_command(root: &Path, _cfg: &Config) -> MosResult<()> {
    if root.join("mos.toml").exists() {
        return Err(Diagnostics::from(
            Diagnostic::error().with_message("`mos init` cannot be run in existing MOS projects"),
        )
        .into());
    }

    fs::write("mos.toml", get_init_toml()).map_err(map_io_error)?;

    Ok(())
}

fn get_init_toml() -> &'static str {
    include_str!("init.toml")
}

#[cfg(test)]
mod tests {
    use super::get_init_toml;
    use crate::config::Config;
    use crate::diagnostic_emitter::MosResult;

    #[test]
    fn create_init_mos_toml() -> MosResult<()> {
        let cfg = Config::from_toml(get_init_toml())?;
        assert_eq!(cfg.build.entry, "main.asm");

        Ok(())
    }
}
