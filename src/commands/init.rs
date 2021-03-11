use crate::config::Config;
use crate::errors::{MosError, MosResult};
use clap::App;
use fs_err as fs;
use std::path::Path;

pub fn init_app() -> App<'static> {
    App::new("init").about("Creates a new MOS project configuration file")
}

pub fn init_command(root: &Path, _cfg: &Config) -> MosResult<()> {
    if root.join("mos.toml").exists() {
        return Err(MosError::Cli(
            "`mos init` cannot be run in existing MOS projects".into(),
        ));
    }

    fs::write("mos.toml", get_init_toml())?;

    Ok(())
}

fn get_init_toml() -> &'static str {
    include_str!("init.toml")
}

#[cfg(test)]
mod tests {
    use super::get_init_toml;
    use crate::config::Config;
    use crate::errors::MosResult;

    #[test]
    fn create_init_mos_toml() -> MosResult<()> {
        let cfg = Config::from_toml(get_init_toml())?;
        assert_eq!(cfg.build.entry, "main.asm");

        Ok(())
    }
}
