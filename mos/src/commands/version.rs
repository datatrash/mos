use crate::diagnostic_emitter::MosResult;

/// Prints the version of the application
#[derive(argh::FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "version")]
pub struct VersionArgs {}

pub fn version_command() -> MosResult<()> {
    log::info!("{}", option_env!("RELEASE_VERSION").unwrap_or("unknown"));
    Ok(())
}
