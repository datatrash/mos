use crate::diagnostic_emitter::MosResult;

/// Prints the version of the application
#[derive(argh::FromArgs, PartialEq, Eq, Debug)]
#[argh(subcommand, name = "version")]
pub struct VersionArgs {}

pub fn version_command() -> MosResult<()> {
    log::info!("{}", env!("CARGO_PKG_VERSION"));
    Ok(())
}
