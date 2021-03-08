use crate::errors::MosResult;
use crate::lsp::LspServer;
use clap::{App, ArgMatches};

pub fn lsp_app() -> App<'static> {
    App::new("lsp").about("Starts a language server, listening on stdin")
}

pub fn lsp_command(_args: &ArgMatches) -> MosResult<()> {
    let ctx = LspServer::new();
    ctx.start()?;

    Ok(())
}
