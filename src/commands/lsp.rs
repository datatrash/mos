use crate::debugger::DebugServer;
use crate::errors::MosResult;
use crate::lsp::{LspContext, LspServer};
use clap::{App, Arg, ArgMatches};

pub fn lsp_app() -> App<'static> {
    App::new("lsp")
        .about("Starts a language server, listening on stdin")
        .arg(
            Arg::new("port")
                .about("The port on which the debug adapter server should listen")
                .default_missing_value("6503")
                .long("debug-adapter-port")
                .takes_value(true),
        )
}

pub fn lsp_command(args: &ArgMatches) -> MosResult<()> {
    let port = args.value_of("port").unwrap_or("6503").parse::<u16>()?;

    let mut ctx = LspContext::new();
    ctx.listen_stdio();
    let lsp = LspServer::new(ctx);
    let mut dbg = DebugServer::new(lsp.context());
    dbg.start(port)?;

    lsp.start()?;
    log::info!("LSP ended");
    dbg.join()?;
    log::info!("DBG ended");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::LspContext;
    use crossbeam_channel::{Receiver, Sender};
    use lsp_server::{Message, Notification, Request};
    use lsp_types::InitializeParams;
    use serde::Serialize;
    use std::thread;

    #[test]
    fn can_shutdown() -> MosResult<()> {
        let mut ctx = LspContext::new();
        let conn = ctx.listen_memory();

        let sender = conn.sender;
        let receiver = conn.receiver;

        // Some other thread will be listening to the LSP shutdown as well (used by the debug adapter)
        let shutdown_receiver = ctx.add_shutdown_handler();
        let some_other_thread = thread::spawn(move || {
            // expect this shutdown receiver to be called
            shutdown_receiver.recv().unwrap();
            log::info!("Received shutdown callback!");
        });

        thread::spawn(move || {
            let lsp = LspServer::new(ctx);
            lsp.start().unwrap();
        });

        // Hey LSP, get ready to initialize
        let params = serde_json::from_str::<InitializeParams>(r#"{ "capabilities": {} }"#).unwrap();
        send_req(&sender, "initialize", params);
        // Receive capabilities response
        expect_msg(&receiver, |_| true);
        // Great, we're now initialized
        send_empty_not(&sender, "initialized");

        // Let's shutdown now
        send_req(&sender, "shutdown", ());
        // Receive shutdown response
        expect_msg(&receiver, |_| true);

        some_other_thread.join().unwrap();
        Ok(())
    }

    fn send_req<P: Serialize>(sender: &Sender<Message>, ty: &str, params: P) {
        sender
            .send(Message::Request(Request::new(0.into(), ty.into(), params)))
            .unwrap();
    }

    fn send_empty_not(sender: &Sender<Message>, ty: &str) {
        sender
            .send(Message::Notification(Notification::new(ty.into(), ())))
            .unwrap();
    }

    fn expect_msg<F: FnOnce(&Message) -> bool>(receiver: &Receiver<Message>, pred: F) {
        let msg = receiver.recv().unwrap();
        if !pred(&msg) {
            panic!("Did not expect: {:?}", msg);
        }
    }
}
