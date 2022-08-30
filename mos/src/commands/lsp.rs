use crate::debugger::DebugServer;
use crate::diagnostic_emitter::MosResult;
use crate::lsp::{LspContext, LspServer};

/// Starts a Language Server
#[derive(argh::FromArgs, PartialEq, Eq, Debug)]
#[argh(subcommand, name = "lsp")]
pub struct LspArgs {
    /// the port on which the debug adapter server should listen
    #[argh(option, default = "6503", short = 'p')]
    debug_adapter_port: u16,
}

pub fn lsp_command(args: &LspArgs) -> MosResult<()> {
    let mut ctx = LspContext::new();
    ctx.listen_stdio();
    let lsp = LspServer::new(ctx);
    let mut dbg = DebugServer::new(lsp.context());
    dbg.start(args.debug_adapter_port)?;

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
            shutdown_receiver.receiver().recv().unwrap();
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
