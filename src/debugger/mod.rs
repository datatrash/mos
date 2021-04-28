pub mod adapters;
pub mod connection;
pub mod protocol;
pub mod types;

use crate::debugger::adapters::vice::ViceAdapter;
use crate::debugger::adapters::{
    Machine, MachineAdapter, MachineBreakpoint, MachineEvent, MachineRunningState,
};
use crate::debugger::connection::DebugConnection;
use crate::debugger::protocol::{Event, EventMessage, ProtocolMessage, Request, ResponseMessage};
use crate::debugger::types::*;
use crate::errors::MosResult;
use crate::lsp::LspContext;
use crossbeam_channel::{Receiver, Select};
use itertools::Itertools;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread::JoinHandle;

pub struct DebugServer {
    lsp: Arc<Mutex<LspContext>>,
    shutdown: Arc<AtomicBool>,
    thread: Option<JoinHandle<()>>,
}

impl DebugServer {
    pub fn new(lsp: Arc<Mutex<LspContext>>) -> Self {
        let shutdown = Arc::new(AtomicBool::new(false));

        Self {
            lsp,
            shutdown,
            thread: None,
        }
    }

    pub fn start(&mut self, port: u16) -> MosResult<()> {
        let thread_shutdown = self.shutdown.clone();
        let lsp = self.lsp.clone();
        self.thread = Some(std::thread::spawn(move || {
            while !thread_shutdown.load(Ordering::Relaxed) {
                let mut dbg = DebugConnectionHandler::new(lsp.clone(), port);
                dbg.start().unwrap();
            }
        }));
        Ok(())
    }

    pub fn join(self) -> MosResult<()> {
        self.shutdown.store(true, Ordering::Relaxed);
        self.thread
            .unwrap()
            .join()
            .expect("Could not join debugger thread");
        Ok(())
    }
}

#[allow(dead_code)]
pub struct DebugConnectionHandler {
    lsp: Arc<Mutex<LspContext>>,
    lsp_shutdown_receiver: Receiver<()>,
    machine: Option<Machine>,
    port: u16,
    conn: Option<Arc<DebugConnection>>,
    pending_events: Vec<ProtocolMessage>,
}

trait Handler<R: Request> {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        args: R::Arguments,
    ) -> MosResult<R::Response>;
}

struct InitializeRequestHandler {}

impl Handler<InitializeRequest> for InitializeRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        _args: InitializeRequestArguments,
    ) -> MosResult<Capabilities> {
        conn.enqueue_event::<InitializedEvent>(());
        Ok(Capabilities {
            supports_configuration_done_request: Some(true),
            supports_value_formatting_options: Some(true),
            supports_set_variable: Some(true),
        })
    }
}

struct LaunchRequestHandler {}

impl Handler<LaunchRequest> for LaunchRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        args: LaunchRequestArguments,
    ) -> MosResult<()> {
        let cfg = conn.lock_lsp().config().unwrap();
        let asm_path = PathBuf::from(args.workspace.clone())
            .join(PathBuf::from(cfg.build.target_directory))
            .join(PathBuf::from(cfg.build.entry));
        let prg_path = asm_path.with_extension("prg");
        let adapter = ViceAdapter::launch(&args, prg_path)?;
        conn.machine = Some(Machine::new(adapter));
        Ok(())
    }
}

struct ConfigurationDoneRequestHandler {}

impl Handler<ConfigurationDoneRequest> for ConfigurationDoneRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: ()) -> MosResult<()> {
        conn.machine_adapter().start()?;
        Ok(())
    }
}

struct DisconnectRequestHandler {}

impl Handler<DisconnectRequest> for DisconnectRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        _args: DisconnectRequestArguments,
    ) -> MosResult<()> {
        conn.machine_adapter().stop()?;
        log::debug!("Machine adapter is STOPPED");
        let machine = std::mem::replace(&mut conn.machine, None);
        machine.unwrap().join();
        log::debug!("Machine is STOPPED");
        Ok(())
    }
}

struct ContinueRequestHandler {}

impl Handler<ContinueRequest> for ContinueRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        _args: ContinueArguments,
    ) -> MosResult<ContinueResponse> {
        conn.machine_adapter().resume()?;
        Ok(ContinueResponse {
            all_threads_continued: Some(true),
        })
    }
}

struct ThreadsRequestHandler {}

impl Handler<ThreadsRequest> for ThreadsRequestHandler {
    fn handle(&self, _conn: &mut DebugConnectionHandler, _args: ()) -> MosResult<ThreadsResponse> {
        let response = ThreadsResponse {
            threads: vec![Thread {
                id: 1,
                name: "CPU".into(),
            }],
        };
        Ok(response)
    }
}

struct StackTraceRequestHandler {}

impl Handler<StackTraceRequest> for StackTraceRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        _args: StackTraceArguments,
    ) -> MosResult<StackTraceResponse> {
        let mut stack_frames = vec![];
        if let MachineRunningState::Stopped(pc) = conn.machine_adapter().running_state()? {
            if let Some(source_map) = conn.lock_lsp().codegen().map(|c| c.source_map()) {
                if let Some((filename, offset)) = source_map.address_to_offset(pc) {
                    let mut frame = StackFrame::new(1, "Stack frame");
                    // TODO: Deal with 0/1 offset
                    frame.line = offset.begin.line + 1;
                    frame.column = offset.begin.column + 1;
                    frame.end_line = Some(offset.end.line + 1);
                    frame.end_column = Some(offset.end.column + 1);
                    frame.source = Some(Source {
                        path: Some(filename.into()),
                        ..Default::default()
                    });
                    stack_frames.push(frame);
                }
            }
        }
        let response = StackTraceResponse {
            stack_frames,
            ..Default::default()
        };
        Ok(response)
    }
}

struct ScopesRequestHandler {}

impl Handler<ScopesRequest> for ScopesRequestHandler {
    fn handle(
        &self,
        _conn: &mut DebugConnectionHandler,
        _args: ScopesArguments,
    ) -> MosResult<ScopesResponse> {
        let mut scope = Scope::new("Default", 1);
        scope.presentation_hint = Some(ScopePresentationHint::Registers);
        let response = ScopesResponse {
            scopes: vec![scope],
        };
        Ok(response)
    }
}

struct VariablesRequestHandler {}

impl Handler<VariablesRequest> for VariablesRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        args: VariablesArguments,
    ) -> MosResult<VariablesResponse> {
        let response = VariablesResponse {
            variables: conn.machine_adapter().variables(args)?,
        };
        Ok(response)
    }
}

struct SetBreakpointsRequestHandler {}

impl Handler<SetBreakpointsRequest> for SetBreakpointsRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        args: SetBreakpointsArguments,
    ) -> MosResult<SetBreakpointsResponse> {
        let source = args.source.clone();
        let source_path = args.source.path.as_ref().unwrap().clone();

        let line_column_pcs = args
            .breakpoints
            .unwrap_or_default()
            .into_iter()
            .map(|bp| {
                let line = bp.line - 1; // TODO: deal with this properly
                let column = bp.column.map(|c| c - 1);

                // A single location may result in multiple breakpoints
                let lsp = conn.lock_lsp();
                let source_map = lsp.codegen().map(|c| c.source_map());
                let pcs = source_map
                    .map(|sm| sm.line_col_to_pcs(&source_path, line, column))
                    .unwrap_or_default();
                (line, column, pcs)
            })
            .collect_vec();

        let bps = line_column_pcs
            .into_iter()
            .map(|(line, column, pcs)| {
                pcs.into_iter().map(move |range| MachineBreakpoint {
                    line,
                    column,
                    range,
                })
            })
            .flatten()
            .collect();

        let validated = conn.machine_adapter().set_breakpoints(&source_path, bps)?;

        let breakpoints = validated
            .into_iter()
            .map(|mvb| {
                let mut b = Breakpoint::new(true);
                b.id = Some(mvb.id);
                b.verified = true;
                b.line = Some(mvb.requested.line + 1);
                b.column = mvb.requested.column.map(|c| c + 1);
                b.source = Some(source.clone());
                b
            })
            .collect();

        let response = SetBreakpointsResponse { breakpoints };
        Ok(response)
    }
}

struct NextRequestHandler {}

impl Handler<NextRequest> for NextRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: NextArguments) -> MosResult<()> {
        conn.machine_adapter().next()?;
        Ok(())
    }
}

struct StepInRequestHandler {}

impl Handler<StepInRequest> for StepInRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: StepInArguments) -> MosResult<()> {
        conn.machine_adapter().step_in()?;
        Ok(())
    }
}

struct StepOutRequestHandler {}

impl Handler<StepOutRequest> for StepOutRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: StepOutArguments) -> MosResult<()> {
        conn.machine_adapter().step_out()?;
        Ok(())
    }
}

struct PauseRequestHandler {}

impl Handler<PauseRequest> for PauseRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: PauseArguments) -> MosResult<()> {
        conn.machine_adapter().pause()?;
        Ok(())
    }
}

impl DebugConnectionHandler {
    pub fn new(lsp: Arc<Mutex<LspContext>>, port: u16) -> Self {
        let lsp_shutdown_receiver = lsp.lock().unwrap().add_shutdown_handler();

        Self {
            lsp,
            lsp_shutdown_receiver,
            machine: None,
            port,
            conn: None,
            pending_events: vec![],
        }
    }

    fn lock_lsp(&self) -> MutexGuard<LspContext> {
        self.lsp.lock().unwrap()
    }

    fn machine_adapter(&self) -> MutexGuard<Box<dyn MachineAdapter + Send>> {
        self.machine.as_ref().unwrap().adapter()
    }

    fn connection(&self) -> Option<Arc<DebugConnection>> {
        self.conn.as_ref().cloned()
    }

    pub fn start(&mut self) -> MosResult<()> {
        log::info!("DebugConnectionHandler listening on port {}...", self.port);
        let (debug_connection, _) = DebugConnection::tcp(&format!("127.0.0.1:{}", self.port))
            .unwrap_or_else(|e| panic!("Couldn't listen on port {}: {}", self.port, e));
        self.conn = Some(Arc::new(debug_connection));

        loop {
            let mut sel = Select::new();

            // We may be receiving messages from the IDE...
            let receiver = &self.connection().unwrap().receiver;
            sel.recv(receiver);

            // ...or from the LSP telling us we should be shutting down...
            sel.recv(&self.lsp_shutdown_receiver);

            // ...or from the machine...
            let machine_receiver = self
                .machine
                .as_ref()
                .map(|m| m.adapter().receiver().unwrap());
            if let Some(recv) = &machine_receiver {
                sel.recv(recv);
            }

            let oper = sel.select();
            match oper.index() {
                0 => match oper.recv(&receiver) {
                    Ok(m) => self.handle_message(m)?,
                    Err(_) => break,
                },
                1 => {
                    log::trace!("Shutdown received from LSP.");
                    break;
                }
                2 => {
                    match oper.recv(machine_receiver.as_ref().unwrap()) {
                        Ok(m) => self.handle_machine_event(m)?,
                        Err(_) => {
                            // Machine has an issue, so let's stop the debugging session
                            self.enqueue_event::<TerminatedEvent>(());
                        }
                    }
                }
                _ => panic!(),
            }

            self.send_pending_events()?;
        }

        log::debug!("DebugConnectionHandler shutting down.");
        Ok(())
    }

    pub fn handle_message(&mut self, msg: ProtocolMessage) -> MosResult<()> {
        log::debug!("Handling debugger message: {:?}", msg);

        let seq = msg.seq();

        match msg {
            ProtocolMessage::Request(req) => {
                let command = req.command.clone();
                let response = match command.as_str() {
                    ConfigurationDoneRequest::COMMAND => {
                        self.handle(&ConfigurationDoneRequestHandler {}, req.arguments)?
                    }
                    ContinueRequest::COMMAND => {
                        self.handle(&ContinueRequestHandler {}, req.arguments)?
                    }
                    DisconnectRequest::COMMAND => {
                        self.handle(&DisconnectRequestHandler {}, req.arguments)?
                    }
                    /*EvaluateRequest::COMMAND => {
                        self.handle(&EvaluateRequestHandler {}, req.arguments)?
                    }*/
                    InitializeRequest::COMMAND => {
                        self.handle(&InitializeRequestHandler {}, req.arguments)?
                    }
                    LaunchRequest::COMMAND => {
                        self.handle(&LaunchRequestHandler {}, req.arguments)?
                    }
                    NextRequest::COMMAND => self.handle(&NextRequestHandler {}, req.arguments)?,
                    PauseRequest::COMMAND => self.handle(&PauseRequestHandler {}, req.arguments)?,
                    StackTraceRequest::COMMAND => {
                        self.handle(&StackTraceRequestHandler {}, req.arguments)?
                    }
                    ScopesRequest::COMMAND => {
                        self.handle(&ScopesRequestHandler {}, req.arguments)?
                    }
                    SetBreakpointsRequest::COMMAND => {
                        self.handle(&SetBreakpointsRequestHandler {}, req.arguments)?
                    }
                    /*SetVariableRequest::COMMAND => {
                        self.handle(&SetVariableRequestHandler {}, req.arguments)?
                    }*/
                    StepInRequest::COMMAND => {
                        self.handle(&StepInRequestHandler {}, req.arguments)?
                    }
                    StepOutRequest::COMMAND => {
                        self.handle(&StepOutRequestHandler {}, req.arguments)?
                    }
                    ThreadsRequest::COMMAND => {
                        self.handle(&ThreadsRequestHandler {}, req.arguments)?
                    }
                    VariablesRequest::COMMAND => {
                        self.handle(&VariablesRequestHandler {}, req.arguments)?
                    }
                    _ => {
                        log::info!("Unknown command: {:?}", req.command);
                        serde_json::Value::Null
                    }
                };
                self.send_response(seq, &command, response)?;
            }
            _ => {
                log::debug!("Unknown message type");
            }
        }
        Ok(())
    }

    pub fn handle_machine_event(&mut self, msg: MachineEvent) -> MosResult<()> {
        log::debug!("Handling machine event: {:?}", msg);

        match msg {
            MachineEvent::RunningStateChanged { old, new } => match (old, new) {
                (MachineRunningState::Running, MachineRunningState::Stopped(_)) => {
                    let mut args = StoppedEventArguments::new(StoppedReason::Breakpoint);
                    args.thread_id = Some(1);
                    self.enqueue_event::<StoppedEvent>(args);
                }
                (MachineRunningState::Stopped(_), MachineRunningState::Stopped(_)) => {
                    let mut args = StoppedEventArguments::new(StoppedReason::Step);
                    args.thread_id = Some(1);
                    self.enqueue_event::<StoppedEvent>(args);
                }
                (MachineRunningState::Stopped(_), MachineRunningState::Running) => {
                    self.enqueue_event::<ContinuedEvent>(ContinuedEventArguments {
                        thread_id: 1,
                        all_threads_continued: Some(true),
                    });
                }
                (MachineRunningState::Running, MachineRunningState::Running) => (),
                (MachineRunningState::Launching, _) | (_, MachineRunningState::Launching) => {
                    panic!("Should never receive any machine events during launch.");
                }
            },
            MachineEvent::Disconnected => {
                self.enqueue_event::<TerminatedEvent>(());
            }
        }

        Ok(())
    }

    // Converts untyped data to the right type, invokes a handler and serializes the result again
    fn handle<R: Request>(
        &mut self,
        handler: &dyn Handler<R>,
        args: serde_json::Value,
    ) -> MosResult<serde_json::Value> {
        let args: R::Arguments = serde_json::from_value(args)?;
        let result: R::Response = handler.handle(self, args)?;
        let result = serde_json::to_value(result)?;
        Ok(result)
    }

    fn send_response<R: DeserializeOwned + Serialize>(
        &self,
        request_seq: usize,
        command: &str,
        body: R,
    ) -> MosResult<()> {
        let body = serde_json::to_value(&body).unwrap();
        let msg = ProtocolMessage::Response(ResponseMessage {
            seq: 0,
            request_seq,
            success: true,
            command: command.into(),
            message: None,
            body,
        });

        if let Some(conn) = self.connection() {
            conn.sender.send(msg)?;
        }

        Ok(())
    }

    pub fn enqueue_event<E: Event>(&mut self, body: E::Body) {
        let body = serde_json::to_value(&body).unwrap();
        let msg = ProtocolMessage::Event(EventMessage {
            seq: 0,
            event: E::EVENT.into(),
            body,
        });
        self.pending_events.push(msg);
    }

    fn send_pending_events(&mut self) -> MosResult<()> {
        if let Some(conn) = self.connection() {
            let events = std::mem::replace(&mut self.pending_events, vec![]);
            for e in events {
                conn.sender.send(e)?;
            }
        }
        self.pending_events.clear();
        Ok(())
    }
}
