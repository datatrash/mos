pub mod adapters;
pub mod connection;
pub mod protocol;
pub mod types;

use crate::core::codegen::{
    relativize_symbols, simplify_symbols, CodegenContext, ProgramCounter, Symbol,
};
use crate::core::parser::IdentifierPath;
use crate::debugger::adapters::vice::ViceAdapter;
use crate::debugger::adapters::{
    Machine, MachineAdapter, MachineBreakpoint, MachineEvent, MachineRunningState,
};
use crate::debugger::connection::DebugConnection;
use crate::debugger::protocol::{Event, EventMessage, ProtocolMessage, Request, ResponseMessage};
use crate::debugger::types::*;
use crate::errors::{MosError, MosErrorOptions, MosResult};
use crate::lsp::LspContext;
use crossbeam_channel::{Receiver, Select};
use itertools::Itertools;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::HashMap;
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
                let mut dbg = DebugSession::new(lsp.clone(), port);
                match dbg.start() {
                    Ok(_) => (),
                    Err(e) => {
                        log::debug!("Could not start DebugSession: {:?}", e);
                    }
                }
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
pub struct DebugSession {
    lsp: Arc<Mutex<LspContext>>,
    lsp_shutdown_receiver: Receiver<()>,
    machine: Option<Machine>,
    port: u16,
    conn: Option<Arc<DebugConnection>>,
    pending_events: Vec<ProtocolMessage>,
    no_debug: bool,
}

trait Handler<R: Request> {
    fn handle(&self, conn: &mut DebugSession, args: R::Arguments) -> MosResult<R::Response>;
}

struct InitializeRequestHandler {}

impl Handler<InitializeRequest> for InitializeRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugSession,
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
    fn handle(&self, conn: &mut DebugSession, args: LaunchRequestArguments) -> MosResult<()> {
        conn.no_debug = args.no_debug.unwrap_or_default();
        let cfg = conn.lock_lsp().config().unwrap();
        let asm_path = PathBuf::from(args.workspace.clone())
            .join(PathBuf::from(cfg.build.target_directory))
            .join(PathBuf::from(cfg.build.entry));
        let prg_path = asm_path.with_extension("prg");
        match ViceAdapter::launch(&args, prg_path) {
            Ok(adapter) => {
                conn.machine = Some(Machine::new(adapter));
            }
            Err(e) => {
                log::error!(
                    "Could not launch machine. Terminating debugging session. Details: {:?}",
                    e
                );
                return Err(e);
            }
        }
        Ok(())
    }
}

struct ConfigurationDoneRequestHandler {}

impl Handler<ConfigurationDoneRequest> for ConfigurationDoneRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: ()) -> MosResult<()> {
        conn.machine_adapter()?.start()?;
        Ok(())
    }
}

struct DisconnectRequestHandler {}

impl Handler<DisconnectRequest> for DisconnectRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: DisconnectRequestArguments) -> MosResult<()> {
        conn.machine_adapter()?.stop()?;
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
        conn: &mut DebugSession,
        _args: ContinueArguments,
    ) -> MosResult<ContinueResponse> {
        conn.machine_adapter()?.resume()?;
        Ok(ContinueResponse {
            all_threads_continued: Some(true),
        })
    }
}

struct ThreadsRequestHandler {}

impl Handler<ThreadsRequest> for ThreadsRequestHandler {
    fn handle(&self, _conn: &mut DebugSession, _args: ()) -> MosResult<ThreadsResponse> {
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
        conn: &mut DebugSession,
        _args: StackTraceArguments,
    ) -> MosResult<StackTraceResponse> {
        let mut stack_frames = vec![];
        if let MachineRunningState::Stopped(pc) = conn.machine_adapter()?.running_state()? {
            if let Some(codegen) = conn.lock_lsp().codegen() {
                if let Some(offset) = codegen.source_map().address_to_offset(pc) {
                    let sl = codegen.tree().code_map.look_up_span(offset.span);

                    let mut frame = StackFrame::new(1, "Stack frame");
                    // TODO: Deal with 0/1 offset
                    frame.line = sl.begin.line + 1;
                    frame.column = sl.begin.column + 1;
                    frame.end_line = Some(sl.end.line + 1);
                    frame.end_column = Some(sl.end.column + 1);
                    frame.source = Some(Source {
                        path: Some(sl.file.name().into()),
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
        _conn: &mut DebugSession,
        _args: ScopesArguments,
    ) -> MosResult<ScopesResponse> {
        let mut registers = Scope::new("Registers", 1);
        registers.presentation_hint = Some(ScopePresentationHint::Registers);
        let mut locals = Scope::new("Locals", 2);
        locals.presentation_hint = Some(ScopePresentationHint::Locals);
        let response = ScopesResponse {
            scopes: vec![registers, locals],
        };
        Ok(response)
    }
}

struct VariablesRequestHandler {}

impl Handler<VariablesRequest> for VariablesRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugSession,
        args: VariablesArguments,
    ) -> MosResult<VariablesResponse> {
        let variables: HashMap<String, i64> = match args.variables_reference {
            1 => {
                // Registers
                conn.machine_adapter()?
                    .registers()?
                    .into_iter()
                    .map(|(name, val)| (name, val as i64))
                    .collect()
            }
            2 => {
                // Locals
                let mut result = HashMap::new();
                if let Some(codegen) = conn.lock_lsp().codegen() {
                    let scopes = get_local_scopes(&conn, codegen)?;
                    for symbols in scopes.values() {
                        for (id, symbol) in symbols {
                            // Hide + / - symbols by default
                            if let Some(stem) = id.stem() {
                                if stem == "-" || stem == "+" {
                                    continue;
                                }
                            }
                            if let Some(val) = symbol.data.try_as_i64() {
                                result.insert(id.to_string(), val);
                            }
                        }
                    }
                }
                result
            }
            _ => panic!(),
        };

        let variables = variables
            .iter()
            .map(|(name, value)| {
                let formatted_value = match args.format {
                    Some(ValueFormat { hex: true }) => {
                        if *value < 256 {
                            format!("${:02X}", value)
                        } else {
                            format!("${:04X}", value)
                        }
                    }
                    _ => format!("{}", value),
                };
                Variable::new(name, &formatted_value)
            })
            .sorted_by_key(|var| var.name.clone())
            .collect();

        let response = VariablesResponse { variables };
        Ok(response)
    }
}

struct SetBreakpointsRequestHandler {}

impl Handler<SetBreakpointsRequest> for SetBreakpointsRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugSession,
        args: SetBreakpointsArguments,
    ) -> MosResult<SetBreakpointsResponse> {
        if conn.no_debug {
            return Ok(SetBreakpointsResponse {
                breakpoints: vec![],
            });
        }

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
                let pcs = match conn.lock_lsp().codegen() {
                    Some(codegen) => codegen
                        .source_map()
                        .line_col_to_offsets(&codegen.tree().code_map, &source_path, line, column)
                        .into_iter()
                        .map(|o| ProgramCounter::new(o.pc.start)..ProgramCounter::new(o.pc.end))
                        .collect_vec(),
                    None => vec![],
                };

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

        let validated = conn.machine_adapter()?.set_breakpoints(&source_path, bps)?;

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
    fn handle(&self, conn: &mut DebugSession, _args: NextArguments) -> MosResult<()> {
        conn.machine_adapter()?.next()?;
        Ok(())
    }
}

struct StepInRequestHandler {}

impl Handler<StepInRequest> for StepInRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: StepInArguments) -> MosResult<()> {
        conn.machine_adapter()?.step_in()?;
        Ok(())
    }
}

struct StepOutRequestHandler {}

impl Handler<StepOutRequest> for StepOutRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: StepOutArguments) -> MosResult<()> {
        conn.machine_adapter()?.step_out()?;
        Ok(())
    }
}

struct PauseRequestHandler {}

impl Handler<PauseRequest> for PauseRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: PauseArguments) -> MosResult<()> {
        conn.machine_adapter()?.pause()?;
        Ok(())
    }
}

struct EvaluateRequestHandler {}

impl Handler<EvaluateRequest> for EvaluateRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugSession,
        args: EvaluateArguments,
    ) -> MosResult<EvaluateResponse> {
        if let Some(codegen) = conn.lock_lsp().codegen() {
            let scopes = get_local_scopes(&conn, codegen)?;
            let expr_path = IdentifierPath::from(args.expression.as_str());
            for symbols in scopes.values() {
                if let Some(symbol) = symbols.get(&expr_path) {
                    if let Some(val) = symbol.data.try_as_i64() {
                        return Ok(EvaluateResponse {
                            result: val.to_string(),
                            ty: None,
                            presentation_hint: None,
                            variables_reference: 0,
                            named_variables: None,
                            indexed_variables: None,
                            memory_reference: None,
                        });
                    }
                }
            }
        }

        Err(MosError::DebugAdapter("not available".into()))
    }
}

/// Get all the symbols that are visible from the local scope
fn get_local_scopes<'a>(
    conn: &'a DebugSession,
    codegen: &'a CodegenContext,
) -> MosResult<HashMap<IdentifierPath, HashMap<IdentifierPath, &'a Symbol>>> {
    if let MachineRunningState::Stopped(pc) = conn.machine_adapter()?.running_state()? {
        if let Some(offset) = codegen.source_map().address_to_offset(pc) {
            let scopes = codegen.visible_symbols(&offset.scope);
            let scopes = relativize_symbols(scopes, &offset.scope);
            let scopes = simplify_symbols(scopes, &offset.scope);
            return Ok(scopes);
        }
    }

    Ok(HashMap::new())
}

impl DebugSession {
    pub fn new(lsp: Arc<Mutex<LspContext>>, port: u16) -> Self {
        let lsp_shutdown_receiver = lsp.lock().unwrap().add_shutdown_handler();

        Self {
            lsp,
            lsp_shutdown_receiver,
            machine: None,
            port,
            conn: None,
            pending_events: vec![],
            no_debug: false,
        }
    }

    fn lock_lsp(&self) -> MutexGuard<LspContext> {
        self.lsp.lock().unwrap()
    }

    fn machine_adapter(&self) -> MosResult<MutexGuard<Box<dyn MachineAdapter + Send>>> {
        match self.machine.as_ref() {
            Some(m) => Ok(m.adapter()),
            None => Err(MosError::Unknown),
        }
    }

    fn connection(&self) -> Option<Arc<DebugConnection>> {
        self.conn.as_ref().cloned()
    }

    pub fn start(&mut self) -> MosResult<()> {
        log::info!("DebugSession listening on port {}...", self.port);
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

        log::debug!("DebugSession shutting down.");
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
                        self.handle(&ConfigurationDoneRequestHandler {}, req.arguments)
                    }
                    ContinueRequest::COMMAND => {
                        self.handle(&ContinueRequestHandler {}, req.arguments)
                    }
                    DisconnectRequest::COMMAND => {
                        self.handle(&DisconnectRequestHandler {}, req.arguments)
                    }
                    EvaluateRequest::COMMAND => {
                        self.handle(&EvaluateRequestHandler {}, req.arguments)
                    }
                    InitializeRequest::COMMAND => {
                        self.handle(&InitializeRequestHandler {}, req.arguments)
                    }
                    LaunchRequest::COMMAND => self.handle(&LaunchRequestHandler {}, req.arguments),
                    NextRequest::COMMAND => self.handle(&NextRequestHandler {}, req.arguments),
                    PauseRequest::COMMAND => self.handle(&PauseRequestHandler {}, req.arguments),
                    StackTraceRequest::COMMAND => {
                        self.handle(&StackTraceRequestHandler {}, req.arguments)
                    }
                    ScopesRequest::COMMAND => self.handle(&ScopesRequestHandler {}, req.arguments),
                    SetBreakpointsRequest::COMMAND => {
                        self.handle(&SetBreakpointsRequestHandler {}, req.arguments)
                    }
                    /*SetVariableRequest::COMMAND => {
                        self.handle(&SetVariableRequestHandler {}, req.arguments)
                    }*/
                    StepInRequest::COMMAND => self.handle(&StepInRequestHandler {}, req.arguments),
                    StepOutRequest::COMMAND => {
                        self.handle(&StepOutRequestHandler {}, req.arguments)
                    }
                    ThreadsRequest::COMMAND => {
                        self.handle(&ThreadsRequestHandler {}, req.arguments)
                    }
                    VariablesRequest::COMMAND => {
                        self.handle(&VariablesRequestHandler {}, req.arguments)
                    }
                    _ => {
                        log::info!("Unknown command: {:?}", req.command);
                        Err(MosError::Unknown)
                    }
                };
                match response {
                    Ok(body) => self.send_response(seq, &command, body)?,
                    Err(e) => self.send_error(
                        seq,
                        &command,
                        &e.format(&MosErrorOptions {
                            use_prefix: false,
                            ..Default::default()
                        }),
                    )?,
                }
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

    fn send_error(&self, request_seq: usize, command: &str, message: &str) -> MosResult<()> {
        let msg = ProtocolMessage::Response(ResponseMessage {
            seq: 0,
            request_seq,
            success: false,
            command: command.into(),
            message: Some(message.into()),
            body: serde_json::Value::Null,
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
