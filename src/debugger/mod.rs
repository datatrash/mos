pub mod connection;
pub mod protocol;
pub mod types;
pub mod utils;
pub mod vice_adapter;

use crate::core::codegen::ProgramCounter;
use crate::debugger::connection::DebugConnection;
use crate::debugger::protocol::{Event, EventMessage, ProtocolMessage, Request, ResponseMessage};
use crate::debugger::types::*;
use crate::debugger::utils::parse_number;
use crate::debugger::vice_adapter::{
    CheckpointSet, MemoryDescriptor, ViceAdapter, ViceRequest, ViceResponse,
};
use crate::errors::{MosError, MosResult};
use crate::lsp::LspContext;
use crossbeam_channel::{Receiver, Select};
use itertools::Itertools;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::HashMap;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread::JoinHandle;

pub struct DebugContext {
    lsp: Arc<Mutex<LspContext>>,
    lsp_shutdown_receiver: Receiver<()>,
    vice: ViceAdapter,
    registers: HashMap<u8, u16>,
    registers_available: HashMap<u8, String>,
    banks_available: HashMap<u16, String>,
    stopped_at: Option<ProgramCounter>,
    breakpoints: HashMap<Range<ProgramCounter>, Breakpoint>,
}

impl DebugContext {
    fn new(lsp: Arc<Mutex<LspContext>>) -> Self {
        let lsp_shutdown_receiver = lsp.lock().unwrap().add_shutdown_handler();

        Self {
            lsp,
            lsp_shutdown_receiver,
            vice: ViceAdapter::new(),
            registers: HashMap::new(),
            registers_available: HashMap::new(),
            banks_available: HashMap::new(),
            stopped_at: None,
            breakpoints: HashMap::new(),
        }
    }

    fn get_bank_id(&self, name: &str) -> Option<u16> {
        self.banks_available
            .iter()
            .find(|(_, bank_name)| *bank_name == name)
            .map(|(bank, _)| *bank)
    }
}

impl DebugContext {
    fn lock_lsp(&self) -> MutexGuard<LspContext> {
        self.lsp.lock().unwrap()
    }
}

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
    context: DebugContext,
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
        let cfg = conn.context.lock_lsp().config().unwrap();
        let asm_path = PathBuf::from(args.workspace)
            .join(PathBuf::from(cfg.build.target_directory))
            .join(PathBuf::from(cfg.build.entry));
        let prg_path = asm_path.with_extension("prg");
        conn.context.vice.start(
            &args.vice_path,
            vec!["-binarymonitor", prg_path.to_str().unwrap()],
        )?;
        conn.context.vice.send(ViceRequest::BanksAvailable)?;
        conn.context.vice.send(ViceRequest::RegistersAvailable)?;
        Ok(())
    }
}

struct ConfigurationDoneRequestHandler {}

impl Handler<ConfigurationDoneRequest> for ConfigurationDoneRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: ()) -> MosResult<()> {
        conn.context.vice.send(ViceRequest::Exit)?;
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
        conn.context.vice.stop()?;
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
        conn.context.vice.send(ViceRequest::Exit)?;
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
        if let Some(pc) = conn.context.stopped_at {
            if let Some(source_map) = conn.context.lock_lsp().codegen().map(|c| c.source_map()) {
                if let Some((filename, offset)) = source_map.address_to_offset(pc) {
                    let mut frame = StackFrame::new(1, "frame");
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
        let variables = conn
            .context
            .registers
            .iter()
            .filter_map(|(id, value)| {
                conn.context.registers_available.get(id).map(|name| {
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
        conn: &mut DebugConnectionHandler,
        args: SetBreakpointsArguments,
    ) -> MosResult<SetBreakpointsResponse> {
        let was_running = conn.context.stopped_at.is_none();

        let source = args.source.clone();
        let source_path = args.source.path.as_ref().unwrap().clone();

        // We're going to communicate with VICE so make sure there a no other messages in the queue
        conn.drain_vice_messages(false)?;

        // Clear existing breakpoints
        let breakpoint_indices = conn
            .context
            .breakpoints
            .iter()
            .map(|(_, b)| b.id.unwrap())
            .collect_vec();
        for index in breakpoint_indices {
            conn.context
                .vice
                .send(ViceRequest::CheckpointDelete(index as u32))?;
            conn.drain_vice_messages_until(true, |msg| {
                matches!(msg, ViceResponse::CheckpointDelete)
            })?;
        }

        let mut breakpoints = HashMap::new();

        for bp in args.breakpoints.unwrap_or_default().into_iter() {
            let line = bp.line - 1; // TODO: deal with this properly
            let column = bp.column.map(|c| c - 1);

            let pcs = {
                let lsp = conn.context.lock_lsp();
                let source_map = lsp.codegen().map(|c| c.source_map());
                source_map
                    .map(|sm| sm.line_col_to_pcs(&source_path, line, column))
                    .unwrap_or_default()
            };

            for pc in pcs {
                let cp = CheckpointSet {
                    start: pc.start.as_u16(),
                    end: pc.end.as_u16(),
                    stop_when_hit: true,
                    enabled: true,
                    cpu_operation: 4,
                    temporary: false,
                };
                conn.context.vice.send(ViceRequest::CheckpointSet(cp))?;

                if let Some(ViceResponse::CheckpointResponse(r)) = conn
                    .drain_vice_messages_until(true, |msg| {
                        matches!(msg, ViceResponse::CheckpointResponse(_))
                    })?
                {
                    let mut b = Breakpoint::new(true);
                    b.id = Some(r.number as usize);
                    b.verified = true;
                    b.line = Some(line + 1);
                    b.column = column.map(|c| c + 1);
                    b.source = Some(source.clone());
                    breakpoints.insert(pc, b);
                }
            }
        }

        conn.context.breakpoints = breakpoints.clone();
        conn.check_breakpoints_hit();

        if was_running {
            // If we were running before setting the breakpoints, we should continue now
            conn.context.vice.send(ViceRequest::Exit)?;
        }

        let response = SetBreakpointsResponse {
            breakpoints: breakpoints.into_iter().map(|(_, b)| b).collect(),
        };
        Ok(response)
    }
}

struct NextRequestHandler {}

impl Handler<NextRequest> for NextRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: NextArguments) -> MosResult<()> {
        conn.advance_instructions(false, true, 1)?;
        Ok(())
    }
}

struct StepInRequestHandler {}

impl Handler<StepInRequest> for StepInRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: StepInArguments) -> MosResult<()> {
        conn.advance_instructions(false, false, 1)?;
        Ok(())
    }
}

struct StepOutRequestHandler {}

impl Handler<StepOutRequest> for StepOutRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: StepOutArguments) -> MosResult<()> {
        conn.advance_instructions(true, false, 0)?;
        Ok(())
    }
}

struct PauseRequestHandler {}

impl Handler<PauseRequest> for PauseRequestHandler {
    fn handle(&self, conn: &mut DebugConnectionHandler, _args: PauseArguments) -> MosResult<()> {
        conn.advance_instructions(false, false, 1)?;
        Ok(())
    }
}

struct EvaluateRequestHandler {}

impl Handler<EvaluateRequest> for EvaluateRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        args: EvaluateArguments,
    ) -> MosResult<EvaluateResponse> {
        let result = match MemoryDescriptor::parse(&args.expression) {
            Some(mut desc) => {
                desc.bank_id = conn.context.get_bank_id("cpu").unwrap();
                conn.context
                    .vice
                    .send(ViceRequest::MemoryGet(desc.clone()))?;
                let response = conn
                    .drain_vice_messages_until(true, |msg| {
                        matches!(msg, ViceResponse::MemoryGet(_))
                    })?
                    .unwrap();
                let data = match response {
                    ViceResponse::MemoryGet(data) => data,
                    _ => panic!(),
                };
                desc.format(&data)
            }
            None => "Invalid expression".into(),
        };

        Ok(EvaluateResponse {
            result,
            ty: None,
            presentation_hint: Some(VariablePresentationHint {
                kind: Some(VariableKind::Data),
                attributes: Some(vec![VariableAttribute::Constant]), // just constant registers for now
                visibility: None,
            }),
            variables_reference: 0,
            named_variables: None,
            indexed_variables: None,
            memory_reference: None,
        })
    }
}

struct SetVariableRequestHandler {}

impl Handler<SetVariableRequest> for SetVariableRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugConnectionHandler,
        args: SetVariableArguments,
    ) -> MosResult<SetVariableResponse> {
        if let Some((_, value)) = parse_number(&args.value) {
            let result = match MemoryDescriptor::parse(&args.name) {
                Some(mut desc) => {
                    let data = vec![value as u8];
                    desc.bank_id = conn.context.get_bank_id("cpu").unwrap();
                    conn.context
                        .vice
                        .send(ViceRequest::MemorySet(desc.clone(), data.clone()))?;
                    conn.drain_vice_messages_until(true, |msg| {
                        matches!(msg, ViceResponse::MemorySet)
                    })?;
                    desc.format(&data)
                }
                None => "Could not set variable".into(),
            };

            Ok(SetVariableResponse {
                value: result,
                ty: None,
                variables_reference: 0,
                named_variables: None,
                indexed_variables: None,
            })
        } else {
            Err(MosError::Unknown)
        }
    }
}

impl DebugConnectionHandler {
    pub fn new(lsp: Arc<Mutex<LspContext>>, port: u16) -> Self {
        Self {
            context: DebugContext::new(lsp),
            port,
            conn: None,
            pending_events: vec![],
        }
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

            // ...or from VICE...
            let vice_receiver = self.context.vice.receiver();
            if let Some(v) = &vice_receiver {
                sel.recv(v);
            }

            // ...or from the LSP telling us we should be shutting down...
            let shutdown_receiver = &self.context.lsp_shutdown_receiver;
            sel.recv(shutdown_receiver);

            let oper = sel.select();
            match oper.index() {
                0 => match oper.recv(&receiver) {
                    Ok(m) => self.handle_message(m)?,
                    Err(_) => break,
                },
                1 if vice_receiver.is_some() => {
                    match oper.recv(vice_receiver.as_ref().unwrap()) {
                        Ok(m) => self.handle_vice_message(m)?,
                        Err(_) => {
                            // Vice got disconnected, so let's stop the debugging session
                            self.enqueue_event::<TerminatedEvent>(());
                        }
                    }
                }
                2 => {
                    log::trace!("Shutdown received from LSP.");
                    break;
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
                    EvaluateRequest::COMMAND => {
                        self.handle(&EvaluateRequestHandler {}, req.arguments)?
                    }
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
                    SetVariableRequest::COMMAND => {
                        self.handle(&SetVariableRequestHandler {}, req.arguments)?
                    }
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

    pub fn handle_vice_message(&mut self, msg: ViceResponse) -> MosResult<()> {
        log::debug!("Handling VICE message: {:?}", msg);

        match msg {
            ViceResponse::Resumed(_) => {
                self.context.stopped_at = None;
            }
            ViceResponse::Stopped(pc) => {
                self.context.stopped_at = Some(ProgramCounter::new(pc as usize));
                self.check_breakpoints_hit();
            }
            ViceResponse::Registers(map) => {
                self.context.registers = map;
            }
            ViceResponse::BanksAvailable(map) => {
                self.context.banks_available = map;
            }
            ViceResponse::RegistersAvailable(map) => {
                self.context.registers_available = map;
            }
            _ => (),
        }

        Ok(())
    }

    fn check_breakpoints_hit(&mut self) {
        if let Some(pc) = self.context.stopped_at {
            if self
                .context
                .breakpoints
                .iter()
                .any(|(range, _)| pc >= range.start && pc <= range.end)
            {
                self.enqueue_stopped_event(StoppedReason::Breakpoint, pc);
            }
        }
    }

    fn enqueue_stopped_event(&mut self, reason: StoppedReason, at: ProgramCounter) {
        self.context.stopped_at = Some(at);
        let mut args = StoppedEventArguments::new(reason);
        args.thread_id = Some(1);
        self.enqueue_event::<StoppedEvent>(args);
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

    fn drain_vice_messages(&mut self, block: bool) -> MosResult<()> {
        let _ = self.drain_vice_messages_until(block, |_| false)?;
        Ok(())
    }

    fn drain_vice_messages_until<F: Fn(&ViceResponse) -> bool>(
        &mut self,
        block: bool,
        predicate: F,
    ) -> MosResult<Option<ViceResponse>> {
        let vice_receiver = self.context.vice.receiver();
        if let Some(v) = &vice_receiver {
            loop {
                let recv = if block {
                    v.recv().map_err(|_| true)
                } else {
                    v.try_recv().map_err(|e| e.is_disconnected())
                };
                match recv {
                    Ok(msg) => {
                        self.handle_vice_message(msg.clone())?;
                        if predicate(&msg) {
                            return Ok(Some(msg));
                        }
                    }
                    Err(is_disconnected) => {
                        if is_disconnected {
                            // Vice got disconnected, so let's stop the debugging session
                            self.enqueue_event::<TerminatedEvent>(());
                        }
                        break;
                    }
                }
            }
        }
        Ok(None)
    }

    fn toggle_all_breakpoints(&mut self, enabled: bool) -> MosResult<()> {
        for bp in self.context.breakpoints.values() {
            let id = bp.id.unwrap();
            self.context
                .vice
                .send(ViceRequest::CheckpointToggle(id as u32, enabled))?;
        }
        Ok(())
    }

    fn advance_instructions(
        &mut self,
        step_out: bool,
        step_over: bool,
        num_instructions: u16,
    ) -> MosResult<()> {
        self.drain_vice_messages(false)?;
        self.toggle_all_breakpoints(false)?;
        if step_out {
            self.context.vice.send(ViceRequest::ExecuteUntilReturn)?;
        } else {
            self.context.vice.send(ViceRequest::AdvanceInstructions(
                step_over,
                num_instructions,
            ))?;
        };
        let response = self
            .drain_vice_messages_until(true, |msg| matches!(msg, ViceResponse::Stopped(_)))?
            .unwrap();
        self.toggle_all_breakpoints(true)?;

        let pc = match response {
            ViceResponse::Stopped(pc) => ProgramCounter::new(pc as usize),
            _ => panic!(),
        };
        if !step_out && !step_over {
            self.enqueue_stopped_event(StoppedReason::Pause, pc);
        } else {
            self.enqueue_stopped_event(StoppedReason::Step, pc);
        }
        Ok(())
    }
}
