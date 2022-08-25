pub mod adapters;
pub mod connection;
pub mod protocol;
pub mod types;

use crate::debugger::adapters::test_runner::TestRunnerAdapter;
use crate::debugger::adapters::vice::ViceAdapter;
use crate::debugger::adapters::{
    Machine, MachineAdapter, MachineBreakpoint, MachineEvent, MachineRunningState,
};
use crate::debugger::connection::DebugConnection;
use crate::debugger::protocol::{Event, EventMessage, ProtocolMessage, Request, ResponseMessage};
use crate::debugger::types::*;
use crate::diagnostic_emitter::MosResult;
use crate::lsp::LspContext;
use crate::memory_accessor::{ensure_ram_fn, MemoryAccessor};
use codespan_reporting::diagnostic::Diagnostic;
use crossbeam_channel::Select;
use itertools::Itertools;
use mos_core::codegen::{CodegenContext, ProgramCounter, SymbolIndex};
use mos_core::errors::Diagnostics;
use mos_core::parser::parse_expression;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
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

pub struct DebugSession {
    lsp: Arc<Mutex<LspContext>>,
    machine: Option<Machine>,
    codegen: Option<Arc<Mutex<CodegenContext>>>,
    port: u16,
    conn: Option<Arc<DebugConnection>>,
    pending_events: Vec<ProtocolMessage>,
    no_debug: bool,
    lines_start_at_1: bool,
    columns_start_at_1: bool,
}

struct MachineAdapterMemoryAccessor {
    adapter: Arc<RwLock<Box<dyn MachineAdapter + Send + Sync>>>,
}

impl MemoryAccessor for MachineAdapterMemoryAccessor {
    fn read(&mut self, address: u16, len: usize) -> Vec<u8> {
        self.adapter.write().unwrap().read(address, len)
    }

    fn write(&mut self, _address: u16, _bytes: &[u8]) {
        unimplemented!()
    }
}

trait Handler<R: Request> {
    fn handle(&self, conn: &mut DebugSession, args: R::Arguments) -> MosResult<R::Response>;
}

struct InitializeRequestHandler {}

impl Handler<InitializeRequest> for InitializeRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugSession,
        args: InitializeRequestArguments,
    ) -> MosResult<Capabilities> {
        conn.lines_start_at_1 = args.lines_start_at_1.unwrap_or_default();
        conn.columns_start_at_1 = args.columns_start_at_1.unwrap_or_default();
        conn.enqueue_event::<InitializedEvent>(());
        Ok(Capabilities {
            supports_configuration_done_request: Some(true),
            supports_value_formatting_options: Some(true),
            supports_set_variable: Some(true),
            supports_completions_request: Some(true),
        })
    }
}

struct LaunchRequestHandler {}

impl Handler<LaunchRequest> for LaunchRequestHandler {
    fn handle(&self, conn: &mut DebugSession, args: LaunchRequestArguments) -> MosResult<()> {
        conn.no_debug = args.no_debug.unwrap_or_default();
        let cfg = conn.lock_lsp().config().unwrap();

        let root = PathBuf::from(args.workspace.clone());
        let src_path = root.join(PathBuf::from(&cfg.build.entry));

        let prg_path = conn.lock_lsp().codegen().map(|codegen| {
            let codegen = codegen.lock().unwrap();
            root.join(&cfg.build.target_directory)
                .join(cfg.build.output_filename(&root, &codegen))
        });

        if prg_path.is_none() {
            return Err(Diagnostics::from(
                Diagnostic::error()
                    .with_message("Could not find output binary. Did assembly fail?"),
            )
            .into());
        }
        let prg_path = prg_path.unwrap();

        if let Some(tr) = &args.test_runner {
            let source = conn.lock_lsp().parsing_source();
            match TestRunnerAdapter::launch(
                &args,
                source,
                src_path,
                &tr.test_case_name.as_str().into(),
            ) {
                Ok(adapter) => {
                    conn.create_machine(adapter);
                    Ok(())
                }
                Err(e) => {
                    log::error!(
                        "Could not launch test runner. Terminating debugging session. Details: {:?}",
                        e
                    );
                    Err(e)
                }
            }
        } else if args.vice_path.is_some() {
            match ViceAdapter::launch(&args, prg_path) {
                Ok(adapter) => {
                    conn.create_machine(adapter);
                    Ok(())
                }
                Err(e) => {
                    log::error!(
                        "Could not launch VICE. Terminating debugging session. Details: {:?}",
                        e
                    );
                    Err(e)
                }
            }
        } else {
            Err(Diagnostics::from(Diagnostic::error().with_message(
                "Could not launch debugging session. No emulator or test run specified.",
            ))
            .into())
        }
    }
}

struct ConfigurationDoneRequestHandler {}

impl Handler<ConfigurationDoneRequest> for ConfigurationDoneRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: ()) -> MosResult<()> {
        conn.machine_adapter_mut()?.start()?;
        Ok(())
    }
}

struct DisconnectRequestHandler {}

impl Handler<DisconnectRequest> for DisconnectRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: DisconnectRequestArguments) -> MosResult<()> {
        conn.machine_adapter_mut()?.stop()?;
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
        conn.machine_adapter_mut()?.resume()?;
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
        let state = conn.machine_adapter()?.running_state()?;
        if let MachineRunningState::Stopped(pc) = state {
            if let Some(codegen) = conn.codegen() {
                if let Some(offset) = codegen.source_map().address_to_offset(pc) {
                    let sl = codegen.tree().code_map.look_up_span(offset.span);

                    let mut frame = StackFrame::new(1, "Stack frame");
                    frame.line = conn.line(sl.begin.line);
                    frame.column = conn.column(sl.begin.column);
                    frame.end_line = Some(conn.line(sl.end.line));
                    frame.end_column = Some(conn.column(sl.end.column));
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
        let mut flags = Scope::new("Flags", 2);
        flags.presentation_hint = Some(ScopePresentationHint::Registers);
        let mut locals = Scope::new("Locals", 3);
        locals.presentation_hint = Some(ScopePresentationHint::Locals);
        let response = ScopesResponse {
            scopes: vec![registers, flags, locals],
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
        let variables = match args.variables_reference {
            1 | 3 => {
                let variables: HashMap<String, i64> = match args.variables_reference {
                    1 => {
                        // Registers
                        conn.machine_adapter()?
                            .registers()?
                            .into_iter()
                            .map(|(name, val)| (name, val))
                            .collect()
                    }
                    3 => {
                        // Locals
                        let mut result = HashMap::new();
                        let state = conn.machine_adapter()?.running_state()?;
                        if let Some(codegen) = conn.codegen() {
                            if let Some(scope) = current_scope(&state, &codegen)? {
                                for (id, symbol_nx) in
                                    codegen.symbols().visible_symbols(scope, true)
                                {
                                    // Hide special symbols by default
                                    if id.is_special() {
                                        continue;
                                    }

                                    if let Some(symbol) = codegen.symbols().try_get(symbol_nx) {
                                        if let Some(val) = symbol.data.try_as_i64() {
                                            result.insert(id.to_string(), val);
                                        }
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

                variables
            }
            2 => {
                // Flags
                let flags = conn.machine_adapter()?.flags()?;
                let fmt = |b: u8| if b != 0 { "true" } else { "false" };
                vec![
                    Variable::new("N - Negative", fmt(flags & 128)),
                    Variable::new("V - Overflow", fmt(flags & 64)),
                    Variable::new("B - Break", fmt(flags & 16)),
                    Variable::new("D - Decimal", fmt(flags & 8)),
                    Variable::new("I - Interrupt", fmt(flags & 4)),
                    Variable::new("Z - Zero", fmt(flags & 2)),
                    Variable::new("C - Carry", fmt(flags & 1)),
                ]
            }
            _ => panic!(),
        };

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
                let line = if conn.lines_start_at_1 {
                    bp.line - 1
                } else {
                    bp.line
                };
                let column = bp
                    .column
                    .map(|c| if conn.columns_start_at_1 { c - 1 } else { c });

                // A single location may result in multiple breakpoints
                let pcs = match conn.codegen() {
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
            .flat_map(|(line, column, pcs)| {
                pcs.into_iter().map(move |range| MachineBreakpoint {
                    line,
                    column,
                    range,
                })
            })
            .collect();

        let validated = conn
            .machine_adapter_mut()?
            .set_breakpoints(&source_path, bps)?;

        let breakpoints = validated
            .into_iter()
            .map(|mvb| {
                let mut b = Breakpoint::new(true);
                b.id = Some(mvb.id);
                b.verified = true;
                b.line = Some(conn.line(mvb.requested.line));
                b.column = mvb.requested.column.map(|c| conn.column(c));
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
        conn.machine_adapter_mut()?.next()?;
        Ok(())
    }
}

struct StepInRequestHandler {}

impl Handler<StepInRequest> for StepInRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: StepInArguments) -> MosResult<()> {
        conn.machine_adapter_mut()?.step_in()?;
        Ok(())
    }
}

struct StepOutRequestHandler {}

impl Handler<StepOutRequest> for StepOutRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: StepOutArguments) -> MosResult<()> {
        conn.machine_adapter_mut()?.step_out()?;
        Ok(())
    }
}

struct PauseRequestHandler {}

impl Handler<PauseRequest> for PauseRequestHandler {
    fn handle(&self, conn: &mut DebugSession, _args: PauseArguments) -> MosResult<()> {
        conn.machine_adapter_mut()?.pause()?;
        Ok(())
    }
}

struct EvaluateRequestHandler {}

impl EvaluateRequestHandler {
    fn evaluate(&self, conn: &mut DebugSession, expr: &str) -> MosResult<EvaluateResponse> {
        let (registers, flags, state) = {
            let adapter = conn.machine_adapter()?;
            let registers = adapter.registers()?;
            let flags = adapter.flags()?;
            let state = adapter.running_state()?;
            (registers, flags, state)
        };

        if let Some(mut codegen) = conn.codegen() {
            codegen.symbols_mut().ensure_cpu_symbols(registers, flags);
            if let Some(scope) = current_scope(&state, &codegen)? {
                let expression = parse_expression(expr)?;
                let evaluator = codegen.get_evaluator_for_scope(scope);
                let result = match evaluator.evaluate_expression(&expression, true) {
                    Ok(Some(val)) => val.to_string(),
                    Ok(None) => {
                        let ids = evaluator
                            .usages()
                            .into_iter()
                            .map(|u| u.path.data.to_string())
                            .join(", ");
                        format!("unknown identifier(s): {}", ids)
                    }
                    Err(e) => e.message,
                };
                return Ok(EvaluateResponse {
                    result,
                    ty: None,
                    presentation_hint: None,
                    variables_reference: 0,
                    named_variables: None,
                    indexed_variables: None,
                    memory_reference: None,
                });
            }
        }

        Err(Diagnostics::from(Diagnostic::error().with_message("could not evaluate")).into())
    }
}

impl Handler<EvaluateRequest> for EvaluateRequestHandler {
    fn handle(
        &self,
        conn: &mut DebugSession,
        args: EvaluateArguments,
    ) -> MosResult<EvaluateResponse> {
        self.evaluate(conn, &args.expression)
    }
}

struct CompletionsRequestHandler {}

impl CompletionsRequestHandler {
    fn local_symbols(&self, conn: &DebugSession) -> MosResult<Vec<String>> {
        if let Some(codegen) = conn.codegen() {
            let state = conn.machine_adapter()?.running_state()?;
            if let Some(scope) = current_scope(&state, &codegen)? {
                return Ok(codegen
                    .symbols()
                    .visible_symbols(scope, true)
                    .iter()
                    .filter(|(id, _)| !id.is_special())
                    .map(|(id, _)| id.to_string())
                    .collect());
            }
        }

        Ok(Vec::new())
    }
}

impl Handler<CompletionsRequest> for CompletionsRequestHandler {
    // TODO: prettify the signature. Why is CompletionsResponse not allowed?
    fn handle(
        &self,
        conn: &mut DebugSession,
        args: <CompletionsRequest as Request>::Arguments,
    ) -> MosResult<<CompletionsRequest as Request>::Response> {
        let registers = {
            let adapter = conn.machine_adapter()?;
            adapter.registers()?
        };

        let locals = self.local_symbols(conn)?;

        // gets the current symbol being completed.
        // assumes space separates each expression
        let curr_completion = {
            let (beginning, _) = args.text.split_at(args.column);
            match beginning.rsplit_once(' ') {
                Some((_, text)) => text,
                None => beginning,
            }
        };

        println!("Completion string: {}", curr_completion);

        // get the completion candidates
        let candidates: Vec<String> = if curr_completion.starts_with("cpu.flags.") {
            let (_, after_dot_current) = curr_completion.split_at(10);
            let flags = vec![
                "carry".to_string(),
                "zero".to_string(),
                "interrupt_disable".to_string(),
                "decimal".to_string(),
                "overflow".to_string(),
                "negative".to_string(),
            ];
            flags
                .iter()
                .filter(|sym| sym.starts_with(after_dot_current))
                .map(|sym| sym.to_string())
                .collect()
        } else if curr_completion.starts_with("cpu.") {
            let (_, after_dot_current) = curr_completion.split_at(4);
            let subcommands = vec!["flags".to_string()];
            registers
                .keys()
                .map(|kv_pair| kv_pair.to_lowercase())
                .chain(subcommands)
                .filter(|sym| sym.starts_with(after_dot_current))
                .collect()
        } else if curr_completion.starts_with("ram(") {
            let (_, after_dot_current) = curr_completion.split_at(4);
            locals
                .iter()
                .filter(|sym| sym.starts_with(after_dot_current))
                .map(|sym| sym.to_string())
                .collect()
        } else {
            // if no other matches, just give a few global symbols.
            // try to match if any of them are trying to be completed
            let globals = vec!["cpu".to_string(), "ram".to_string()];
            globals
                .iter()
                .chain(locals.iter())
                .filter(|sym| sym.starts_with(curr_completion))
                .map(|sym| sym.to_string())
                .collect()
        };

        Ok(CompletionsResponse {
            targets: candidates
                .iter()
                .map(|candidate| CompletionItem {
                    label: candidate.to_string(),
                })
                .collect(),
        })
    }
}

fn current_scope(
    state: &MachineRunningState,
    codegen: &CodegenContext,
) -> MosResult<Option<SymbolIndex>> {
    if let MachineRunningState::Stopped(pc) = state {
        if let Some(offset) = codegen.source_map().address_to_offset(*pc) {
            return Ok(Some(offset.scope));
        }
    }

    Ok(None)
}

impl DebugSession {
    pub fn new(lsp: Arc<Mutex<LspContext>>, port: u16) -> Self {
        Self {
            lsp,
            codegen: None,
            machine: None,
            port,
            conn: None,
            pending_events: vec![],
            no_debug: false,
            lines_start_at_1: false,
            columns_start_at_1: false,
        }
    }

    fn create_machine(&mut self, adapter: Box<dyn MachineAdapter + Send + Sync>) {
        if let Some(cg) = adapter.codegen() {
            self.codegen = Some(cg);
        } else {
            self.codegen = self.lsp.lock().unwrap().codegen();
        }
        let adapter = Arc::new(RwLock::new(adapter));

        if let Some(codegen) = &self.codegen {
            let mut codegen = codegen.lock().unwrap();
            ensure_ram_fn(
                &mut codegen,
                Box::new(MachineAdapterMemoryAccessor {
                    adapter: adapter.clone(),
                }),
            );
        }

        self.machine = Some(Machine::new(adapter));
    }

    fn line(&self, line: usize) -> usize {
        if self.lines_start_at_1 {
            line + 1
        } else {
            line
        }
    }

    fn column(&self, column: usize) -> usize {
        if self.columns_start_at_1 {
            column + 1
        } else {
            column
        }
    }

    fn lock_lsp(&self) -> MutexGuard<LspContext> {
        self.lsp.lock().unwrap()
    }

    fn codegen(&self) -> Option<MutexGuard<CodegenContext>> {
        self.codegen.as_ref().map(|cg| cg.lock().unwrap())
    }

    fn machine_adapter(&self) -> MosResult<RwLockReadGuard<Box<dyn MachineAdapter + Send + Sync>>> {
        match self.machine.as_ref() {
            Some(m) => Ok(m.adapter()),
            None => Err(Diagnostics::from(
                Diagnostic::error().with_message("Machine adapter not initialized"),
            )
            .into()),
        }
    }

    fn machine_adapter_mut(
        &self,
    ) -> MosResult<RwLockWriteGuard<Box<dyn MachineAdapter + Send + Sync>>> {
        match self.machine.as_ref() {
            Some(m) => Ok(m.adapter_mut()),
            None => Err(Diagnostics::from(
                Diagnostic::error().with_message("Machine adapter not initialized"),
            )
            .into()),
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
        let lsp_shutdown_receiver = self.lsp.lock().unwrap().add_shutdown_handler();

        loop {
            let mut sel = Select::new();

            // We may be receiving messages from the IDE...
            let receiver = &self.connection().unwrap().receiver;
            sel.recv(receiver);

            // ...or from the LSP telling us we should be shutting down...
            sel.recv(lsp_shutdown_receiver.receiver());

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
                0 => match oper.recv(receiver) {
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
                    CompletionsRequest::COMMAND => {
                        self.handle(&CompletionsRequestHandler {}, req.arguments)
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
                    _ => Err(Diagnostics::from(
                        Diagnostic::warning().with_message(format!(
                            "Received an unknown command: {:?}",
                            req.command
                        )),
                    )
                    .into()),
                };
                match response {
                    Ok(body) => self.send_response(seq, &command, body)?,
                    Err(e) => self.send_error(seq, &command, &e.to_string())?,
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
            MachineEvent::Message { output, location } => {
                self.enqueue_event::<OutputEvent>(OutputEventArguments {
                    category: Some("stdout".into()),
                    output,
                    source: location.as_ref().map(|sl| Source {
                        path: Some(sl.file.name().into()),
                        ..Default::default()
                    }),
                    line: location.as_ref().map(|sl| self.line(sl.begin.line)),
                    column: location.as_ref().map(|sl| self.column(sl.begin.column)),
                });
            }
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
            let events = std::mem::take(&mut self.pending_events);
            for e in events {
                conn.sender.send(e)?;
            }
        }
        self.pending_events.clear();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::testing::test_root;
    use crate::lsp::LspServer;
    use std::thread;
    use std::time::{Duration, Instant};

    #[test]
    fn evaluate() -> MosResult<()> {
        let src = r#".test "a" {
                         ldx #123
                         foo: nop
                         {
                             foo: asl
                         }
                         brk
                     }"#;
        let mut session = launch_session_and_break(
            src,
            vec![MachineBreakpoint {
                line: 2,
                column: None,
                range: ProgramCounter::new(0xc002)..ProgramCounter::new(0xc003),
            }],
        )?;

        let response = EvaluateRequestHandler {}.evaluate(&mut session, "ram(foo)")?;
        assert_eq!(response.result, "234".to_string()); // 234 = 'nop'

        let response = EvaluateRequestHandler {}.evaluate(&mut session, "cpu.x")?;
        assert_eq!(response.result, "123".to_string());

        let mut session = launch_session_and_break(
            src,
            vec![MachineBreakpoint {
                line: 4,
                column: None,
                range: ProgramCounter::new(0xc003)..ProgramCounter::new(0xc004),
            }],
        )?;

        let response = EvaluateRequestHandler {}.evaluate(&mut session, "ram(foo)")?;
        assert_eq!(response.result, "10".to_string()); // 10 = 'asl'

        let response = EvaluateRequestHandler {}.evaluate(&mut session, "invalid")?;
        assert_eq!(
            response.result,
            "unknown identifier(s): invalid".to_string()
        );

        Ok(())
    }

    #[test]
    fn completions() -> MosResult<()> {
        use mos_testing::assert_unordered_eq;

        // TODO: completing in the middle of the word should probably not return the entire word?
        //       or should it?

        let src = r#".test "a" {
                         ldx #123
                         foo: nop
                         {
                             foo: asl
                         }
                         brk
                     }"#;

        let mut session = launch_session_and_break(
            src,
            vec![MachineBreakpoint {
                line: 6,
                column: None,
                range: ProgramCounter::new(0xc002)..ProgramCounter::new(0xc003),
            }],
        )?;

        // global symbols (top level ones)
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: String::new(),
                    column: 0,
                    line: None,
                },
            )?
            .targets;
        let response_labels: Vec<&str> = response.iter().map(|item| item.label.as_str()).collect();
        assert_eq!(response_labels.len(), 6);
        assert_unordered_eq(
            &response_labels,
            &["cpu", "ram", "foo", "a", "TEST", "segments"],
        );

        // check that we get cpu back if we complete from c
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: "c".to_string(),
                    column: 1,
                    line: None,
                },
            )?
            .targets;
        assert_eq!(response.len(), 1);
        assert_eq!(response[0].label, "cpu".to_string());

        // that that cpu registers are completed successfully
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: "cpu.".to_string(),
                    column: 4,
                    line: None,
                },
            )?
            .targets;
        assert_eq!(response.len(), 5);
        let response_labels: Vec<&str> = response.iter().map(|item| item.label.as_str()).collect();
        assert_unordered_eq(&response_labels, &["a", "x", "y", "cyc", "flags"]);

        // test that we get all flags
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: "cpu.flags.".to_string(),
                    column: 10,
                    line: None,
                },
            )?
            .targets;
        assert_eq!(response.len(), 6);
        let response_labels: Vec<&str> = response.iter().map(|item| item.label.as_str()).collect();
        assert_unordered_eq(
            &response_labels,
            &[
                "carry",
                "zero",
                "interrupt_disable",
                "decimal",
                "overflow",
                "negative",
            ],
        );

        // test that global symbols/locals also complete successfully inside ram-block
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: "ram(".to_string(),
                    column: 4,
                    line: None,
                },
            )?
            .targets;
        let response_labels: Vec<&str> = response.iter().map(|item| item.label.as_str()).collect();
        assert_unordered_eq(&response_labels, &["foo", "a", "TEST", "segments"]);

        // test that we can also complete in the middle of a text
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: "cpu. == foo".to_string(),
                    column: 4,
                    line: None,
                },
            )?
            .targets;
        let response_labels: Vec<&str> = response.iter().map(|item| item.label.as_str()).collect();
        assert_unordered_eq(&response_labels, &["a", "x", "y", "cyc", "flags"]);

        // test that we can complete at the end
        let response = CompletionsRequestHandler {}
            .handle(
                &mut session,
                CompletionsArguments {
                    frame_id: None,
                    text: "foo == cpu.fl".to_string(),
                    column: 13,
                    line: None,
                },
            )?
            .targets;
        let response_labels: Vec<&str> = response.iter().map(|item| item.label.as_str()).collect();
        assert_unordered_eq(&response_labels, &["flags"]);

        Ok(())
    }

    fn launch_session_and_break(
        source: &str,
        breakpoints: Vec<MachineBreakpoint>,
    ) -> MosResult<DebugSession> {
        let server = get_lsp(source)?;
        let mut session = DebugSession::new(server.context(), 0);

        LaunchRequestHandler {}.handle(
            &mut session,
            LaunchRequestArguments {
                no_debug: None,
                workspace: test_root().to_str().unwrap().into(),
                vice_path: None,
                test_runner: Some(TestRunnerArguments {
                    test_case_name: "a".to_string(),
                }),
            },
        )?;

        session
            .machine_adapter_mut()?
            .set_breakpoints("main.asm", breakpoints)?;

        ConfigurationDoneRequestHandler {}.handle(&mut session, ())?;

        // Wait for breakpoint to hit
        let now = Instant::now();
        while !matches!(
            session.machine_adapter()?.running_state()?,
            MachineRunningState::Stopped(_)
        ) {
            thread::sleep(Duration::from_millis(50));
            if now.elapsed().as_secs() > 2 {
                panic!("waiting for breakpoint timed out");
            }
        }

        Ok(session)
    }

    fn get_lsp(source: &str) -> MosResult<LspServer> {
        let ctx = LspContext::new();
        ctx.parsing_source()
            .lock()
            .unwrap()
            .insert(&test_root().join("mos.toml"), "");
        let mut server = LspServer::new(ctx);
        server.did_open_text_document(test_root().join("main.asm"), source)?;
        Ok(server)
    }
}
