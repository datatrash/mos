pub mod protocol;

use crate::debugger::adapters::vice::protocol::*;
use crate::debugger::adapters::*;
use crate::memory_accessor::MemoryAccessor;
use codespan_reporting::diagnostic::Diagnostic;
use crossbeam_channel::{Receiver, RecvTimeoutError, Sender, unbounded};
use itertools::Itertools;
use mos_core::errors::Diagnostics;
use std::collections::HashMap;
use std::io::{ErrorKind, Read};
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

/// A single long-lived binary-monitor connection shared by every debug session that uses the same
/// VICE instance. The emulator's binary monitor does not reliably re-serve a *new* TCP connection
/// after a client disconnects, so this socket stays open for the whole lifetime of the VICE process
/// and is reused (via the Autostart 0xdd command) rather than being closed and reconnected.
struct SharedConnection {
    sender: Sender<ViceRequest>,
    receiver: Receiver<ViceResponse>,
}

/// A running VICE emulator process plus the monitor address it listens on. The emulator and its
/// connection are intentionally kept separate from any individual debug session ({@link
/// ViceAdapter}) so they can be reused for later sessions (Autostart) instead of being relaunched
/// or reconnected each time.
pub struct ViceProcess {
    _process: Option<Child>,
    port: u16,
    stderr: Arc<Mutex<Vec<u8>>>,
}

impl ViceProcess {
    /// Launches a fresh VICE with `-binarymonitor` and the given binary loaded, returning the
    /// process handle plus the monitor address to connect to.
    pub fn spawn<P: AsRef<Path>>(vice_path: &str, binary_path: P) -> MosResult<ViceProcess> {
        let binary_path = binary_path.as_ref();
        let port = find_available_port();
        let monitor_address = format!("ip4://127.0.0.1:{}", port);
        let args = vec![
            "-binarymonitor",
            "-binarymonitoraddress",
            &monitor_address,
            binary_path.to_str().unwrap(),
        ];
        log::debug!("Launching VICE with arguments: {:?}", args);

        // Launch VICE but make sure it doesn't inherit any stdout/stderr stuff from our main LSP, since that will cause the LSP
        // communication to break once VICE exits.
        let mut process = Command::new(vice_path)
            .args(&args)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()?;

        let stderr = child_stream_to_vec(process.stderr.take().expect("!stderr"));

        Ok(ViceProcess {
            _process: Some(process),
            port,
            stderr,
        })
    }

    /// Blocks until VICE accepts a TCP connection to its binary monitor, returning the stream.
    fn connect(&self) -> MosResult<TcpStream> {
        let mut attempts = 50;
        loop {
            if let Ok(s) = std::str::from_utf8(&self.stderr.lock().unwrap())
                && s.contains("Unknown option")
            {
                return Err(Diagnostics::from(
                    Diagnostic::error().with_message(
                        "Your version of VICE does not support the '-binarymonitor' flag. Please update to VICE 3.5 or newer.",
                    ),
                )
                .into());
            }

            let stream = TcpStream::connect_timeout(
                &format!("127.0.0.1:{}", self.port).parse().unwrap(),
                Duration::from_secs(1),
            );
            match stream {
                Ok(s) => return Ok(s),
                Err(e) if e.kind() == ErrorKind::ConnectionRefused => {
                    log::debug!("VICE refused connection...");
                    thread::sleep(Duration::from_millis(300));
                }
                Err(e) if e.kind() == ErrorKind::TimedOut => {
                    log::debug!("VICE connection timed out...");
                    thread::sleep(Duration::from_millis(300));
                }
                Err(e) => {
                    return Err(e.into());
                }
            }

            attempts -= 1;
            if attempts == 0 {
                log::error!("Unable to connect to VICE.");
                return Err(Diagnostics::from(
                    Diagnostic::error().with_message("Unable to connect to VICE"),
                )
                .into());
            }
        }
    }

    /// Force-quits the emulator process. Returns whether a process was actually terminated.
    fn quit(&mut self) -> bool {
        match self._process.take() {
            Some(mut child) => {
                let _ = child.kill();
                let _ = child.wait();
                true
            }
            None => false,
        }
    }
}

/// Owns the single reusable VICE emulator instance for the whole debug server. Keeps one VICE
/// process (and one open binary-monitor connection) alive across debug-session restarts so the
/// emulator is not relaunched each time; the current binary is reloaded via the binary-monitor
/// Autostart (0xdd) command over the same connection.
pub struct VicePool {
    default_process: Option<ViceProcess>,
    connection: Option<Arc<Mutex<SharedConnection>>>,
}

impl VicePool {
    pub fn new() -> Self {
        Self {
            default_process: None,
            connection: None,
        }
    }

    /// Returns an adapter shared with the pooled VICE instance, launching one if none is running
    /// yet, or autostarting {@code prg_path} into the existing instance over the same connection.
    pub fn obtain(
        &mut self,
        vice_path: &str,
        prg_path: &Path,
    ) -> MosResult<Box<dyn MachineAdapter + Send + Sync>> {
        if let Some(connection) = &self.connection {
            log::debug!("Reusing existing VICE instance via Autostart.");
            let mut adapter = ViceAdapter::from_shared(connection.clone());
            adapter.autostart(prg_path)?;
            return Ok(adapter);
        }
        log::debug!("Launching a new VICE instance.");
        let process = ViceProcess::spawn(vice_path, prg_path)?;
        let connection = Arc::new(Mutex::new(SharedConnection::connect(process.connect()?)));
        self.default_process = Some(process);
        self.connection = Some(connection.clone());
        Ok(ViceAdapter::from_shared(connection))
    }

    /// Quits any running VICE instance (used when the user stops debugging or on server shutdown).
    pub fn clear(&mut self) {
        self.connection = None;
        if let Some(mut process) = self.default_process.take() {
            let _ = process.quit();
        }
    }
}

pub struct ViceAdapter {
    _process: Option<Child>,
    connection: Arc<Mutex<SharedConnection>>,
    has_received_start: bool,
    is_connected: bool,
    running_state: MachineRunningState,
    available_banks: HashMap<u16, String>,
    available_registers: HashMap<u8, String>,
    current_register_values: HashMap<u8, u16>,
    breakpoints: Vec<ViceBreakpoint>,
    event_sender: Sender<MachineEvent>,
    event_receiver: Receiver<MachineEvent>,
    received_memory: Vec<u8>,
    stop_count: u64,
}

#[derive(Clone)]
struct ViceBreakpoint {
    id: Option<u32>,
    source_path: String,
    requested: MachineBreakpoint,
    range: Range<u16>,
}

impl MemoryAccessor for ViceAdapter {
    fn read(&mut self, address: u16, len: usize) -> Vec<u8> {
        let _ = self.send(ViceRequest::MemoryGet(MemoryDescriptor {
            cause_side_effects: false,
            start: address,
            end: address + len as u16 - 1,
            memory_space: 0,
            bank_id: 0,
        }));
        while self.received_memory.is_empty() {
            let _ = self.handle_responses(true);
        }
        std::mem::take(&mut self.received_memory)
    }

    fn write(&mut self, _address: u16, _bytes: &[u8]) {
        unimplemented!()
    }
}

impl MachineAdapter for ViceAdapter {
    fn codegen(&self) -> Option<Arc<Mutex<CodegenContext>>> {
        None
    }

    fn poll(&mut self) -> MosResult<()> {
        self.handle_responses(false)
    }

    fn receiver(&self) -> MosResult<Receiver<MachineEvent>> {
        Ok(self.event_receiver.clone())
    }

    fn start(&mut self) -> MosResult<()> {
        self.send(ViceRequest::BanksAvailable)?;
        self.send(ViceRequest::RegistersAvailable)?;
        self.send(ViceRequest::Exit)?;
        self.has_received_start = true;
        Ok(())
    }

    fn stop(&mut self) -> MosResult<()> {
        // Quit VICE and don't wait for the response
        self.connection
            .lock()
            .unwrap()
            .sender
            .send(ViceRequest::Quit)?;
        self.is_connected = false;
        Ok(())
    }

    /// Stops this adapter from using the shared connection, but deliberately leaves the socket and
    /// the VICE process intact so a subsequent debug session can reuse them (Autostart) instead of
    /// being relaunched. Called when a session is preempted by a new debug run.
    fn terminate(&mut self) -> MosResult<()> {
        self.is_connected = false;
        Ok(())
    }

    fn is_connected(&self) -> MosResult<bool> {
        Ok(self.is_connected)
    }

    fn running_state(&self) -> MosResult<MachineRunningState> {
        Ok(self.running_state)
    }

    fn resume(&mut self) -> MosResult<()> {
        if !matches!(self.running_state, MachineRunningState::Running) {
            self.send(ViceRequest::Exit)?;
        }
        Ok(())
    }

    fn pause(&mut self) -> MosResult<()> {
        if !matches!(self.running_state, MachineRunningState::Stopped(_)) {
            self.send(ViceRequest::AdvanceInstructions(false, 1))?;
        }
        Ok(())
    }

    fn next(&mut self) -> MosResult<()> {
        self.step(ViceRequest::AdvanceInstructions(true, 1))
    }

    fn step_in(&mut self) -> MosResult<()> {
        self.step(ViceRequest::AdvanceInstructions(false, 1))
    }

    fn step_out(&mut self) -> MosResult<()> {
        self.step(ViceRequest::ExecuteUntilReturn)
    }

    fn set_breakpoints(
        &mut self,
        source_path: &str,
        breakpoints: Vec<MachineBreakpoint>,
    ) -> MosResult<Vec<MachineValidatedBreakpoint>> {
        let was_running = matches!(&self.running_state, MachineRunningState::Running);

        // Delete all existing breakpoints for this source path
        let existing = std::mem::take(&mut self.breakpoints);
        for bp in existing {
            if bp.source_path == source_path {
                self.send(ViceRequest::CheckpointDelete(bp.id.unwrap() as u32))?;
            } else {
                self.breakpoints.push(bp);
            }
        }

        // Add the new breakpoints
        let new_vice_breakpoints = breakpoints
            .iter()
            .map(|bp| ViceBreakpoint {
                id: None,
                source_path: source_path.into(),
                requested: bp.clone(),
                range: bp.range.start.as_u16()..bp.range.end.as_u16(),
            })
            .collect_vec();
        self.breakpoints.extend(new_vice_breakpoints);
        for bp in breakpoints {
            let cs = CheckpointSet {
                start: bp.range.start.as_u16(),
                end: bp.range.end.as_u16() - 1, // Vice 'Checkpoint' ranges are inclusive
                stop_when_hit: true,
                enabled: true,
                cpu_operation: 4,
                temporary: false,
            };
            self.send(ViceRequest::CheckpointSet(cs))?;
        }

        // ...and continue execution if we were running before
        if was_running {
            self.resume()?;
        }

        // By this point all breakpoints should have received an ID, so filter out the ones that are not part of the current
        // source_path and we should end up with the right breakpoints again, in the order they were just added.
        let validated_breakpoints = self
            .breakpoints
            .iter()
            .filter(|bp| bp.source_path == source_path && bp.id.is_some())
            .map(|bp| MachineValidatedBreakpoint {
                id: bp.id.unwrap() as usize,
                source_path: bp.source_path.clone(),
                requested: bp.requested.clone(),
                range: Range {
                    start: (bp.range.start as usize).into(),
                    end: (bp.range.end as usize).into(),
                },
            })
            .collect();

        Ok(validated_breakpoints)
    }

    fn registers(&self) -> MosResult<HashMap<String, i64>> {
        Ok(self
            .current_register_values
            .iter()
            .filter_map(|(id, value)| {
                self.available_registers
                    .get(id)
                    .filter(|name| name.as_str() != "FL")
                    .map(|name| (name.clone(), *value as i64))
            })
            .collect())
    }

    fn flags(&self) -> MosResult<u8> {
        let result = self
            .available_registers
            .iter()
            .find(|(_, value)| value.as_str() == "FL")
            .and_then(|(flag_register_id, _)| self.current_register_values.get(flag_register_id));

        match result {
            Some(r) => Ok(*r as u8),
            None => Err(Diagnostics::from(
                Diagnostic::error().with_message("Expected VICE to return a 'FL' register"),
            )
            .into()),
        }
    }

    fn set_variable(&mut self, name: String, value: u8) -> MosResult<()> {
        // check if variable is a register
        let register_id = self
            .available_registers
            .iter()
            .find(|(_, reg_name)| reg_name.as_str() == name.as_str());

        if let Some((id, _)) = register_id {
            self.send(ViceRequest::RegistersSet(*id, value))?;
        }

        // from VICE docs (https://vice-emu.sourceforge.io/vice_13.html#SEC312):
        // "The transmission of any command causes the emulator to stop, similar to the regular monitor."
        // so resume after setting the variable
        self.resume()?;

        Ok(())
    }
}

impl ViceAdapter {
    fn from_connection(connection: Arc<Mutex<SharedConnection>>) -> Box<ViceAdapter> {
        let (event_sender, event_receiver) = unbounded();

        let adapter = Self {
            _process: None,
            connection,
            has_received_start: false,
            is_connected: true,
            running_state: MachineRunningState::Launching,
            available_banks: HashMap::new(),
            available_registers: HashMap::new(),
            current_register_values: HashMap::new(),
            breakpoints: vec![],
            event_sender,
            event_receiver,
            received_memory: vec![],
            stop_count: 0,
        };

        Box::new(adapter)
    }

    /// Builds a fresh adapter sharing the pooled VICE instance's long-lived connection. The
    /// connection (and the underlying VICE process) is owned by {@link VicePool}, so it survives
    /// across debug-session restarts instead of being relaunched or reconnected each time.
    fn from_shared(connection: Arc<Mutex<SharedConnection>>) -> Box<ViceAdapter> {
        Self::from_connection(connection)
    }

    /// Autoloads and runs {@code binary} into an already-running VICE via the binary-monitor
    /// Autostart (0xdd) command, so the emulator process does not need to be relaunched.
    fn autostart<P: AsRef<Path>>(&mut self, binary_path: P) -> MosResult<()> {
        let filename = binary_path.as_ref().to_string_lossy().into_owned();
        log::debug!("VICE: Autostarting {:?} into existing instance", filename);
        self.send(ViceRequest::Autostart(Autostart {
            run_after_loading: true,
            file_index: 0,
            filename,
        }))?;
        Ok(())
    }

    fn send(&mut self, req: ViceRequest) -> MosResult<()> {
        log::trace!("VICE: Sending request: {:?}", req);
        self.connection.lock().unwrap().sender.send(req)?;
        self.handle_responses(true)?;
        Ok(())
    }

    fn handle_responses(&mut self, block: bool) -> MosResult<()> {
        let connection = self.connection.clone();
        let connection = connection.lock().unwrap();
        let mut recv = if block {
            connection
                .receiver
                .recv()
                .map_err(|_| TryRecvError::Disconnected)
        } else {
            connection.receiver.try_recv()
        };

        while recv.is_ok() {
            let response = recv.ok().unwrap();
            self.handle_response(response)?;
            recv = connection.receiver.try_recv();
        }

        match recv.err().unwrap() {
            TryRecvError::Disconnected => {
                self.is_connected = false;
                self.event_sender.send(MachineEvent::Disconnected)?;
                Ok(())
            }
            TryRecvError::Empty => Ok(()),
        }
    }

    fn handle_response(&mut self, response: ViceResponse) -> MosResult<()> {
        log::trace!("VICE: Received response: {:?}", response);

        match response {
            ViceResponse::CheckpointResponse(cp) => {
                let bp = self
                    .breakpoints
                    .iter_mut()
                    .find(|bp| {
                        // cp.end + 1 because Vice 'Checkpoint' ranges are inclusive, and our 'bp.range' is exclusive
                        bp.range.start == cp.start && bp.range.end == cp.end + 1
                    })
                    .expect("Received a response for an unknown checkpoint");
                bp.id = Some(cp.number);
            }
            ViceResponse::BanksAvailable(map) => {
                self.available_banks = map;
            }
            ViceResponse::RegistersAvailable(map) => {
                self.available_registers = map;
            }
            ViceResponse::Registers(map) => {
                self.current_register_values = map;
            }
            ViceResponse::MemoryGet(bytes) => {
                self.received_memory = bytes;
            }
            ViceResponse::Exit => {
                let old = self.running_state;
                self.running_state = MachineRunningState::Running;

                if self.has_received_start {
                    self.event_sender.send(MachineEvent::RunningStateChanged {
                        old,
                        new: self.running_state,
                    })?;
                }
            }
            ViceResponse::Stopped(pc) => {
                let old = self.running_state;
                self.running_state = MachineRunningState::Stopped(ProgramCounter::new(pc as usize));
                self.stop_count += 1;

                if self.has_received_start {
                    self.event_sender.send(MachineEvent::RunningStateChanged {
                        old,
                        new: self.running_state,
                    })?;
                }
            }
            ViceResponse::Error {
                response_type,
                error_code,
            } => {
                anyhow::bail!(
                    "VICE response 0x{:02X} failed with error code 0x{:02X}",
                    response_type,
                    error_code
                );
            }
            ViceResponse::Unknown(response_type) => {
                log::warn!(
                    "Ignoring unknown VICE response type 0x{:02X}",
                    response_type
                );
            }
            _ => (),
        }
        Ok(())
    }

    /// Performs a single stepping request, keeping the user's breakpoints out of the way.
    ///
    /// VICE re-enters its monitor as soon as it receives *any* command, so re-enabling the
    /// breakpoints has to wait until the step has actually finished. A single instruction completes
    /// almost immediately, but stepping over a `jsr` runs the whole subroutine, and toggling too
    /// early used to strand the program counter somewhere inside it (typically in KERNAL or
    /// interrupt code) instead of on the next line.
    fn step(&mut self, request: ViceRequest) -> MosResult<()> {
        if !matches!(self.running_state, MachineRunningState::Stopped(_)) {
            return Ok(());
        }
        self.toggle_breakpoints(false)?;
        let stops_before = self.stop_count;
        self.send(request)?;
        self.wait_for_stop(stops_before)?;
        self.toggle_breakpoints(true)?;
        Ok(())
    }

    /// Waits until VICE reports that the machine has halted again.
    fn wait_for_stop(&mut self, stops_before: u64) -> MosResult<()> {
        let deadline = Instant::now() + Duration::from_secs(10);
        let connection = self.connection.clone();
        let connection = connection.lock().unwrap();
        while self.stop_count == stops_before {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                log::debug!("Timed out waiting for VICE to finish stepping.");
                break;
            }
            match connection.receiver.recv_timeout(remaining) {
                Ok(response) => self.handle_response(response)?,
                Err(RecvTimeoutError::Timeout) => {}
                Err(RecvTimeoutError::Disconnected) => {
                    self.is_connected = false;
                    self.event_sender.send(MachineEvent::Disconnected)?;
                    break;
                }
            }
        }
        Ok(())
    }

    fn toggle_breakpoints(&mut self, enabled: bool) -> MosResult<()> {
        let bps = self.breakpoints.clone();
        for bp in bps {
            self.send(ViceRequest::CheckpointToggle(bp.id.unwrap(), enabled))?;
        }
        Ok(())
    }
}

fn find_available_port() -> u16 {
    if let Ok(a) = TcpListener::bind("127.0.0.1:0") {
        if let Ok(a) = a.local_addr() {
            return a.port();
        }
    }

    panic!("No available port")
}

impl SharedConnection {
    /// Wraps an established monitor stream, spawning the single reader/writer thread pair that
    /// will serve it for the whole lifetime of the shared connection.
    fn connect(stream: TcpStream) -> SharedConnection {
        let (reader_receiver, _) = make_reader(stream.try_clone().unwrap());
        let (writer_sender, _) = make_writer(stream.try_clone().unwrap());
        SharedConnection {
            sender: writer_sender,
            receiver: reader_receiver,
        }
    }
}

fn make_reader(
    stream: TcpStream,
) -> (
    Receiver<ViceResponse>,
    thread::JoinHandle<std::io::Result<()>>,
) {
    let (reader_sender, reader_receiver) = bounded::<ViceResponse>(0);
    let reader = thread::spawn(move || {
        let mut buf_read = BufReader::new(stream);
        loop {
            match ViceResponse::read(&mut buf_read) {
                Ok(msg) => {
                    let result = reader_sender.send(msg);
                    if result.is_err() {
                        break;
                    }
                }
                Err(e) => {
                    log::error!("VICE protocol error: {:?}", e);
                    break;
                }
            }
        }

        log::debug!("VICE: Reader thread stopped.");
        Ok(())
    });
    (reader_receiver, reader)
}

fn make_writer(
    mut stream: TcpStream,
) -> (Sender<ViceRequest>, thread::JoinHandle<std::io::Result<()>>) {
    let (writer_sender, writer_receiver) = bounded::<ViceRequest>(0);
    let writer = thread::spawn(move || {
        if let Err(e) = writer_receiver
            .into_iter()
            .try_for_each(|it| it.write(&mut stream))
        {
            log::debug!("Could not write ViceRequests to receiver: {:?}", e);
        }

        log::debug!("VICE: Writer thread stopped.");
        Ok(())
    });
    (writer_sender, writer)
}

// Used for monitoring Vice's stderr
fn child_stream_to_vec<R>(mut stream: R) -> Arc<Mutex<Vec<u8>>>
where
    R: Read + Send + 'static,
{
    let out = Arc::new(Mutex::new(Vec::new()));
    let vec = out.clone();
    thread::Builder::new()
        .name("child_stream_to_vec".into())
        .spawn(move || {
            loop {
                let mut buf = [0];
                match stream.read(&mut buf) {
                    Err(err) => {
                        println!("{}] Error reading from stream: {}", line!(), err);
                        break;
                    }
                    Ok(got) => {
                        if got == 0 {
                            break;
                        } else if got == 1 {
                            vec.lock().expect("!lock").push(buf[0])
                        } else {
                            println!("{}] Unexpected number of bytes: {}", line!(), got);
                            break;
                        }
                    }
                }
            }
        })
        .expect("!thread");
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{HashMap, VecDeque};
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Arc, Mutex};
    use std::thread::JoinHandle;

    struct MockVice {
        expectations: Arc<Mutex<VecDeque<(ViceRequest, Vec<ViceResponse>)>>>,
        responses: Sender<ViceResponse>,
        thread: JoinHandle<()>,
        done: Arc<AtomicBool>,
    }

    impl MockVice {
        fn new(sender: Sender<ViceResponse>, receiver: Receiver<ViceRequest>) -> Self {
            let expectations = Arc::new(Mutex::new(VecDeque::new()));
            let exp = expectations.clone();
            let done = Arc::new(AtomicBool::new(false));
            let thread_done = done.clone();
            let responses = sender.clone();
            let thread = thread::spawn(move || {
                while !thread_done.load(Ordering::Relaxed) {
                    if let Ok(req) = receiver.recv_timeout(Duration::from_millis(10)) {
                        let (exp_req, responses) = exp
                            .lock()
                            .unwrap()
                            .pop_front()
                            .unwrap_or_else(|| panic!("Expected enqueued request: {:?}", req));
                        assert_eq!(req, exp_req);
                        for resp in responses {
                            sender.send(resp).unwrap();
                        }
                    }
                }
            });
            Self {
                expectations,
                responses,
                thread,
                done,
            }
        }

        /// Sends a response that was not triggered by a request, after a short delay.
        fn send_unsolicited_after(&self, delay: Duration, response: ViceResponse) {
            let responses = self.responses.clone();
            thread::spawn(move || {
                thread::sleep(delay);
                let _ = responses.send(response);
            });
        }

        fn disconnect(self) {
            self.done.store(true, Ordering::Relaxed);
            self.thread.join().unwrap();
        }

        fn enqueue(&self, request: ViceRequest, responses: &[ViceResponse]) {
            self.expectations
                .lock()
                .unwrap()
                .push_back((request, responses.to_vec()));
        }

        fn add_default_mocks(&self) {
            let mut available_banks = HashMap::new();
            available_banks.insert(1, "CPU".to_string());
            available_banks.insert(2, "RAM".to_string());
            let mut available_registers = HashMap::new();
            available_registers.insert(1, "A".to_string());
            available_registers.insert(2, "X".to_string());

            self.enqueue(
                ViceRequest::BanksAvailable,
                &[ViceResponse::BanksAvailable(available_banks)],
            );
            self.enqueue(
                ViceRequest::RegistersAvailable,
                &[ViceResponse::RegistersAvailable(available_registers)],
            );
            self.enqueue(ViceRequest::Exit, &[ViceResponse::Exit]);
        }
    }

    impl ViceAdapter {
        fn launch_in_memory() -> (Box<ViceAdapter>, MockVice) {
            let (response_sender, receiver) = bounded(100);
            let (sender, request_receiver) = bounded(100);
            let connection = Arc::new(Mutex::new(SharedConnection { receiver, sender }));
            let mock = MockVice::new(response_sender, request_receiver);
            (Self::from_connection(connection), mock)
        }
    }

    #[test]
    fn stop_resume() -> MosResult<()> {
        let (mut adapter, mock) = launch()?;

        mock.enqueue(
            ViceRequest::AdvanceInstructions(false, 1),
            &[
                ViceResponse::AdvanceInstructions,
                ViceResponse::Stopped(0x1234),
            ],
        );

        adapter.pause()?;
        assert_eq!(
            adapter.running_state()?,
            MachineRunningState::Stopped(0x1234.into())
        );

        mock.enqueue(ViceRequest::Exit, &[ViceResponse::Exit]);

        adapter.resume()?;
        assert_eq!(adapter.running_state()?, MachineRunningState::Running);

        Ok(())
    }

    /// Stepping must not report completion until VICE says the machine has halted again. Any
    /// command sent while the program is still running pulls VICE back into its monitor, which used
    /// to strand the program counter inside the subroutine being stepped over.
    #[test]
    fn stepping_waits_for_the_machine_to_halt() -> MosResult<()> {
        let (mut adapter, mock) = launch()?;

        mock.enqueue(
            ViceRequest::AdvanceInstructions(false, 1),
            &[
                ViceResponse::AdvanceInstructions,
                ViceResponse::Stopped(0x1234),
            ],
        );
        adapter.pause()?;

        mock.enqueue(
            ViceRequest::AdvanceInstructions(true, 1),
            &[
                ViceResponse::AdvanceInstructions,
                ViceResponse::Resumed(0x1234),
            ],
        );
        // The machine keeps running for a while, exactly like a `jsr` being stepped over.
        mock.send_unsolicited_after(Duration::from_millis(250), ViceResponse::Stopped(0x2000));

        adapter.next()?;

        assert_eq!(
            adapter.running_state()?,
            MachineRunningState::Stopped(0x2000.into())
        );

        Ok(())
    }

    #[test]
    fn disconnect() -> MosResult<()> {
        let (mut adapter, mock) = launch()?;
        assert!(adapter.is_connected()?);
        mock.disconnect();

        // When trying to handle pending messages the adapter will discover it is not connected anymore
        adapter.handle_responses(true)?;
        assert!(!adapter.is_connected()?);

        Ok(())
    }

    #[test]
    fn breakpoints() -> MosResult<()> {
        let (mut adapter, mock) = launch()?;

        // Pause the adapter so we don't get continual 'exit' messages
        adapter.running_state = MachineRunningState::Stopped(0.into());

        let cp = |r: Range<u16>| CheckpointSet {
            start: r.start,
            end: r.end,
            stop_when_hit: true,
            enabled: true,
            cpu_operation: 4,
            temporary: false,
        };
        let cr = |r: Range<u16>, number: u32| CheckpointResponse {
            number,
            start: r.start,
            end: r.end,
            ..Default::default()
        };
        let mvb = |r: Range<u16>, number: u32, source: &str, requested: &MachineBreakpoint| {
            MachineValidatedBreakpoint {
                id: number as usize,
                source_path: source.into(),
                requested: requested.clone(),
                range: Range {
                    start: (r.start as usize).into(),
                    end: (r.end as usize).into(),
                },
            }
        };

        // Create two breakpoints for source file "a"
        mock.enqueue(
            ViceRequest::CheckpointSet(cp(10..14)),
            &[ViceResponse::CheckpointResponse(cr(10..14, 1))],
        );
        mock.enqueue(
            ViceRequest::CheckpointSet(cp(20..24)),
            &[ViceResponse::CheckpointResponse(cr(20..24, 2))],
        );

        let mut a_bps = vec![
            MachineBreakpoint {
                line: 1,
                column: None,
                range: Range {
                    start: 10.into(),
                    end: 15.into(),
                },
            },
            MachineBreakpoint {
                line: 2,
                column: Some(22),
                range: Range {
                    start: 20.into(),
                    end: 25.into(),
                },
            },
        ];
        assert_eq!(
            adapter.set_breakpoints("a", a_bps.clone())?,
            &[
                mvb(10..15, 1, "a", &a_bps[0]),
                mvb(20..25, 2, "a", &a_bps[1])
            ]
        );

        // Create breakpoints for source file "b"
        mock.enqueue(
            ViceRequest::CheckpointSet(cp(30..34)),
            &[ViceResponse::CheckpointResponse(cr(30..34, 3))],
        );

        let b_bps = vec![MachineBreakpoint {
            line: 3,
            column: None,
            range: Range {
                start: 30.into(),
                end: 35.into(),
            },
        }];
        assert_eq!(
            adapter.set_breakpoints("b", b_bps.clone())?,
            &[mvb(30..35, 3, "b", &b_bps[0])]
        );

        // Now, remove the second checkpoint for source "a", which should cause both breakpoints to be deleted and then the first one
        // re-added
        mock.enqueue(
            ViceRequest::CheckpointDelete(1),
            &[ViceResponse::CheckpointDelete],
        );
        mock.enqueue(
            ViceRequest::CheckpointDelete(2),
            &[ViceResponse::CheckpointDelete],
        );
        mock.enqueue(
            ViceRequest::CheckpointSet(cp(10..14)),
            &[ViceResponse::CheckpointResponse(cr(10..14, 1))],
        );

        a_bps.remove(1);
        adapter.set_breakpoints("a", a_bps)?;

        // Resume the adapter and see if we get the correct resume messages when doing anything with breakpoints
        // (since VICE pauses automatically when changing breakpoints)
        adapter.running_state = MachineRunningState::Running;

        mock.enqueue(
            ViceRequest::CheckpointDelete(1),
            &[ViceResponse::CheckpointDelete],
        );
        mock.enqueue(ViceRequest::Exit, &[ViceResponse::Exit]);

        adapter.set_breakpoints("a", vec![])?;

        Ok(())
    }

    fn launch() -> MosResult<(Box<ViceAdapter>, MockVice)> {
        let (mut adapter, mock) = ViceAdapter::launch_in_memory();
        mock.add_default_mocks();
        assert_eq!(adapter.running_state()?, MachineRunningState::Launching);
        adapter.start()?;
        Ok((adapter, mock))
    }
}
