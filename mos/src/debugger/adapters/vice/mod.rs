pub mod protocol;

use crate::debugger::adapters::vice::protocol::*;
use crate::debugger::adapters::*;
use crossbeam_channel::{unbounded, Receiver, Sender};
use itertools::Itertools;
use std::collections::HashMap;
use std::net::TcpListener;
use std::process::{Child, Stdio};

struct ViceConnection {
    sender: Sender<ViceRequest>,
    receiver: Receiver<ViceResponse>,
}

pub struct ViceAdapter {
    _process: Option<Child>,
    connection: ViceConnection,
    has_received_start: bool,
    is_connected: bool,
    running_state: MachineRunningState,
    available_banks: HashMap<u16, String>,
    available_registers: HashMap<u8, String>,
    current_register_values: HashMap<u8, u16>,
    breakpoints: Vec<ViceBreakpoint>,
    event_sender: Sender<MachineEvent>,
    event_receiver: Receiver<MachineEvent>,
}

#[derive(Clone)]
struct ViceBreakpoint {
    id: Option<u32>,
    source_path: String,
    requested: MachineBreakpoint,
    range: Range<u16>,
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
        self.connection.sender.send(ViceRequest::Quit)?;
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
        if matches!(self.running_state, MachineRunningState::Stopped(_)) {
            self.toggle_breakpoints(false)?;
            self.send(ViceRequest::AdvanceInstructions(true, 1))?;
            self.toggle_breakpoints(true)?;
        }
        Ok(())
    }

    fn step_in(&mut self) -> MosResult<()> {
        if matches!(self.running_state, MachineRunningState::Stopped(_)) {
            self.toggle_breakpoints(false)?;
            self.send(ViceRequest::AdvanceInstructions(false, 1))?;
            self.toggle_breakpoints(true)?;
        }
        Ok(())
    }

    fn step_out(&mut self) -> MosResult<()> {
        if matches!(self.running_state, MachineRunningState::Stopped(_)) {
            self.toggle_breakpoints(false)?;
            self.send(ViceRequest::ExecuteUntilReturn)?;
            self.toggle_breakpoints(true)?;
        }
        Ok(())
    }

    fn set_breakpoints(
        &mut self,
        source_path: &str,
        breakpoints: Vec<MachineBreakpoint>,
    ) -> MosResult<Vec<MachineValidatedBreakpoint>> {
        let was_running = matches!(&self.running_state, MachineRunningState::Running);

        // Delete all existing breakpoints for this source path
        let existing = std::mem::replace(&mut self.breakpoints, vec![]);
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
                end: bp.range.end.as_u16(),
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
            .map(|(flag_register_id, _)| self.current_register_values.get(flag_register_id))
            .flatten();

        match result {
            Some(r) => Ok(*r as u8),
            None => Err(MosError::Unknown),
        }
    }
}

impl ViceAdapter {
    fn from_connection(process: Option<Child>, connection: ViceConnection) -> Box<ViceAdapter> {
        let (event_sender, event_receiver) = unbounded();

        let adapter = Self {
            _process: process,
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
        };

        Box::new(adapter)
    }

    pub fn launch<P: Into<PathBuf>>(
        launch_args: &LaunchRequestArguments,
        binary_path: P,
    ) -> MosResult<Box<dyn MachineAdapter + Send>> {
        let binary_path = binary_path.into();
        let port = find_available_port();
        let monitor_address = format!("ip4://127.0.0.1:{}", port);
        let args = vec![
            "-binarymonitor",
            "-binarymonitoraddress",
            &monitor_address,
            binary_path.to_str().unwrap(),
        ];
        log::debug!("Launching VICE with arguments: {:?}", args);

        // Launch VICE but make sure it doesn't inherit any stdout/stderr stuff from our main LSP, since that will cause the LSp
        // communication to break once VICE exits.
        let process = Command::new(launch_args.vice_path.as_ref().unwrap())
            .args(&args)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()?;

        let mut attempts = 50;
        let stream = loop {
            let stream = TcpStream::connect_timeout(
                &format!("127.0.0.1:{}", port).parse().unwrap(),
                Duration::from_secs(1),
            );
            match stream {
                Ok(s) => break s,
                Err(e) if e.kind() == ErrorKind::ConnectionRefused => {
                    log::debug!("VICE refused connection...");
                    thread::sleep(Duration::from_millis(300));
                }
                Err(e) => {
                    let m: MosError = e.into();
                    return Err(m);
                }
            }

            attempts -= 1;
            if attempts == 0 {
                log::error!("Unable to connect to VICE.");
                return Err(MosError::Vice("Unable to connect to VICE".into()));
            }
        };
        log::debug!("VICE is connected.");

        let (reader_receiver, _) = make_reader(stream.try_clone().unwrap());
        let (writer_sender, _) = make_writer(stream.try_clone().unwrap());

        let connection = ViceConnection {
            receiver: reader_receiver,
            sender: writer_sender,
        };

        Ok(Self::from_connection(Some(process), connection))
    }

    fn send(&mut self, req: ViceRequest) -> MosResult<()> {
        log::trace!("VICE: Sending request: {:?}", req);
        self.connection.sender.send(req)?;
        self.handle_responses(true)?;
        Ok(())
    }

    fn handle_responses(&mut self, block: bool) -> MosResult<()> {
        let mut recv = if block {
            self.connection
                .receiver
                .recv()
                .map_err(|_| TryRecvError::Disconnected)
        } else {
            self.connection.receiver.try_recv()
        };

        while recv.is_ok() {
            let response = recv.ok().unwrap();
            self.handle_response(response)?;
            recv = self.connection.receiver.try_recv();
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
                    .find(|bp| bp.range.start == cp.start && bp.range.end == cp.end)
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

                if self.has_received_start {
                    self.event_sender.send(MachineEvent::RunningStateChanged {
                        old,
                        new: self.running_state,
                    })?;
                }
            }
            _ => (),
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
                    log::debug!("Could not handle ViceResponse: {:?}", e);
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{HashMap, VecDeque};
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Arc, Mutex};
    use std::thread::JoinHandle;

    struct MockVice {
        expectations: Arc<Mutex<VecDeque<(ViceRequest, Vec<ViceResponse>)>>>,
        thread: JoinHandle<()>,
        done: Arc<AtomicBool>,
    }

    impl MockVice {
        fn new(sender: Sender<ViceResponse>, receiver: Receiver<ViceRequest>) -> Self {
            let expectations = Arc::new(Mutex::new(VecDeque::new()));
            let exp = expectations.clone();
            let done = Arc::new(AtomicBool::new(false));
            let thread_done = done.clone();
            let thread = thread::spawn(move || {
                while !thread_done.load(Ordering::Relaxed) {
                    if let Ok(req) = receiver.recv_timeout(Duration::from_millis(10)) {
                        let (exp_req, responses) = exp
                            .lock()
                            .unwrap()
                            .pop_front()
                            .expect(&format!("Expected enqueued request: {:?}", req));
                        assert_eq!(req, exp_req);
                        for resp in responses {
                            sender.send(resp).unwrap();
                        }
                    }
                }
            });
            Self {
                expectations,
                thread,
                done,
            }
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
            let connection = ViceConnection { receiver, sender };
            let mock = MockVice::new(response_sender, request_receiver);
            (Self::from_connection(None, connection), mock)
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

    #[test]
    fn disconnect() -> MosResult<()> {
        let (mut adapter, mock) = launch()?;
        assert_eq!(adapter.is_connected()?, true);
        mock.disconnect();

        // When trying to handle pending messages the adapter will discover it is not connected anymore
        adapter.handle_responses(true)?;
        assert_eq!(adapter.is_connected()?, false);

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
            ViceRequest::CheckpointSet(cp(10..15)),
            &[ViceResponse::CheckpointResponse(cr(10..15, 1))],
        );
        mock.enqueue(
            ViceRequest::CheckpointSet(cp(20..25)),
            &[ViceResponse::CheckpointResponse(cr(20..25, 2))],
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
            ViceRequest::CheckpointSet(cp(30..35)),
            &[ViceResponse::CheckpointResponse(cr(30..35, 3))],
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
            ViceRequest::CheckpointSet(cp(10..15)),
            &[ViceResponse::CheckpointResponse(cr(10..15, 1))],
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
