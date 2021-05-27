pub mod test_runner;
pub mod vice;

use crate::debugger::types::LaunchRequestArguments;
use crate::errors::{MosError, MosResult};
use crossbeam_channel::{bounded, Receiver, TryRecvError};
use mos_core::codegen::{CodegenContext, ProgramCounter};
use std::collections::HashMap;
use std::io::{BufReader, ErrorKind};
use std::net::TcpStream;
use std::ops::Range;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

pub struct Machine {
    adapter: Arc<Mutex<Box<dyn MachineAdapter + Send>>>,
    poller: JoinHandle<MosResult<()>>,
}

impl Machine {
    pub fn new(adapter: Box<dyn MachineAdapter + Send>) -> Self {
        let adapter = Arc::new(Mutex::new(adapter));

        let adap = adapter.clone();
        let poller = thread::spawn(move || {
            log::debug!("Starting machine poller thread.");
            loop {
                {
                    let mut adap = adap.lock().unwrap();
                    if !adap.is_connected()? {
                        break;
                    }
                    adap.poll()?;
                }

                thread::sleep(Duration::from_millis(50));
            }
            log::debug!("Shutting down machine poller thread.");
            Ok(())
        });

        Self { adapter, poller }
    }

    pub fn adapter(&self) -> MutexGuard<Box<dyn MachineAdapter + Send>> {
        self.adapter.lock().unwrap()
    }

    pub fn join(self) {
        let _ = self.poller.join().unwrap();
    }
}

#[derive(Debug, PartialEq)]
pub enum MachineEvent {
    RunningStateChanged {
        old: MachineRunningState,
        new: MachineRunningState,
    },
    Disconnected,
}

pub trait MachineAdapter {
    /// If the adapter is doing its own code generation instead of the one that the LSP is doing, we can grab that here
    /// to generate the breakpoint mappings etc
    fn codegen(&self) -> Option<Arc<Mutex<CodegenContext>>>;

    /// Poll the underlying machine for data and handle any events that may have occured
    fn poll(&mut self) -> MosResult<()>;
    /// The receiver for any events that may originate from the underlying machine
    fn receiver(&self) -> MosResult<Receiver<MachineEvent>>;

    /// Transition the underlying machine from launching to running
    fn start(&mut self) -> MosResult<()>;
    /// Stops the underlying machine and kills any associated processes
    fn stop(&mut self) -> MosResult<()>;
    /// Is the underlying machine still connected?
    fn is_connected(&self) -> MosResult<bool>;
    /// What is the current running state?
    fn running_state(&self) -> MosResult<MachineRunningState>;

    /// When paused, resume
    fn resume(&mut self) -> MosResult<()>;
    /// When running, pause
    fn pause(&mut self) -> MosResult<()>;
    /// When paused, go to the next instruction
    fn next(&mut self) -> MosResult<()>;
    /// When paused, step in to the next instruction (also step into subroutines)
    fn step_in(&mut self) -> MosResult<()>;
    /// When paused, step out of a subroutine
    fn step_out(&mut self) -> MosResult<()>;

    /// Set all breakpoints for a specific source path
    fn set_breakpoints(
        &mut self,
        source_path: &str,
        breakpoints: Vec<MachineBreakpoint>,
    ) -> MosResult<Vec<MachineValidatedBreakpoint>>;

    /// Gets the current register values
    fn registers(&self) -> MosResult<HashMap<String, u16>>;

    /// Get the cpu flags
    fn flags(&self) -> MosResult<u8>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct MachineBreakpoint {
    pub line: usize,
    pub column: Option<usize>,
    pub range: Range<ProgramCounter>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MachineValidatedBreakpoint {
    pub id: usize,
    pub source_path: String,
    pub requested: MachineBreakpoint,
    pub range: Range<ProgramCounter>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MachineRunningState {
    Launching,
    Running,
    Stopped(ProgramCounter),
}
