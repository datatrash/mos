use crate::debugger::adapters::{
    MachineAdapter, MachineBreakpoint, MachineEvent, MachineRunningState,
    MachineValidatedBreakpoint,
};
use crate::debugger::types::LaunchRequestArguments;
use crate::errors::MosResult;
use crate::test_runner::TestRunner;
use crossbeam_channel::{unbounded, Receiver, Sender};
use mos_core::codegen::{CodegenContext, ProgramCounter};
use mos_core::parser::source::ParsingSource;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

pub struct TestRunnerAdapter {
    is_connected: Arc<AtomicBool>,
    state: Arc<Mutex<MachineRunningState>>,
    runner: Arc<Mutex<TestRunner>>,
    ctx: Arc<Mutex<CodegenContext>>,
    event_sender: Sender<MachineEvent>,
    event_receiver: Receiver<MachineEvent>,
}

impl TestRunnerAdapter {
    pub fn launch<P: Into<PathBuf>>(
        _launch_args: &LaunchRequestArguments,
        parsing_source: Arc<Mutex<dyn ParsingSource>>,
        source_path: P,
    ) -> MosResult<Box<dyn MachineAdapter + Send>> {
        let is_connected = Arc::new(AtomicBool::new(true));
        let state = Arc::new(Mutex::new(MachineRunningState::Launching));

        let (event_sender, event_receiver) = unbounded();
        let runner = Arc::new(Mutex::new(TestRunner::new(
            parsing_source,
            &source_path.into(),
            &mos_core::idpath!("stack_pointer"),
        )?));

        let thread_is_connected = is_connected.clone();
        let thread_state = state.clone();
        let thread_runner = runner.clone();
        thread::spawn(move || {
            while thread_is_connected.load(Ordering::Relaxed) {
                let state = *thread_state.lock().unwrap();
                match state {
                    MachineRunningState::Launching | MachineRunningState::Stopped(_) => {
                        thread::sleep(Duration::from_millis(50));
                    }
                    MachineRunningState::Running => {
                        let mut runner = thread_runner.lock().unwrap();
                        match runner.cycle() {
                            Ok(_) => {
                                // Give rest of core a chance to do something
                                thread::sleep(Duration::from_millis(0));
                            }
                            Err(_e) => {
                                // Do something
                            }
                        }
                    }
                }
            }
        });

        let ctx = runner.lock().unwrap().codegen();
        Ok(Box::new(Self {
            is_connected,
            state,
            runner,
            ctx,
            event_sender,
            event_receiver,
        }))
    }
}

impl MachineAdapter for TestRunnerAdapter {
    fn codegen(&self) -> Option<Arc<Mutex<CodegenContext>>> {
        Some(self.ctx.clone())
    }

    fn poll(&mut self) -> MosResult<()> {
        Ok(())
    }

    fn receiver(&self) -> MosResult<Receiver<MachineEvent>> {
        Ok(self.event_receiver.clone())
    }

    fn start(&mut self) -> MosResult<()> {
        *self.state.lock().unwrap() = MachineRunningState::Running;
        Ok(())
    }

    fn stop(&mut self) -> MosResult<()> {
        self.is_connected.store(false, Ordering::Relaxed);
        Ok(())
    }

    fn is_connected(&self) -> MosResult<bool> {
        Ok(self.is_connected.load(Ordering::Relaxed))
    }

    fn running_state(&self) -> MosResult<MachineRunningState> {
        Ok(*self.state.lock().unwrap())
    }

    fn resume(&mut self) -> MosResult<()> {
        let mut state = self.state.lock().unwrap();
        let old = *state;
        let new = MachineRunningState::Running;
        *state = new;
        self.event_sender
            .send(MachineEvent::RunningStateChanged { old, new })?;
        Ok(())
    }

    fn pause(&mut self) -> MosResult<()> {
        let pc = self.runner.lock().unwrap().cpu().get_program_counter();
        let mut state = self.state.lock().unwrap();
        let old = *state;
        let new = MachineRunningState::Stopped(ProgramCounter::new(pc as usize));
        *state = new;
        self.event_sender
            .send(MachineEvent::RunningStateChanged { old, new })?;
        Ok(())
    }

    fn next(&mut self) -> MosResult<()> {
        Ok(())
    }

    fn step_in(&mut self) -> MosResult<()> {
        Ok(())
    }

    fn step_out(&mut self) -> MosResult<()> {
        Ok(())
    }

    fn set_breakpoints(
        &mut self,
        _source_path: &str,
        breakpoints: Vec<MachineBreakpoint>,
    ) -> MosResult<Vec<MachineValidatedBreakpoint>> {
        dbg!(&breakpoints);
        Ok(vec![])
    }

    fn registers(&self) -> MosResult<HashMap<String, u16>> {
        Ok(HashMap::new())
    }

    fn flags(&self) -> MosResult<u8> {
        Ok(0)
    }
}
