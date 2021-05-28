use crate::debugger::adapters::{
    MachineAdapter, MachineBreakpoint, MachineEvent, MachineRunningState,
    MachineValidatedBreakpoint,
};
use crate::debugger::types::LaunchRequestArguments;
use crate::errors::MosResult;
use crate::test_runner::{format_cpu_details, ExecuteResult, TestRunner};
use crate::utils::paint;
use ansi_term::Colour;
use crossbeam_channel::{unbounded, Receiver, Sender};
use mos_core::codegen::{CodegenContext, ProgramCounter};
use mos_core::parser::source::ParsingSource;
use mos_core::parser::IdentifierPath;
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
    breakpoints: Arc<Mutex<Vec<MachineBreakpoint>>>,
}

impl TestRunnerAdapter {
    pub fn new<P: Into<PathBuf>>(
        no_debug: bool,
        parsing_source: Arc<Mutex<dyn ParsingSource>>,
        source_path: P,
        test_case_path: &IdentifierPath,
    ) -> MosResult<Self> {
        let is_connected = Arc::new(AtomicBool::new(true));
        let state = Arc::new(Mutex::new(MachineRunningState::Launching));
        let breakpoints: Arc<Mutex<Vec<MachineBreakpoint>>> = Arc::new(Mutex::new(vec![]));

        let (event_sender, event_receiver) = unbounded();
        let runner = Arc::new(Mutex::new(TestRunner::new(
            parsing_source,
            &source_path.into(),
            test_case_path,
        )?));

        let thread_is_connected = is_connected.clone();
        let thread_state = state.clone();
        let thread_runner = runner.clone();
        let thread_breakpoints = breakpoints.clone();
        let thread_sender = event_sender.clone();
        let thread_test_case_path = test_case_path.clone();
        thread::spawn(move || {
            let mut last_checked_pc = None;
            while thread_is_connected.load(Ordering::Relaxed) {
                let state = *thread_state.lock().unwrap();
                match state {
                    MachineRunningState::Launching | MachineRunningState::Stopped(_) => {
                        thread::sleep(Duration::from_millis(50));
                    }
                    MachineRunningState::Running => {
                        let mut runner = thread_runner.lock().unwrap();

                        let pc = ProgramCounter::new(runner.cpu().get_program_counter() as usize);
                        if last_checked_pc != Some(pc) && !no_debug {
                            last_checked_pc = Some(pc);
                            let bps = thread_breakpoints.lock().unwrap();
                            if bps
                                .iter()
                                .any(|bp| bp.range.start <= pc && bp.range.end >= pc)
                            {
                                let mut state = thread_state.lock().unwrap();
                                let old = *state;
                                let new = MachineRunningState::Stopped(pc);
                                *state = new;
                                thread_sender
                                    .send(MachineEvent::RunningStateChanged { old, new })
                                    .unwrap();
                                continue;
                            }
                        }

                        match runner.execute_instruction() {
                            Ok(result) => {
                                // Give rest of core a chance to do something
                                thread::sleep(Duration::from_millis(0));

                                match result {
                                    ExecuteResult::Running => {}
                                    ExecuteResult::TestFailed(cycles, failure) => {
                                        let _ = thread_sender.send(MachineEvent::Message {
                                            output: format!(
                                                "test {} {} {}: {}\n{}",
                                                thread_test_case_path,
                                                paint(true, Colour::Red, "FAILED"),
                                                paint(
                                                    true,
                                                    Colour::Yellow,
                                                    format!("({} cycles)", cycles)
                                                ),
                                                failure.message,
                                                format_cpu_details(&failure.cpu, true)
                                            ),
                                            location: failure.location,
                                        });
                                        let _ = thread_sender.send(MachineEvent::Disconnected);
                                        thread_is_connected.store(false, Ordering::Relaxed);
                                    }
                                    ExecuteResult::TestSuccess(cycles) => {
                                        let _ = thread_sender.send(MachineEvent::Message {
                                            output: format!(
                                                "test {} {} {}",
                                                thread_test_case_path,
                                                paint(true, Colour::Green, "ok"),
                                                paint(
                                                    true,
                                                    Colour::Yellow,
                                                    format!("({} cycles)", cycles)
                                                )
                                            ),
                                            location: None,
                                        });
                                        let _ = thread_sender.send(MachineEvent::Disconnected);
                                        thread_is_connected.store(false, Ordering::Relaxed);
                                    }
                                }
                            }
                            Err(e) => {
                                log::error!("Error when executing instructions: {}", e);
                                let _ = thread_sender.send(MachineEvent::Disconnected);
                                thread_is_connected.store(false, Ordering::Relaxed);
                            }
                        }
                    }
                }
            }
        });

        let ctx = runner.lock().unwrap().codegen();
        Ok(Self {
            is_connected,
            state,
            runner,
            ctx,
            event_sender,
            event_receiver,
            breakpoints,
        })
    }

    pub fn launch<P: Into<PathBuf>>(
        launch_args: &LaunchRequestArguments,
        parsing_source: Arc<Mutex<dyn ParsingSource>>,
        source_path: P,
        test_case_path: &IdentifierPath,
    ) -> MosResult<Box<dyn MachineAdapter + Send>> {
        Ok(Box::new(Self::new(
            launch_args.no_debug.unwrap_or_default(),
            parsing_source,
            source_path,
            test_case_path,
        )?))
    }

    fn update_state(&mut self, new: MachineRunningState) -> MosResult<()> {
        let mut state = self.state.lock().unwrap();
        let old = *state;
        *state = new;
        self.event_sender
            .send(MachineEvent::RunningStateChanged { old, new })?;
        Ok(())
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
        self.update_state(MachineRunningState::Running)?;
        Ok(())
    }

    fn pause(&mut self) -> MosResult<()> {
        let pc = self.runner.lock().unwrap().cpu().get_program_counter();
        self.update_state(MachineRunningState::Stopped(ProgramCounter::new(
            pc as usize,
        )))?;
        Ok(())
    }

    fn next(&mut self) -> MosResult<()> {
        {
            let mut runner = self.runner.lock().unwrap();
            runner.step_over()?;
        }
        self.pause()?;
        Ok(())
    }

    fn step_in(&mut self) -> MosResult<()> {
        {
            let mut runner = self.runner.lock().unwrap();
            runner.execute_instruction()?;
        }
        self.pause()?;
        Ok(())
    }

    fn step_out(&mut self) -> MosResult<()> {
        {
            let mut runner = self.runner.lock().unwrap();
            runner.step_out()?;
        }
        self.pause()?;
        Ok(())
    }

    fn set_breakpoints(
        &mut self,
        source_path: &str,
        breakpoints: Vec<MachineBreakpoint>,
    ) -> MosResult<Vec<MachineValidatedBreakpoint>> {
        *self.breakpoints.lock().unwrap() = breakpoints.clone();
        Ok(breakpoints
            .into_iter()
            .enumerate()
            .map(|(id, bp)| MachineValidatedBreakpoint {
                id,
                source_path: source_path.into(),
                range: bp.range.clone(),
                requested: bp,
            })
            .collect())
    }

    fn registers(&self) -> MosResult<HashMap<String, i64>> {
        let runner = self.runner.lock().unwrap();
        let cpu = runner.cpu();

        let mut map = HashMap::new();
        map.insert("A".into(), cpu.get_accumulator() as i64);
        map.insert("X".into(), cpu.get_x_register() as i64);
        map.insert("Y".into(), cpu.get_y_register() as i64);
        map.insert("CYC".into(), runner.num_cycles() as i64);
        Ok(map)
    }

    fn flags(&self) -> MosResult<u8> {
        Ok(self.runner.lock().unwrap().cpu().get_status_register())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mos_core::idpath;
    use mos_core::parser::source::InMemoryParsingSource;

    #[test]
    fn breakpoints() -> MosResult<()> {
        let source = InMemoryParsingSource::new()
            .add("test.asm", ".test a {\nnop\nnop\n}")
            .into();
        let mut adapter = TestRunnerAdapter::new(false, source, "test.asm", &idpath!("a"))?;
        adapter.set_breakpoints(
            "test.asm",
            vec![MachineBreakpoint {
                line: 2,
                column: None,
                range: ProgramCounter::new(0x2001)..ProgramCounter::new(0x2001),
            }],
        )?;
        adapter.start()?;
        let event = adapter.event_receiver.recv()?;
        assert!(matches!(
            event,
            MachineEvent::RunningStateChanged {
                old: MachineRunningState::Running,
                new: MachineRunningState::Stopped(_)
            }
        ));
        Ok(())
    }
}
