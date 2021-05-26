use crate::errors::MosResult;
use emulator_6502::{Interface6502, MOS6502};
use itertools::Itertools;
use mos_core::codegen::{
    codegen, Assertion, CodegenContext, CodegenOptions, Symbol, SymbolIndex, SymbolType,
};
use mos_core::parser;
use mos_core::parser::code_map::SpanLoc;
use mos_core::parser::source::ParsingSource;
use mos_core::parser::IdentifierPath;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub struct TestRunner {
    ctx: CodegenContext,
    ram: BasicRam,
    cpu: MOS6502,
    cpu_pc_nx: SymbolIndex,
    num_cycles: usize,
}

#[derive(Debug, PartialEq)]
pub enum CycleResult {
    Running,
    TestFailed(usize, Box<TestFailure>),
    TestSuccess(usize),
}

#[derive(Debug, PartialEq)]
pub struct TestFailure {
    pub location: Option<SpanLoc>,
    pub message: String,
    pub assertion: Option<Assertion>,
    pub cpu: MOS6502,
}

pub fn enumerate_test_cases(
    src: Arc<Mutex<dyn ParsingSource>>,
    input_path: &Path,
) -> MosResult<Vec<IdentifierPath>> {
    let ctx = generate(
        src,
        input_path,
        CodegenOptions {
            pc: 0x2000.into(),
            ..Default::default()
        },
    )?;
    let test_cases = ctx
        .symbols()
        .all()
        .into_iter()
        .filter(|(_, data)| data.ty == SymbolType::TestCase)
        .map(|(name, _)| name)
        .sorted()
        .collect();
    Ok(test_cases)
}

impl TestRunner {
    pub fn new(
        src: Arc<Mutex<dyn ParsingSource>>,
        input_path: &Path,
        test_path: &IdentifierPath,
    ) -> MosResult<Self> {
        let mut ctx = generate(
            src,
            input_path,
            CodegenOptions {
                pc: 0x2000.into(),
                active_test: Some(test_path.clone()),
            },
        )?;

        let test_pc = ctx
            .symbols()
            .try_index(ctx.symbols().root, test_path)
            .map(|nx| ctx.symbols().try_get(nx))
            .flatten()
            .map(|symbol| symbol.data.as_i64())
            .unwrap();

        let mut ram = BasicRam::new();
        for segment in ctx.segments().values() {
            ram.load_program(segment.range().start, segment.range_data());
        }
        let mut cpu = MOS6502::new();
        cpu.set_program_counter(test_pc as u16);

        let root = ctx.symbols().root;
        let cpu_nx = ctx.symbols_mut().ensure_index(root, "cpu");
        let cpu_pc_nx = ctx.symbols_mut().insert(cpu_nx, "pc", Symbol::label(0, 0));

        Ok(Self {
            ctx,
            ram,
            cpu,
            cpu_pc_nx,
            num_cycles: 0,
        })
    }

    pub fn run(&mut self) -> MosResult<CycleResult> {
        loop {
            let result = self.cycle()?;
            if let CycleResult::Running = result {
                continue;
            }
            return Ok(result);
        }
    }

    pub fn cycle(&mut self) -> MosResult<CycleResult> {
        log::trace!(
            "PC: {} (ram: ${:02X})",
            self.cpu.get_program_counter(),
            self.ram.ram[self.cpu.get_program_counter() as usize]
        );

        // Check assertions
        if self.cpu.get_remaining_cycles() == 0 {
            let active_assertions = self
                .ctx
                .assertions()
                .iter()
                .filter(|a| a.pc.as_u16() == self.cpu.get_program_counter())
                .cloned()
                .collect_vec();

            if !active_assertions.is_empty() {
                self.ctx.symbols_mut().update_data(
                    self.cpu_pc_nx,
                    Symbol::label(0, self.cpu.get_program_counter() as i64),
                );
            }

            for assertion in active_assertions {
                let eval_result = self.ctx.evaluate_expression(&assertion.expression)?;
                if eval_result == 0 {
                    let message = assertion
                        .failure_message
                        .clone()
                        .unwrap_or_else(|| format!("{}", &assertion.expression.data).trim().into());
                    let location = self.ctx.analysis().look_up(assertion.expression.span);
                    let failure = TestFailure {
                        location: Some(location),
                        message,
                        assertion: Some(assertion),
                        cpu: self.cpu.clone(),
                    };
                    return Ok(CycleResult::TestFailed(self.num_cycles, Box::new(failure)));
                }
            }
        }

        if self.cpu.get_program_counter() == 0 {
            // BRK caused the program counter to go to zero
            self.num_cycles -= 1; // ignore the BRK
            return Ok(CycleResult::TestFailed(
                self.num_cycles,
                Box::new(TestFailure {
                    location: None,
                    message: "encountered an invalid instruction".into(),
                    assertion: None,
                    cpu: self.cpu.clone(),
                }),
            ));
        }

        if self.cpu.get_remaining_cycles() == 0
            && self.ram.ram[self.cpu.get_program_counter() as usize] == 0x60
        {
            // RTS, test succeeded
            return Ok(CycleResult::TestSuccess(self.num_cycles));
        }

        self.cpu.cycle(&mut self.ram);
        self.num_cycles += 1;

        Ok(CycleResult::Running)
    }
}

fn generate(
    src: Arc<Mutex<dyn ParsingSource>>,
    input_path: &Path,
    options: CodegenOptions,
) -> MosResult<CodegenContext> {
    let (tree, error) = parser::parse(input_path, src);
    if let Some(e) = error {
        return Err(e.into());
    }
    let tree = tree.unwrap();
    let (generated_code, error) = codegen(tree, options);
    if let Some(error) = error {
        return Err(error.into());
    }
    Ok(generated_code.unwrap())
}

struct BasicRam {
    ram: Vec<u8>,
}

impl BasicRam {
    fn new() -> Self {
        Self {
            ram: vec![0; 65536],
        }
    }

    fn load_program(&mut self, start: usize, data: &[u8]) {
        self.ram[start..start + data.len()].clone_from_slice(data);
    }
}

impl Interface6502 for BasicRam {
    fn read(&mut self, address: u16) -> u8 {
        self.ram[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.ram[address as usize] = data
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mos_core::idpath;
    use mos_core::parser::source::InMemoryParsingSource;
    use mos_testing::assert_unordered_eq;

    #[test]
    fn can_enumerate_tests() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".test a {rts}\n.test b {rts}")
            .into();
        let cases = enumerate_test_cases(src, Path::new("test.asm"))?;
        assert_unordered_eq(&cases, &[idpath!("a"), idpath!("b")]);
        Ok(())
    }

    #[test]
    fn use_pc_in_assertions() -> MosResult<()> {
        let mut runner = get_runner(
            ".test a {\nnop\n.assert cpu.pc == $2001\nrts\n}",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, CycleResult::TestSuccess(2));
        Ok(())
    }

    #[test]
    fn failing_assertion_with_message() -> MosResult<()> {
        let mut runner = get_runner(
            ".test a {\nnop\n.assert 1 == 2 \"oh no\"\nrts\n}",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?.as_failure().message, "oh no");
        Ok(())
    }

    #[test]
    fn failing_assertion_without_message() -> MosResult<()> {
        let mut runner = get_runner(".test a {\nnop\n.assert 1 == 2\nrts\n}", idpath!("a"))?;
        assert_eq!(runner.run()?.as_failure().message, "1 == 2");
        Ok(())
    }

    fn get_runner(source: &str, test_path: IdentifierPath) -> MosResult<TestRunner> {
        let src = InMemoryParsingSource::new().add("test.asm", source).into();
        Ok(TestRunner::new(src, Path::new("test.asm"), &test_path)?)
    }

    impl CycleResult {
        fn as_failure(&self) -> &TestFailure {
            match self {
                CycleResult::TestFailed(_, failure) => failure.as_ref(),
                _ => panic!(),
            }
        }
    }
}
