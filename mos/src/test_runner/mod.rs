use crate::errors::MosResult;
use emulator_6502::{Interface6502, MOS6502};
use itertools::Itertools;
use mos_core::codegen::{codegen, CodegenContext, CodegenOptions, SymbolType};
use mos_core::parser;
use mos_core::parser::source::ParsingSource;
use mos_core::parser::IdentifierPath;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub struct TestRunner {
    ctx: CodegenContext,
    ram: BasicRam,
    cpu: MOS6502,
    num_cycles: usize,
}

#[derive(Debug, PartialEq)]
pub enum CycleResult {
    Running,
    TestFailed(usize, String),
    TestSuccess(usize),
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
        .collect();
    Ok(test_cases)
}

impl TestRunner {
    pub fn new(
        src: Arc<Mutex<dyn ParsingSource>>,
        input_path: &Path,
        test_path: &IdentifierPath,
    ) -> MosResult<Self> {
        let ctx = generate(
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

        Ok(Self {
            ctx,
            ram,
            cpu,
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

            for assertion in active_assertions {
                if assertion.pc.as_u16() == self.cpu.get_program_counter() {
                    let eval_result = self.ctx.evaluate_expression(&assertion.expression)?;
                    if eval_result == 0 {
                        let msg = assertion.failure_message.clone().unwrap_or_else(|| {
                            format!("{}", &assertion.expression.data).trim().into()
                        });
                        return Ok(CycleResult::TestFailed(self.num_cycles, msg));
                    }
                }
            }
        }

        if self.cpu.get_program_counter() == 0 {
            // BRK caused the PC to jump to zero, so let's bail
            self.num_cycles -= 1; // ignore the BRK
            return Ok(CycleResult::TestFailed(
                self.num_cycles,
                "encountered an invalid instruction".into(),
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
    fn failing_assertion_with_message() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".test a {\nnop\n.assert 1 == 2 \"oh no\"\nrts\n}",
            )
            .into();
        let mut runner = TestRunner::new(src, Path::new("test.asm"), &idpath!("a"))?;
        assert_eq!(runner.run()?, CycleResult::TestFailed(2, "oh no".into()));
        Ok(())
    }

    #[test]
    fn failing_assertion_without_message() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add("test.asm", ".test a {\nnop\n.assert 1 == 2\nrts\n}")
            .into();
        let mut runner = TestRunner::new(src, Path::new("test.asm"), &idpath!("a"))?;
        assert_eq!(runner.run()?, CycleResult::TestFailed(2, "1 == 2".into()));
        Ok(())
    }
}
