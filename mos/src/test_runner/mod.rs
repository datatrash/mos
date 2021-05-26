use crate::errors::MosResult;
use emulator_6502::{Interface6502, MOS6502};
use mos_core::codegen::{codegen, CodegenContext, CodegenOptions, SymbolType};
use mos_core::parser;
use mos_core::parser::source::FileSystemParsingSource;
use mos_core::parser::IdentifierPath;
use std::path::Path;

pub struct TestRunner {
    #[allow(dead_code)]
    ctx: CodegenContext,
    ram: BasicRam,
    cpu: MOS6502,
    num_cycles: usize,
}

pub enum CycleResult {
    Running,
    TestFailed(usize, String),
    TestSuccess(usize),
}

pub fn enumerate_test_cases(input_path: &Path) -> MosResult<Vec<IdentifierPath>> {
    let ctx = generate(
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
    pub fn new(input_path: &Path, test_path: &IdentifierPath) -> MosResult<Self> {
        let ctx = generate(
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
        if self.cpu.get_program_counter() == 0 {
            // BRK caused the PC to jump to zero, so let's bail
            self.num_cycles -= 1; // ignore the BRK
            return Ok(CycleResult::TestFailed(self.num_cycles, "oh no".into()));
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

fn generate(input_path: &Path, options: CodegenOptions) -> MosResult<CodegenContext> {
    let source = FileSystemParsingSource::new();
    let (tree, error) = parser::parse(input_path, source.into());
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
