use crate::errors::{MosError, MosResult};
use crate::utils::paint;
use ansi_term::Colour;
use emulator_6502::{Interface6502, MOS6502};
use itertools::Itertools;
use mos_core::codegen::{
    codegen, Assertion, CodegenContext, CodegenOptions, EvaluationResult, Evaluator,
    FunctionCallback, Symbol, SymbolIndex, SymbolTable, SymbolType, TestElement, Trace,
};
use mos_core::parser;
use mos_core::parser::code_map::SpanLoc;
use mos_core::parser::source::ParsingSource;
use mos_core::parser::{Expression, IdentifierPath, Located};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::DerefMut;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub struct TestRunner {
    ctx: Arc<Mutex<CodegenContext>>,
    test_elements: Vec<TestElement>,
    ram: Arc<Mutex<BasicRam>>,
    cpu: MOS6502,
    num_cycles: usize,
    formatted_traces: Vec<FormattedTrace>,
}

#[derive(Debug, PartialEq)]
pub enum ExecuteResult {
    Running,
    TestFailed(usize, Box<TestFailure>),
    TestSuccess(usize),
}

pub struct TestFailure {
    pub location: Option<SpanLoc>,
    pub message: String,
    pub assertion: Option<Assertion>,
    pub cpu: MOS6502,
    pub traces: Vec<FormattedTrace>,
}

impl Debug for TestFailure {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl PartialEq for TestFailure {
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
            && self.message == other.message
            && self.traces == other.traces
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormattedTrace(String);

impl Display for FormattedTrace {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn format_cpu_details(cpu: &MOS6502, use_color: bool) -> String {
    let flags = cpu.get_status_register();
    let fmt = |f: char, b: u8| if b != 0 { f } else { '-' };
    let flags = vec![
        fmt('N', flags & 128),
        fmt('V', flags & 64),
        '-',
        fmt('B', flags & 16),
        fmt('D', flags & 8),
        fmt('I', flags & 4),
        fmt('Z', flags & 2),
        fmt('C', flags & 1),
    ];
    let flags: String = flags.into_iter().collect();

    format!(
        "* = {}, SP = {}, flags = {}, A = {}, X = {}, Y = {}",
        paint(
            use_color,
            Colour::Yellow,
            format!("${:04X}", cpu.get_program_counter())
        ),
        paint(
            use_color,
            Colour::Yellow,
            format!("${:02X}", cpu.get_stack_pointer())
        ),
        paint(use_color, Colour::Yellow, flags),
        paint(
            use_color,
            Colour::Yellow,
            format!("${:02X}", cpu.get_accumulator())
        ),
        paint(
            use_color,
            Colour::Yellow,
            format!("${:02X}", cpu.get_x_register())
        ),
        paint(
            use_color,
            Colour::Yellow,
            format!("${:02X}", cpu.get_y_register())
        )
    )
}

pub fn enumerate_test_cases(
    src: Arc<Mutex<dyn ParsingSource>>,
    input_path: &Path,
) -> MosResult<Vec<(SpanLoc, IdentifierPath)>> {
    let mut ctx = generate(
        src,
        input_path,
        CodegenOptions {
            pc: 0x2000.into(),
            ..Default::default()
        },
    )?;
    struct DummyFn;
    impl FunctionCallback for DummyFn {
        fn expected_args(&self) -> usize {
            1
        }

        fn apply(
            &self,
            _: &Evaluator,
            _: &[&Located<Expression>],
        ) -> EvaluationResult<Option<i64>> {
            Ok(None)
        }
    }
    ctx.register_fn("ram", DummyFn {});
    ctx.register_fn("ram16", DummyFn {});
    let test_cases = ctx
        .symbols()
        .all()
        .into_iter()
        .filter(|(_, data)| data.ty == SymbolType::TestCase && data.span.is_some())
        .map(|(name, data)| {
            let location = ctx.analysis().look_up(data.span.unwrap());
            (location, name)
        })
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
        let mut predefined_constants = HashMap::new();
        predefined_constants.insert("TEST".into(), 1);

        let mut ctx = generate(
            src,
            input_path,
            CodegenOptions {
                pc: 0x2000.into(),
                active_test: Some(test_path.clone()),
                predefined_constants,
            },
        )?;

        let test_pc = ctx
            .symbols()
            .try_index(ctx.symbols().root, test_path)
            .map(|nx| ctx.symbols().try_get(nx))
            .flatten()
            .map(|symbol| symbol.data.as_i64());

        let test_pc = match test_pc {
            Some(pc) => pc,
            None => {
                return Err(MosError::UnitTest(format!(
                    "Test case not found: {}",
                    test_path
                )));
            }
        };

        let ram = Arc::new(Mutex::new(BasicRam::new()));
        for segment in ctx.segments().values() {
            ram.lock()
                .unwrap()
                .load_program(segment.range().start, segment.range_data());
        }
        let mut cpu = MOS6502::new();
        cpu.set_program_counter(test_pc as u16);

        struct RamFn {
            ram: Arc<Mutex<BasicRam>>,
            word: bool,
        }
        impl FunctionCallback for RamFn {
            fn expected_args(&self) -> usize {
                1
            }

            fn apply(
                &self,
                ctx: &Evaluator,
                args: &[&Located<Expression>],
            ) -> EvaluationResult<Option<i64>> {
                let expr = args.first().unwrap();
                match ctx.evaluate_expression(expr, false) {
                    Ok(result) => Ok(result.map(|address| {
                        let address = address as usize;
                        let ram = &self.ram.lock().unwrap().ram;
                        if self.word {
                            let lo = ram.get(address).cloned().unwrap_or_default() as i64;
                            let hi = ram.get(address + 1).cloned().unwrap_or_default() as i64;
                            hi * 256 + lo
                        } else {
                            ram.get(address).cloned().unwrap_or_default() as i64
                        }
                    })),
                    Err(_) => Ok(None),
                }
            }
        }
        ctx.register_fn(
            "ram",
            RamFn {
                ram: ram.clone(),
                word: false,
            },
        );
        ctx.register_fn(
            "ram16",
            RamFn {
                ram: ram.clone(),
                word: true,
            },
        );

        let test_elements = ctx.remove_test_elements();
        Ok(Self {
            ctx: Arc::new(Mutex::new(ctx)),
            test_elements,
            ram,
            cpu,
            num_cycles: 0,
            formatted_traces: vec![],
        })
    }

    pub fn cpu(&self) -> &MOS6502 {
        &self.cpu
    }

    pub fn num_cycles(&self) -> usize {
        self.num_cycles
    }

    pub fn codegen(&self) -> Arc<Mutex<CodegenContext>> {
        self.ctx.clone()
    }

    pub fn run(&mut self) -> MosResult<ExecuteResult> {
        loop {
            let result = self.execute_instruction()?;
            if let ExecuteResult::Running = result {
                continue;
            }
            return Ok(result);
        }
    }

    pub fn execute_instruction(&mut self) -> MosResult<ExecuteResult> {
        log::trace!(
            "PC: {} (ram: ${:02X})",
            self.cpu.get_program_counter(),
            self.ram.lock().unwrap().ram[self.cpu.get_program_counter() as usize]
        );

        // Check active elements
        let mut active_traces = vec![];
        let mut active_assertions = vec![];
        let mut idx = 0;
        while idx < self.test_elements.len() {
            let should_extract = match &self.test_elements[idx] {
                TestElement::Assertion(e) => {
                    e.extracted_evaluator.pc.as_u16() == self.cpu.get_program_counter()
                }
                TestElement::Trace(e) => {
                    e.extracted_evaluator.pc.as_u16() == self.cpu.get_program_counter()
                }
            };

            if should_extract {
                match self.test_elements.remove(idx) {
                    TestElement::Assertion(a) => {
                        active_assertions.push(a);
                    }
                    TestElement::Trace(t) => {
                        active_traces.push(t);
                    }
                }
            } else {
                idx += 1;
            }
        }

        for mut trace in active_traces {
            let fmt = match trace.exprs.is_empty() {
                true => format_cpu_details(&self.cpu, false),
                false => {
                    self.add_symbols(&mut trace.extracted_evaluator.symbols);
                    format_trace(trace, &self.ctx.lock().unwrap())?
                }
            };
            self.formatted_traces.push(FormattedTrace(fmt));
        }

        for mut assertion in active_assertions {
            self.add_symbols(&mut assertion.extracted_evaluator.symbols);
            let ctx = self.ctx.lock().unwrap();
            let evaluator = assertion
                .extracted_evaluator
                .get_evaluator(&ctx.functions());
            let eval_result = evaluator
                .evaluate_expression(&assertion.expr, false)
                .ok()
                .flatten();
            if eval_result == Some(0) || eval_result.is_none() {
                let message = assertion
                    .failure_message
                    .clone()
                    .unwrap_or_else(|| format!("{}", &assertion.expr.data).trim().into());
                let location = ctx.analysis().look_up(assertion.expr.span);
                let failure = TestFailure {
                    location: Some(location),
                    message,
                    assertion: Some(assertion),
                    cpu: self.cpu.clone(),
                    traces: self.formatted_traces.clone(),
                };
                return Ok(ExecuteResult::TestFailed(
                    self.num_cycles,
                    Box::new(failure),
                ));
            }
        }

        if self.ram.lock().unwrap().ram[self.cpu.get_program_counter() as usize] == 0 {
            // BRK, test succeeded
            return Ok(ExecuteResult::TestSuccess(self.num_cycles));
        }

        self.cpu.cycle(self.ram.lock().unwrap().deref_mut());
        self.num_cycles += 1 + self.cpu.get_remaining_cycles() as usize;
        self.cpu
            .execute_instruction(self.ram.lock().unwrap().deref_mut());

        Ok(ExecuteResult::Running)
    }

    pub fn step_over(&mut self) -> MosResult<ExecuteResult> {
        let opcode = self.ram.lock().unwrap().ram[self.cpu.get_program_counter() as usize];
        match opcode {
            0x20 => {
                // jsr
                let wait_until_pc = self.cpu.get_program_counter() + 3;
                loop {
                    let result = self.execute_instruction()?;

                    if self.cpu.get_program_counter() == wait_until_pc {
                        return Ok(result);
                    }

                    match result {
                        ExecuteResult::Running => {}
                        result => {
                            return Ok(result);
                        }
                    }
                }
            }
            _ => self.execute_instruction(),
        }
    }

    pub fn step_out(&mut self) -> MosResult<ExecuteResult> {
        if self.cpu.get_stack_pointer() > 253 {
            // Nothing to step out to
            return Ok(ExecuteResult::Running);
        }

        let sp_lo =
            self.ram.lock().unwrap().ram[256 + self.cpu.get_stack_pointer() as usize + 1] as usize;
        let sp_hi =
            self.ram.lock().unwrap().ram[256 + self.cpu.get_stack_pointer() as usize + 2] as usize;
        let will_return_to = 1 + sp_lo + 256 * sp_hi;

        loop {
            if self.cpu.get_program_counter() == will_return_to as u16 {
                return Ok(ExecuteResult::Running);
            }

            match self.execute_instruction()? {
                ExecuteResult::Running => {}
                result => {
                    return Ok(result);
                }
            }
        }
    }

    fn add_symbols(&self, symbols: &mut SymbolTable<Symbol>) {
        let root = symbols.root;
        let cpu_nx = symbols.ensure_index(root, "cpu");
        let cpu_flags_nx = symbols.ensure_index(cpu_nx, "flags");

        let mut add = |parent_nx: SymbolIndex, id: &str, data: i64, ty: SymbolType| {
            let symbol = Symbol {
                pass_idx: 0,
                span: None,
                data: data.into(),
                ty,
            };
            symbols.insert(parent_nx, id, symbol);
        };

        add(
            cpu_nx,
            "sp",
            self.cpu.get_stack_pointer() as i64,
            SymbolType::Label,
        );
        add(
            cpu_nx,
            "a",
            self.cpu.get_accumulator() as i64,
            SymbolType::Constant,
        );
        add(
            cpu_nx,
            "x",
            self.cpu.get_x_register() as i64,
            SymbolType::Constant,
        );
        add(
            cpu_nx,
            "y",
            self.cpu.get_y_register() as i64,
            SymbolType::Constant,
        );

        add(
            cpu_flags_nx,
            "carry",
            (self.cpu.get_status_register() & 1) as i64,
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "zero",
            (self.cpu.get_status_register() & 2) as i64,
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "interrupt_disable",
            (self.cpu.get_status_register() & 4) as i64,
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "decimal",
            (self.cpu.get_status_register() & 8) as i64,
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "overflow",
            (self.cpu.get_status_register() & 64) as i64,
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "negative",
            (self.cpu.get_status_register() & 128) as i64,
            SymbolType::Constant,
        );
    }
}

fn format_trace(trace: Trace, ctx: &CodegenContext) -> MosResult<String> {
    let mut eval = vec![];
    for expr in &trace.exprs {
        let evaluator = trace.extracted_evaluator.get_evaluator(ctx.functions());
        let value = evaluator.evaluate_expression(expr, false).ok().flatten();

        let value = match value {
            Some(value) => {
                if value < 256 {
                    format!("${:02X}", value)
                } else {
                    format!("${:04X}", value)
                }
            }
            None => "<unknown>".into(),
        };
        eval.push(format!("{} = {}", &expr.data, value));
    }
    Ok(eval.join(", "))
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
    fn step_over() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                jsr foo
                brk
           foo: nop
                rts
            }",
            idpath!("a"),
        )?;
        runner.step_over()?;
        assert_eq!(runner.cpu.get_program_counter(), 0x2003);
        Ok(())
    }

    #[test]
    fn step_out() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                jsr foo
                brk
           foo: nop
                rts
            }",
            idpath!("a"),
        )?;
        runner.execute_instruction()?;
        assert_eq!(runner.cpu.get_program_counter(), 0x2004);
        runner.step_out()?;
        assert_eq!(runner.cpu.get_program_counter(), 0x2003);
        Ok(())
    }

    #[test]
    fn can_enumerate_tests() -> MosResult<()> {
        let src = InMemoryParsingSource::new()
            .add(
                "test.asm",
                ".test a {brk}\n.test b {brk}\nscope: {\n.test c {brk}\n}",
            )
            .into();
        let cases = enumerate_test_cases(src, Path::new("test.asm"))?;
        let cases = cases.into_iter().map(|(_, c)| c).collect_vec();
        assert_unordered_eq(&cases, &[idpath!("a"), idpath!("b"), idpath!("scope.c")]);
        Ok(())
    }

    #[test]
    fn use_pc_sp_in_assertions() -> MosResult<()> {
        let mut runner = get_runner(
            ".test a {\nnop\n.assert * == $2001\n.assert cpu.sp == $fd\nbrk\n}",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, ExecuteResult::TestSuccess(2));
        Ok(())
    }

    #[test]
    fn use_index_in_assertions() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                .loop 2 {
                    .assert * == $2000 + index
                    nop
                 }
                 brk
             }",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, ExecuteResult::TestSuccess(4));
        Ok(())
    }

    #[test]
    fn use_registers_in_assertions() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                lda #$1
                ldx #$2
                ldy #$3
                .assert cpu.a == 1
                .assert cpu.x == 2
                .assert cpu.y == 3
                brk
             }",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, ExecuteResult::TestSuccess(6));
        Ok(())
    }

    #[test]
    fn use_flags_in_assertions() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                cli
                lda #1
                .assert !cpu.flags.zero
                .assert !cpu.flags.carry
                .assert !cpu.flags.interrupt_disable
                .assert !cpu.flags.decimal
                .assert !cpu.flags.overflow
                .assert !cpu.flags.negative
                sec
                .assert cpu.flags.carry
                ldx #$01
                dex
                .assert cpu.flags.zero
                sei
                .assert cpu.flags.interrupt_disable
                sed
                .assert cpu.flags.decimal
                lda #127
                clc
                adc #1
                .assert cpu.flags.overflow
                lda #0
                sbc #1
                .assert cpu.flags.negative
                brk
             }",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, ExecuteResult::TestSuccess(24));
        Ok(())
    }

    #[test]
    fn use_ram_in_assertions() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                lda #1
                sta foo
                lda #2
                sta foo + 1
                .assert ram(foo) == 1
                .assert ram16(foo) == 513
                brk
                foo: .word 0
             }",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, ExecuteResult::TestSuccess(12));
        Ok(())
    }

    #[test]
    fn nested_lookups() -> MosResult<()> {
        let mut runner = get_runner(
            r"
            .test a {
                .const foo = 1
                label: {
                    .const foo = 2
                    .assert foo == 2
                    .assert super.foo == 1
                }
                .assert foo == 1
                .assert label.foo == 2
                brk
             }",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?, ExecuteResult::TestSuccess(0));
        Ok(())
    }

    #[test]
    fn can_determine_code_is_in_test_mode() -> MosResult<()> {
        let runner = get_runner(
            r"
            .test a {
                .if defined(TEST) { nop } else { asl }
                brk
             }",
            idpath!("a"),
        )?;
        let ctx = runner.ctx.lock().unwrap();
        let segment = ctx.segments().values().next().unwrap();
        assert_eq!(segment.range_data(), vec![0xea, 0]);
        Ok(())
    }

    #[test]
    fn failing_assertion_with_message() -> MosResult<()> {
        let mut runner = get_runner(
            ".test a {\nnop\n.assert 1 == 2 \"oh no\"\nbrk\n}",
            idpath!("a"),
        )?;
        assert_eq!(runner.run()?.as_failure().message, "oh no");
        Ok(())
    }

    #[test]
    fn failing_assertion_without_message() -> MosResult<()> {
        let mut runner = get_runner(".test a {\nnop\n.assert 1 == 2\nbrk\n}", idpath!("a"))?;
        assert_eq!(runner.run()?.as_failure().message, "1 == 2");
        Ok(())
    }

    #[test]
    fn trace_formatting() -> MosResult<()> {
        let mut runner = get_runner(
            r#"
        .test a {
            .trace
            .trace (*, cpu.sp, foo, ram($2000))
            .loop 2 { .trace (index) }
            .assert 1 == 2
        }
        "#,
            idpath!("a"),
        )?;
        assert_eq!(
            runner.run()?.as_failure().traces,
            vec![
                FormattedTrace(
                    "* = $2000, SP = $FD, flags = -----I--, A = $00, X = $00, Y = $00".into()
                ),
                FormattedTrace("* = $2000, cpu.sp = $FD, foo = <unknown>, ram($2000) = $00".into()),
                FormattedTrace("index = $00".into()),
                FormattedTrace("index = $01".into()),
            ]
        );
        Ok(())
    }

    fn get_runner(source: &str, test_path: IdentifierPath) -> MosResult<TestRunner> {
        let src = InMemoryParsingSource::new().add("test.asm", source).into();
        Ok(TestRunner::new(src, Path::new("test.asm"), &test_path)?)
    }

    impl ExecuteResult {
        fn as_failure(&self) -> &TestFailure {
            match self {
                ExecuteResult::TestFailed(_, failure) => failure.as_ref(),
                _ => panic!(),
            }
        }
    }
}
