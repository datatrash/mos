use mos_core::codegen::{CodegenContext, EvaluationResult, Evaluator, FunctionCallback};
use mos_core::parser::{Expression, Located};
use std::sync::{Arc, Mutex};

pub trait MemoryAccessor {
    fn read(&mut self, address: u16, len: usize) -> Vec<u8>;
    fn write(&mut self, address: u16, bytes: &[u8]);
}

pub fn ensure_ram_fn(
    ctx: &mut CodegenContext,
    memory_accessor: Box<dyn MemoryAccessor + 'static + Send + Sync>,
) {
    let memory_accessor = Arc::new(Mutex::new(memory_accessor));

    struct RamFn {
        memory_accessor: Arc<Mutex<Box<dyn MemoryAccessor + 'static + Send + Sync>>>,
        word: bool,
    }

    impl FunctionCallback for RamFn {
        fn expected_args(&self) -> usize {
            1
        }

        fn apply(
            &mut self,
            ctx: &Evaluator,
            args: &[&Located<Expression>],
        ) -> EvaluationResult<Option<i64>> {
            let arg = args.first().unwrap();
            let address = ctx.evaluate_expression(arg, false)?;
            let val = address.and_then(|a| {
                let len = if self.word { 2 } else { 1 };
                let bytes = self.memory_accessor.lock().unwrap().read(a as u16, len);
                if self.word {
                    let lo = bytes.get(0);
                    let hi = bytes.get(1);
                    match (lo, hi) {
                        (Some(lo), Some(hi)) => Some(256 * (*hi as i64) + (*lo as i64)),
                        _ => None,
                    }
                } else {
                    bytes.get(0).map(|b| *b as i64)
                }
            });
            Ok(val)
        }
    }

    ctx.register_fn(
        "ram",
        RamFn {
            memory_accessor: memory_accessor.clone(),
            word: false,
        },
    );
    ctx.register_fn(
        "ram16",
        RamFn {
            memory_accessor,
            word: true,
        },
    );
}
