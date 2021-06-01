use crate::codegen::{ProgramCounter, Symbol, SymbolIndex, SymbolTable};
use crate::parser::code_map::Span;
use crate::parser::{
    AddressModifier, Expression, ExpressionFactor, ExpressionFactorFlags, IdentifierPath, Located,
};
use itertools::Itertools;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub type EvaluationResult<T> = Result<T, EvaluationError>;

#[derive(Debug)]
pub struct EvaluationError {
    pub span: Span,
    pub message: String,
}

pub type FunctionMap = HashMap<String, Arc<Mutex<dyn FunctionCallback + Send + Sync>>>;

pub trait FunctionCallback {
    fn expected_args(&self) -> usize;
    fn apply(
        &mut self,
        ctx: &Evaluator,
        args: &[&Located<Expression>],
    ) -> EvaluationResult<Option<i64>>;
}

pub struct Evaluator<'a> {
    current_scope_nx: SymbolIndex,
    symbols: &'a SymbolTable<Symbol>,
    functions: &'a FunctionMap,
    pc: Option<ProgramCounter>,
    usages: Arc<Mutex<Vec<SymbolUsage>>>,
}

#[derive(Debug)]
pub struct SymbolUsage {
    pub symbol_index: Option<SymbolIndex>,
    pub path: Located<IdentifierPath>,
}

pub struct SymbolSnapshot {
    pub current_scope_nx: SymbolIndex,
    pub symbols: SymbolTable<Symbol>,
    pub pc: ProgramCounter,
}

impl SymbolSnapshot {
    pub fn get_evaluator<'a>(&'a self, functions: &'a FunctionMap) -> Evaluator<'a> {
        Evaluator::new(
            self.current_scope_nx,
            &self.symbols,
            functions,
            Some(self.pc),
        )
    }
}

impl<'a> Evaluator<'a> {
    pub fn new(
        current_scope_nx: SymbolIndex,
        symbols: &'a SymbolTable<Symbol>,
        functions: &'a FunctionMap,
        pc: Option<ProgramCounter>,
    ) -> Evaluator<'a> {
        Self {
            current_scope_nx,
            symbols,
            functions,
            pc,
            usages: Arc::new(Mutex::new(vec![])),
        }
    }

    pub fn usages(self) -> Vec<SymbolUsage> {
        let u = Arc::try_unwrap(self.usages).unwrap();
        Mutex::into_inner(u).unwrap()
    }

    pub fn snapshot(&self) -> SymbolSnapshot {
        let symbols = self.symbols.clone();
        SymbolSnapshot {
            symbols,
            current_scope_nx: self.current_scope_nx,
            pc: self.pc.unwrap(),
        }
    }

    pub fn get_symbol(
        &self,
        scope_nx: SymbolIndex,
        id: &IdentifierPath,
    ) -> Option<(SymbolIndex, &Symbol)> {
        log::trace!("Trying to get symbol value: {}", id);
        if let Some(nx) = self.symbols.query(scope_nx, id) {
            return self.symbols.try_get(nx).map(|s| (nx, s));
        }

        None
    }

    pub fn evaluate_expression(
        &self,
        expr: &Located<Expression>,
        track_usage: bool,
    ) -> EvaluationResult<Option<i64>> {
        match &expr.data {
            Expression::Factor { factor, flags, .. } => {
                match self.evaluate_expression_factor(factor, track_usage)? {
                    Some(mut value) => {
                        if flags.contains(ExpressionFactorFlags::NOT) {
                            if value == 0 {
                                value = 1
                            } else {
                                value = 0
                            }
                        }
                        if flags.contains(ExpressionFactorFlags::NEG) {
                            value = -value;
                        }
                        Ok(Some(value))
                    }
                    None => Ok(None),
                }
            }
            Expression::BinaryExpression(bin) => {
                let lhs = self.evaluate_expression(&bin.lhs, track_usage)?;
                let rhs = self.evaluate_expression(&bin.rhs, track_usage)?;
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => Ok(Some(bin.op.data.apply(lhs, rhs))),
                    _ => Ok(None),
                }
            }
        }
    }

    fn evaluate_expression_factor(
        &self,
        factor: &Located<ExpressionFactor>,
        track_usage: bool,
    ) -> EvaluationResult<Option<i64>> {
        match &factor.data {
            ExpressionFactor::CurrentProgramCounter(_) => {
                Ok(Some(self.pc.unwrap_or_default().as_i64()))
            }
            ExpressionFactor::ExprParens { inner, .. } => {
                self.evaluate_expression(inner, track_usage)
            }
            ExpressionFactor::FunctionCall { name, args, .. } => {
                match self.functions.get(name.data.as_str()) {
                    Some(callback) => {
                        let mut callback = callback.lock().unwrap();
                        self.expect_args(name.span, args.len(), callback.expected_args())?;
                        callback.apply(self, &args.iter().map(|(expr, _)| expr).collect_vec())
                    }
                    None => self.error(name.span, format!("unknown function: {}", &name.data)),
                }
            }
            ExpressionFactor::IdentifierValue { path, modifier } => {
                let tuple = self.get_symbol(self.current_scope_nx, &path.data);
                let symbol_index = tuple.as_ref().map(|(s, _)| *s);
                let symbol_data = tuple.as_ref().map(|(_, s)| &s.data);

                if track_usage {
                    self.usages.lock().unwrap().push(SymbolUsage {
                        symbol_index,
                        path: path.clone(),
                    });
                }

                Ok(symbol_data.map(|d| d.as_i64()).map(|val| {
                    match modifier.as_ref().map(|m| &m.data) {
                        Some(AddressModifier::LowByte) => val & 255,
                        Some(AddressModifier::HighByte) => (val >> 8) & 255,
                        _ => val,
                    }
                }))
            }
            ExpressionFactor::Number { value: number, .. } => Ok(Some(number.data.value())),
        }
    }

    pub fn expect_args(&self, span: Span, actual: usize, expected: usize) -> EvaluationResult<()> {
        if actual != expected {
            self.error(
                span,
                format!("expected {} arguments, got {}", expected, actual),
            )
        } else {
            Ok(())
        }
    }

    fn error<T, M: Into<String>>(&self, span: Span, message: M) -> EvaluationResult<T> {
        Err(EvaluationError {
            span,
            message: message.into(),
        })
    }
}
