use crate::codegen::{ProgramCounter, Symbol, SymbolData, SymbolIndex, SymbolTable};
use crate::parser::code_map::Span;
use crate::parser::{
    AddressModifier, BinaryOp, Expression, ExpressionFactor, ExpressionFactorFlags, IdentifierPath,
    InterpolatedString, InterpolatedStringItem, Located,
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
    ) -> EvaluationResult<Option<SymbolData>>;
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

impl BinaryOp {
    fn apply_i64(&self, lhs: i64, rhs: i64) -> i64 {
        match self {
            BinaryOp::Add => lhs + rhs,
            BinaryOp::Sub => lhs - rhs,
            BinaryOp::Mul => lhs * rhs,
            BinaryOp::Div => match rhs {
                0 => 0,
                _ => lhs / rhs,
            },
            BinaryOp::Mod => match rhs {
                0 => 0,
                _ => lhs % rhs,
            },
            BinaryOp::Shl => lhs << rhs,
            BinaryOp::Shr => lhs >> rhs,
            BinaryOp::Xor => lhs ^ rhs,
            BinaryOp::Eq => (lhs == rhs) as i64,
            BinaryOp::Ne => (lhs != rhs) as i64,
            BinaryOp::Gt => (lhs > rhs) as i64,
            BinaryOp::GtEq => (lhs >= rhs) as i64,
            BinaryOp::Lt => (lhs < rhs) as i64,
            BinaryOp::LtEq => (lhs <= rhs) as i64,
            BinaryOp::And => (lhs != 0 && rhs != 0) as i64,
            BinaryOp::Or => (lhs != 0 || rhs != 0) as i64,
        }
    }

    fn try_apply_str(&self, lhs: String, rhs: String) -> Option<SymbolData> {
        match self {
            BinaryOp::Add => Some((lhs + rhs.as_str()).into()),
            BinaryOp::Eq => Some((lhs == rhs).into()),
            BinaryOp::Ne => Some((lhs != rhs).into()),
            _ => None,
        }
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
        self.symbols
            .query(scope_nx, id)
            .and_then(|nx| self.symbols.try_get(nx).map(|s| (nx, s)))
    }

    pub fn get_symbol_filtered(
        &self,
        scope_nx: SymbolIndex,
        id: &IdentifierPath,
        filter: impl Fn(&Symbol) -> bool,
    ) -> Option<(SymbolIndex, &Symbol)> {
        log::trace!("Trying to get filtered symbol value: {}", id);
        self.symbols
            .query_all(scope_nx, id)
            .into_iter()
            .filter_map(|nx| self.symbols.try_get(nx).map(|s| (nx, s)))
            .find(|(_, s)| filter(s))
    }

    pub fn evaluate_expression(
        &self,
        expr: &Located<Expression>,
        track_usage: bool,
    ) -> EvaluationResult<Option<SymbolData>> {
        match &expr.data {
            Expression::Factor { factor, flags, .. } => {
                match self.evaluate_expression_factor(factor, track_usage)? {
                    Some(value) => match value {
                        SymbolData::Number(mut number) => {
                            if flags.contains(ExpressionFactorFlags::NOT) {
                                if number == 0 {
                                    number = 1
                                } else {
                                    number = 0
                                }
                            }
                            if flags.contains(ExpressionFactorFlags::NEG) {
                                number = -number;
                            }
                            Ok(Some(number.into()))
                        }
                        _ => Ok(Some(value)),
                    },
                    None => Ok(None),
                }
            }
            Expression::BinaryExpression(bin) => {
                let lhs = self.evaluate_expression(&bin.lhs, track_usage)?;
                let rhs = self.evaluate_expression(&bin.rhs, track_usage)?;
                match (lhs, rhs) {
                    (Some(SymbolData::Number(lhs)), Some(SymbolData::Number(rhs))) => {
                        Ok(Some(bin.op.data.apply_i64(lhs, rhs).into()))
                    }
                    (Some(SymbolData::String(lhs)), Some(SymbolData::String(rhs))) => {
                        match bin.op.data.try_apply_str(lhs, rhs) {
                            Some(result) => Ok(Some(result)),
                            None => Err(EvaluationError {
                                span: bin.op.span,
                                message: format!(
                                    "cannot apply operation '{}' on strings",
                                    bin.op.data
                                ),
                            }),
                        }
                    }
                    _ => Ok(None),
                }
            }
        }
    }

    fn evaluate_expression_factor(
        &self,
        factor: &Located<ExpressionFactor>,
        track_usage: bool,
    ) -> EvaluationResult<Option<SymbolData>> {
        match &factor.data {
            ExpressionFactor::CurrentProgramCounter(_) => {
                Ok(Some(self.pc.unwrap_or_default().as_i64().into()))
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
                let symbol_data = self.lookup_symbol(path, track_usage);

                Ok(symbol_data.and_then(|data| match data {
                    SymbolData::MacroDefinition(_) => None,
                    SymbolData::Number(val) => {
                        let result = match modifier.as_ref().map(|m| &m.data) {
                            Some(AddressModifier::LowByte) => val & 255,
                            Some(AddressModifier::HighByte) => (val >> 8) & 255,
                            _ => *val,
                        };
                        Some(SymbolData::Number(result))
                    }
                    SymbolData::String(q) => Some(SymbolData::String(q.clone())),
                    SymbolData::Placeholder => None,
                }))
            }
            ExpressionFactor::Number { value: number, .. } => Ok(Some(number.data.value().into())),
            ExpressionFactor::InterpolatedString(i) => {
                Ok(Some(SymbolData::String(self.interpolate(i, track_usage)?)))
            }
        }
    }

    pub fn lookup_symbol(
        &self,
        path: &Located<IdentifierPath>,
        track_usage: bool,
    ) -> Option<&SymbolData> {
        let tuple = self.get_symbol(self.current_scope_nx, &path.data);
        let symbol_index = tuple.as_ref().map(|(s, _)| *s);
        let symbol_data = tuple.as_ref().map(|(_, s)| &s.data);

        if track_usage {
            self.usages.lock().unwrap().push(SymbolUsage {
                symbol_index,
                path: path.clone(),
            });
        }

        symbol_data
    }

    pub fn interpolate(
        &self,
        i: &InterpolatedString,
        track_usage: bool,
    ) -> EvaluationResult<String> {
        let mut result = "".to_string();
        for item in &i.items {
            match item {
                InterpolatedStringItem::String(s) => result += s.data.as_str(),
                InterpolatedStringItem::IdentifierPath(path) => {
                    if let Some(data) = self.lookup_symbol(path, track_usage) {
                        if let Some(data) = data.try_as_string() {
                            result += data.as_str();
                        } else {
                            return Err(EvaluationError {
                                span: path.span,
                                message: format!("could not interpolate '{}' because '{}' does not resolve to a string", i, path)
                            });
                        }
                    }
                }
            }
        }
        Ok(result)
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
