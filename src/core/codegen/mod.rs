#![allow(dead_code)]

use std::collections::HashMap;
use std::str::FromStr;

use itertools::Itertools;
use smallvec::{smallvec, SmallVec};

use crate::core::codegen::segment::{
    require_segment_options_fields, ProgramCounter, Segment, SegmentOptions,
};
use crate::core::codegen::symbol_table::{Symbol, SymbolTable};
use crate::errors::{MosError, MosResult};
use crate::parser::*;

mod segment;
mod symbol_table;

pub type CodegenResult<'a, T> = Result<T, CodegenError<'a>>;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum CodegenError<'a> {
    #[error("unknown identifier: {1}")]
    UnknownIdentifier(Location<'a>, IdentifierPath<'a>),
    #[error("branch too far")]
    BranchTooFar(Location<'a>),
    #[error("cannot redefine symbol: {1}")]
    SymbolRedefinition(Location<'a>, Identifier<'a>),
    #[error("operand size mismatch")]
    OperandSizeMismatch(Location<'a>),
    #[error("invalid definition: {1}: {2}")]
    InvalidDefinition(Location<'a>, Identifier<'a>, &'a str),
}

impl<'a> From<CodegenError<'a>> for MosError {
    fn from(error: CodegenError<'a>) -> Self {
        let location = match &error {
            CodegenError::UnknownIdentifier(location, _) => location,
            CodegenError::BranchTooFar(location) => location,
            CodegenError::SymbolRedefinition(location, _) => location,
            CodegenError::OperandSizeMismatch(location) => location,
            CodegenError::InvalidDefinition(location, _, _) => location,
        };
        MosError::Codegen {
            location: location.into(),
            message: format!("{}", error),
        }
    }
}

pub struct CodegenOptions {
    pub pc: ProgramCounter,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            pc: ProgramCounter::new(0xc000),
        }
    }
}

pub struct SegmentMap {
    segments: HashMap<String, Segment>,
    current: Option<String>,
}

impl SegmentMap {
    fn new() -> Self {
        Self {
            segments: HashMap::new(),
            current: None,
        }
    }

    fn insert<N: Into<String>>(&mut self, name: N, segment: Segment) {
        let name = name.into();
        self.segments.insert(name.clone(), segment);
        if self.current.is_none() {
            self.current = Some(name);
        }
    }

    fn try_get(&self, name: &str) -> Option<&Segment> {
        self.segments.get(name)
    }

    fn get(&self, name: &str) -> &Segment {
        self.segments
            .get(name)
            .unwrap_or_else(|| panic!("Segment not found: {}", name))
    }

    fn get_mut(&mut self, name: &str) -> &mut Segment {
        self.segments
            .get_mut(name)
            .unwrap_or_else(|| panic!("Segment not found: {}", name))
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn current(&self) -> &Segment {
        self.try_current().expect("No active segment")
    }

    pub(crate) fn try_current(&self) -> Option<&Segment> {
        self.current.as_ref().map(|n| self.get(n))
    }

    pub(crate) fn current_segment_name(&self) -> Option<String> {
        self.current.as_ref().cloned()
    }

    pub(crate) fn set_current(&mut self, name: Option<String>) {
        self.current = name;
    }

    pub fn current_mut(&mut self) -> &mut Segment {
        self.try_current_mut().expect("No active segment")
    }

    fn try_current_mut(&mut self) -> Option<&mut Segment> {
        match &self.current {
            Some(n) => self.segments.get_mut(n),
            None => None,
        }
    }
}

pub struct CodegenContext {
    options: CodegenOptions,
    segments: SegmentMap,
    symbols: SymbolTable,
    errors: Vec<MosError>,
}

#[derive(Debug)]
pub enum Emittable<'a> {
    Single(Option<ProgramCounter>, Location<'a>, Token<'a>),
    /// (Name of the scope, the emittables in the scope)
    Nested(String, Vec<Emittable<'a>>),
    SegmentDefinition(ConfigMap<'a>),
    Segment(String, Location<'a>, Vec<Emittable<'a>>),
    If(
        IfType,
        Located<'a, Expression<'a>>,
        Vec<Emittable<'a>>,
        Vec<Emittable<'a>>,
    ),
}

impl CodegenContext {
    fn new(options: CodegenOptions) -> Self {
        Self {
            options,
            segments: SegmentMap::new(),
            symbols: SymbolTable::new(),
            errors: vec![],
        }
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn segments(&self) -> &SegmentMap {
        &self.segments
    }

    fn evaluate_factor<'a>(
        &self,
        lt: &Located<'a, ExpressionFactor<'a>>,
        pc: Option<ProgramCounter>,
        error_on_failure: bool,
    ) -> CodegenResult<'a, Option<i64>> {
        match &lt.data {
            ExpressionFactor::Number(n, _) => Ok(Some(*n)),
            ExpressionFactor::CurrentProgramCounter => Ok(pc.map(|p| p.as_i64())),
            ExpressionFactor::IdentifierValue(path, modifier) => {
                let symbol_value = self.symbols.value(&path.to_str_vec());

                match (symbol_value, modifier, error_on_failure) {
                    (Some(val), None, _) => Ok(Some(val)),
                    (Some(val), Some(modifier), _) => match modifier {
                        AddressModifier::HighByte => Ok(Some((val >> 8) & 255)),
                        AddressModifier::LowByte => Ok(Some(val & 255)),
                    },
                    (None, _, false) => Ok(None),
                    (None, _, true) => Err(CodegenError::UnknownIdentifier(
                        lt.location.clone(),
                        path.clone(),
                    )),
                }
            }
            _ => panic!("Unsupported token: {:?}", lt.data),
        }
    }

    fn evaluate<'a>(
        &self,
        lt: &Located<'a, Expression<'a>>,
        pc: Option<ProgramCounter>,
        error_on_failure: bool,
    ) -> CodegenResult<'a, Option<i64>> {
        match &lt.data {
            Expression::Factor(factor, flags) => {
                match self.evaluate_factor(factor, pc, error_on_failure) {
                    Ok(Some(mut val)) => {
                        if flags.contains(ExpressionFactorFlags::NOT) {
                            val = !val;
                        }
                        if flags.contains(ExpressionFactorFlags::NEG) {
                            val = -val;
                        }
                        Ok(Some(val))
                    }
                    v => v,
                }
            }
            Expression::BinaryExpression(expr) => {
                let lhs = self.evaluate(&expr.lhs, pc, error_on_failure)?;
                let rhs = self.evaluate(&expr.rhs, pc, error_on_failure)?;
                let op = &expr.op;
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => {
                        let result: i64 = match op {
                            BinaryOp::Add => lhs + rhs,
                            BinaryOp::Sub => lhs - rhs,
                            BinaryOp::Mul => lhs * rhs,
                            BinaryOp::Div => lhs / rhs,
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
                        };
                        Ok(Some(result))
                    }
                    _ => Ok(None),
                }
            }
            _ => panic!("Unsupported token: {:?}", lt.data),
        }
    }

    fn evaluate_operand<'a, 'b>(
        &self,
        i: &'a Instruction<'b>,
        pc: Option<ProgramCounter>,
        location: &'a Location<'b>,
        error_on_failure: bool,
    ) -> CodegenResult<'b, (&'a AddressingMode, Option<i64>, Option<Register>)> {
        match i.operand.as_deref() {
            Some(Located {
                data: Token::Operand(operand),
                ..
            }) => {
                let register_suffix = operand.suffix.as_deref().map(|s| match s {
                    Located {
                        data: Token::RegisterSuffix(r),
                        ..
                    } => *r,
                    _ => panic!(),
                });

                let evaluated = self.evaluate(&*operand.expr, pc, error_on_failure)?;
                evaluated
                    .map(|val| match i.mnemonic {
                        Mnemonic::Bcc
                        | Mnemonic::Bcs
                        | Mnemonic::Beq
                        | Mnemonic::Bmi
                        | Mnemonic::Bne
                        | Mnemonic::Bpl
                        | Mnemonic::Bvc
                        | Mnemonic::Bvs => {
                            let target_pc = val as i64;
                            // If the current PC cannot be determined we'll just default to the target_pc. This will be fixed up later
                            // when the instruction is re-emitted.
                            let cur_pc =
                                (pc.unwrap_or_else(|| target_pc.into()).as_usize() + 2) as i64;
                            let mut offset = target_pc - cur_pc;
                            if offset >= -128 && offset <= 127 {
                                if offset < 0 {
                                    offset += 256;
                                }
                                let val = offset as i64;
                                Ok((&operand.addressing_mode, Some(val), register_suffix))
                            } else {
                                Err(CodegenError::BranchTooFar(location.clone()))
                            }
                        }
                        _ => Ok((&operand.addressing_mode, Some(val), register_suffix)),
                    })
                    .unwrap_or_else(|| Ok((&operand.addressing_mode, None, register_suffix)))
            }
            _ => Ok((&AddressingMode::Implied, None, None)),
        }
    }

    fn emit_instruction<'a, 'b>(
        &mut self,
        i: &'a Instruction<'b>,
        pc: Option<ProgramCounter>,
        location: &'a Location<'b>,
        error_on_failure: bool,
    ) -> CodegenResult<'b, (bool, Vec<u8>)> {
        type MM = Mnemonic;
        type AM = AddressingMode;
        use smallvec::smallvec as v;

        let (am, val, suffix) = self.evaluate_operand(i, pc, location, error_on_failure)?;
        let possible_opcodes: SmallVec<[(u8, usize); 2]> = match (&i.mnemonic, am, suffix) {
            (MM::Adc, AM::Immediate, None) => v![(0x69, 1)],
            (MM::Adc, AM::Indirect, Some(Register::X)) => v![(0x61, 1)],
            (MM::Adc, AM::OuterIndirect, Some(Register::Y)) => v![(0x71, 1)],
            (MM::Adc, AM::AbsoluteOrZP, None) => v![(0x65, 1), (0x6d, 2)],
            (MM::Adc, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x75, 1), (0x7d, 2)],
            (MM::Adc, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0x79, 2)],
            (MM::And, AM::AbsoluteOrZP, None) => v![(0x25, 1), (0x2d, 2)],
            (MM::And, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x35, 1), (0x3d, 2)],
            (MM::And, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0x39, 2)],
            (MM::And, AM::Immediate, None) => v![(0x29, 1)],
            (MM::And, AM::Indirect, Some(Register::X)) => v![(0x21, 1)],
            (MM::And, AM::OuterIndirect, Some(Register::Y)) => v![(0x31, 1)],
            (MM::Asl, AM::AbsoluteOrZP, None) => v![(0x06, 1), (0x0e, 2)],
            (MM::Asl, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x16, 1), (0x1e, 2)],
            (MM::Asl, AM::Implied, None) => v![(0x0a, 0)],
            (MM::Bcc, AM::AbsoluteOrZP, None) => v![(0x90, 1)],
            (MM::Bcs, AM::AbsoluteOrZP, None) => v![(0xb0, 1)],
            (MM::Bit, AM::AbsoluteOrZP, None) => v![(0x24, 1), (0x2c, 2)],
            (MM::Bmi, AM::AbsoluteOrZP, None) => v![(0x30, 1)],
            (MM::Bne, AM::AbsoluteOrZP, None) => v![(0xd0, 1)],
            (MM::Beq, AM::AbsoluteOrZP, None) => v![(0xf0, 1)],
            (MM::Brk, AM::Implied, None) => v![(0x00, 0)],
            (MM::Bpl, AM::AbsoluteOrZP, None) => v![(0x10, 1)],
            (MM::Bvc, AM::AbsoluteOrZP, None) => v![(0x50, 1)],
            (MM::Bvs, AM::AbsoluteOrZP, None) => v![(0x70, 1)],
            (MM::Clc, AM::Implied, None) => v![(0x18, 0)],
            (MM::Cld, AM::Implied, None) => v![(0xd8, 0)],
            (MM::Cli, AM::Implied, None) => v![(0x58, 0)],
            (MM::Clv, AM::Implied, None) => v![(0xb8, 0)],
            (MM::Cmp, AM::AbsoluteOrZP, None) => v![(0xc5, 1), (0xcd, 2)],
            (MM::Cmp, AM::AbsoluteOrZP, Some(Register::X)) => v![(0xd5, 1), (0xdd, 2)],
            (MM::Cmp, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0xd9, 2)],
            (MM::Cmp, AM::Immediate, None) => v![(0xc9, 1)],
            (MM::Cmp, AM::Indirect, Some(Register::X)) => v![(0xc1, 1)],
            (MM::Cmp, AM::OuterIndirect, Some(Register::Y)) => v![(0xd1, 1)],
            (MM::Cpx, AM::AbsoluteOrZP, None) => v![(0xe4, 1), (0xec, 2)],
            (MM::Cpx, AM::Immediate, None) => v![(0xe0, 1)],
            (MM::Cpy, AM::AbsoluteOrZP, None) => v![(0xc4, 1), (0xcc, 2)],
            (MM::Cpy, AM::Immediate, None) => v![(0xc0, 1)],
            (MM::Dec, AM::AbsoluteOrZP, None) => v![(0xc6, 1), (0xce, 2)],
            (MM::Dec, AM::AbsoluteOrZP, Some(Register::X)) => v![(0xd6, 1), (0xde, 2)],
            (MM::Dex, AM::Implied, None) => v![(0xca, 0)],
            (MM::Dey, AM::Implied, None) => v![(0x88, 0)],
            (MM::Eor, AM::AbsoluteOrZP, None) => v![(0x45, 1), (0x4d, 2)],
            (MM::Eor, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x55, 1), (0x5d, 2)],
            (MM::Eor, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0x59, 2)],
            (MM::Eor, AM::Immediate, None) => v![(0x49, 1)],
            (MM::Eor, AM::Indirect, Some(Register::X)) => v![(0x41, 1)],
            (MM::Eor, AM::OuterIndirect, Some(Register::Y)) => v![(0x51, 1)],
            (MM::Jmp, AM::AbsoluteOrZP, None) => v![(0x4c, 2)],
            (MM::Jmp, AM::OuterIndirect, None) => v![(0x6c, 2)],
            (MM::Jsr, AM::AbsoluteOrZP, None) => v![(0x20, 2)],
            (MM::Inc, AM::AbsoluteOrZP, None) => v![(0xe6, 1), (0xee, 2)],
            (MM::Inc, AM::AbsoluteOrZP, Some(Register::X)) => v![(0xf6, 1), (0xfe, 2)],
            (MM::Inx, AM::Implied, None) => v![(0xe8, 0)],
            (MM::Iny, AM::Implied, None) => v![(0xc8, 0)],
            (MM::Lda, AM::Immediate, None) => v![(0xa9, 1)],
            (MM::Lda, AM::AbsoluteOrZP, None) => v![(0xa5, 1), (0xad, 2)],
            (MM::Lda, AM::AbsoluteOrZP, Some(Register::X)) => v![(0xb5, 1), (0xbd, 2)],
            (MM::Lda, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0xb9, 2)],
            (MM::Lda, AM::Indirect, Some(Register::X)) => v![(0xa1, 1)],
            (MM::Lda, AM::OuterIndirect, Some(Register::Y)) => v![(0xb1, 1)],
            (MM::Ldx, AM::Immediate, None) => v![(0xa2, 1)],
            (MM::Ldx, AM::AbsoluteOrZP, None) => v![(0xa6, 1), (0xae, 2)],
            (MM::Ldx, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0xb6, 1), (0xbe, 2)],
            (MM::Ldy, AM::Immediate, None) => v![(0xa0, 1)],
            (MM::Ldy, AM::AbsoluteOrZP, None) => v![(0xa4, 1), (0xac, 2)],
            (MM::Ldy, AM::AbsoluteOrZP, Some(Register::X)) => v![(0xb4, 1), (0xbc, 2)],
            (MM::Lsr, AM::AbsoluteOrZP, None) => v![(0x46, 1), (0x4e, 2)],
            (MM::Lsr, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x56, 1), (0x5e, 2)],
            (MM::Lsr, AM::Implied, None) => v![(0x4a, 0)],
            (MM::Nop, AM::Implied, None) => v![(0xea, 0)],
            (MM::Ora, AM::Indirect, Some(Register::X)) => v![(0x01, 1)],
            (MM::Ora, AM::OuterIndirect, Some(Register::Y)) => v![(0x11, 1)],
            (MM::Ora, AM::AbsoluteOrZP, None) => v![(0x05, 1), (0x0d, 2)],
            (MM::Ora, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x15, 1), (0x1d, 2)],
            (MM::Ora, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0x19, 2)],
            (MM::Ora, AM::Immediate, None) => v![(0x09, 1)],
            (MM::Pha, AM::Implied, None) => v![(0x48, 0)],
            (MM::Php, AM::Implied, None) => v![(0x08, 0)],
            (MM::Pla, AM::Implied, None) => v![(0x68, 0)],
            (MM::Plp, AM::Implied, None) => v![(0x28, 0)],
            (MM::Rti, AM::Implied, None) => v![(0x40, 0)],
            (MM::Rol, AM::AbsoluteOrZP, None) => v![(0x26, 1), (0x2e, 2)],
            (MM::Rol, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x36, 1), (0x3e, 2)],
            (MM::Rol, AM::Implied, None) => v![(0x2a, 0)],
            (MM::Ror, AM::AbsoluteOrZP, None) => v![(0x66, 1), (0x6e, 2)],
            (MM::Ror, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x76, 1), (0x7e, 2)],
            (MM::Ror, AM::Implied, None) => v![(0x6a, 0)],
            (MM::Rts, AM::Implied, None) => v![(0x60, 0)],
            (MM::Sbc, AM::AbsoluteOrZP, None) => v![(0xe5, 1), (0xed, 2)],
            (MM::Sbc, AM::AbsoluteOrZP, Some(Register::X)) => v![(0xf5, 1), (0xfd, 2)],
            (MM::Sbc, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0xf9, 2)],
            (MM::Sbc, AM::Immediate, None) => v![(0xe9, 1)],
            (MM::Sbc, AM::Indirect, Some(Register::X)) => v![(0xe1, 1)],
            (MM::Sbc, AM::OuterIndirect, Some(Register::Y)) => v![(0xf1, 1)],
            (MM::Sec, AM::Implied, None) => v![(0x38, 0)],
            (MM::Sed, AM::Implied, None) => v![(0xf8, 0)],
            (MM::Sei, AM::Implied, None) => v![(0x78, 0)],
            (MM::Sta, AM::AbsoluteOrZP, None) => v![(0x85, 1), (0x8d, 2)],
            (MM::Sta, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x95, 1), (0x9d, 2)],
            (MM::Sta, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0x99, 2)],
            (MM::Sta, AM::Indirect, Some(Register::X)) => v![(0x81, 1)],
            (MM::Sta, AM::OuterIndirect, Some(Register::Y)) => v![(0x91, 1)],
            (MM::Stx, AM::AbsoluteOrZP, None) => v![(0x86, 1), (0x8e, 2)],
            (MM::Stx, AM::AbsoluteOrZP, Some(Register::Y)) => v![(0x96, 1)],
            (MM::Sty, AM::AbsoluteOrZP, None) => v![(0x84, 1), (0x8c, 2)],
            (MM::Sty, AM::AbsoluteOrZP, Some(Register::X)) => v![(0x94, 1)],
            (MM::Tax, AM::Implied, None) => v![(0xaa, 0)],
            (MM::Tay, AM::Implied, None) => v![(0xa8, 0)],
            (MM::Tsx, AM::Implied, None) => v![(0xba, 0)],
            (MM::Txa, AM::Implied, None) => v![(0x8a, 0)],
            (MM::Tya, AM::Implied, None) => v![(0x98, 0)],
            (MM::Txs, AM::Implied, None) => v![(0x9a, 0)],
            _ => panic!("Invalid instruction"),
        };

        // For all possible opcodes, pick the one that best matches the operand's size
        let (expression_is_valid, bytes): (bool, SmallVec<[u8; 3]>) = match &i.operand {
            Some(_) => {
                match val {
                    Some(val) => {
                        let mut result = None;
                        for (opcode, operand_length) in possible_opcodes {
                            if operand_length == 1 && val < 256 {
                                result = Some((true, smallvec![opcode, val as u8]));
                                break;
                            } else if operand_length == 2 {
                                let v = (val as u16).to_le_bytes();
                                result = Some((true, smallvec![opcode, v[0], v[1]]));
                                break;
                            }
                        }

                        match result {
                            Some(r) => r,
                            None => {
                                return Err(CodegenError::OperandSizeMismatch(location.clone()))
                            }
                        }
                    }
                    None => {
                        // Couldn't evaluate yet, so find maximum operand length and use that opcode
                        let (opcode, len) = possible_opcodes
                            .iter()
                            .max_by(|(_, len1), (_, len2)| len1.cmp(len2))
                            .unwrap();
                        match len {
                            1 => (false, smallvec![*opcode, 0]),
                            2 => (false, smallvec![*opcode, 0, 0]),
                            _ => panic!("Unknown operand length"),
                        }
                    }
                }
            }
            None => (true, smallvec![possible_opcodes[0].0]),
        };

        Ok((!expression_is_valid, bytes.to_vec()))
    }

    fn emit_data<'a>(
        &mut self,
        exprs: &[Located<'a, Expression<'a>>],
        data_length: usize,
        pc: Option<ProgramCounter>,
        error_on_failure: bool,
    ) -> CodegenResult<'a, (bool, Vec<u8>)> {
        // Did any of the emitted exprs fail to evaluate? Then re-evaluate all of them later.
        let mut any_failed = false;

        let mut result = vec![];
        for expr in exprs {
            let evaluated = self.evaluate(expr, pc, error_on_failure)?;
            let bytes: Vec<u8> = match evaluated {
                Some(val) => match data_length {
                    1 => vec![val as u8],
                    2 => (val as u16).to_le_bytes().to_vec(),
                    4 => (val as u32).to_le_bytes().to_vec(),
                    _ => panic!(),
                },
                None => {
                    any_failed = true;
                    match data_length {
                        1 => vec![0_u8],
                        2 => 0_u16.to_le_bytes().to_vec(),
                        4 => 0_u32.to_le_bytes().to_vec(),
                        _ => panic!(),
                    }
                }
            };
            result.extend(bytes);
        }

        Ok((any_failed, result))
    }

    fn emit_single<'a, 'b>(
        &mut self,
        token: &'a Token<'b>,
        location: &'a Location<'b>,
        pc: Option<ProgramCounter>,
        error_on_failure: bool,
    ) -> CodegenResult<'b, (bool, Vec<u8>)> {
        let (emit_later, bytes) = match token {
            Token::Label(id) => match pc {
                Some(pc) => self
                    .symbols
                    .register(id, Symbol::Label(pc), location, false)
                    .map(|_| (false, vec![])),
                None => Ok((true, vec![])),
            },
            Token::VariableDefinition(id, val, ty) => {
                let eval = self.evaluate(val, pc, error_on_failure)?.unwrap();

                let result = match ty {
                    VariableType::Variable => {
                        self.symbols
                            .register(id, Symbol::Variable(eval), location, true)
                    }
                    VariableType::Constant => {
                        self.symbols
                            .register(id, Symbol::Constant(eval), location, false)
                    }
                };

                result.map(|_| (false, vec![]))
            }
            Token::ProgramCounterDefinition(val) => {
                let eval = self.evaluate(val, pc, error_on_failure)?.unwrap();
                match self.segments.try_current_mut() {
                    Some(segment) => {
                        segment.set_current_pc(ProgramCounter::new(eval as u16));
                        Ok((false, vec![]))
                    }
                    None => Ok((true, vec![])),
                }
            }
            Token::Instruction(i) => self.emit_instruction(i, pc, location, error_on_failure),
            Token::Data(exprs, data_length) => {
                self.emit_data(exprs, *data_length, pc, error_on_failure)
            }
            _ => Ok((false, vec![])),
        }?;

        if let Some(pc) = pc {
            if !bytes.is_empty() {
                log::trace!(
                    "Emitting in segment {:?} at pc {}: {:?}",
                    self.segments.current,
                    pc,
                    &bytes
                );
                let segment = self.segments.current_mut();
                segment.set_current_pc(pc);
                segment.set(&bytes);
            }
        }

        // We need to handle this emittable again later if the emittable has indicated this by itself (emit_later) or if there was no
        // active segment we could emit to (pc.is_none()).
        Ok((emit_later || pc.is_none(), bytes))
    }

    fn evaluate_or_error(
        &mut self,
        identifier: &str,
        cfg: &ConfigMap,
        pc: Option<ProgramCounter>,
        error_on_failure: bool,
        error_msg: &str,
    ) -> Option<i64> {
        let expr = cfg.value(identifier).map(|val| val.as_expression().clone());
        match self.evaluate(&expr, pc, error_on_failure) {
            Ok(Some(val)) => Some(val),
            _ => {
                if error_on_failure {
                    let err = CodegenError::InvalidDefinition(
                        expr.location.clone(),
                        Identifier(identifier),
                        error_msg,
                    )
                    .into();
                    self.errors.push(err);
                }
                None
            }
        }
    }

    fn emit_emittables<'a>(
        &mut self,
        emittables: Vec<Emittable<'a>>,
        error_on_failure: bool,
    ) -> Vec<Emittable<'a>> {
        let result = emittables
            .into_iter()
            .filter_map(|emittable| {
                log::trace!("Processing emittable: {:?}", emittable);
                let pc = self.segments.try_current().map(|seg| seg.current_pc());
                match emittable {
                    Emittable::SegmentDefinition(cfg) => {
                        let start = self.evaluate_or_error(
                            "start",
                            &cfg,
                            pc,
                            error_on_failure,
                            "Could not determine start address for segment",
                        );

                        match start {
                            Some(start) => {
                                let name = cfg.value_as_identifier_path("name").single().0;
                                let write = match cfg.try_value_as_identifier_path("write") {
                                    Some(val) => bool::from_str(val.single().0).unwrap_or(true),
                                    None => true,
                                };
                                let options = SegmentOptions {
                                    initial_pc: start.into(),
                                    write,
                                };
                                let segment = Segment::new(options);
                                self.segments.insert(name, segment);
                                None
                            }
                            None => {
                                // try again later
                                Some(Emittable::SegmentDefinition(cfg))
                            }
                        }
                    }
                    Emittable::Single(provided_pc, location, token) => {
                        let pc = match provided_pc {
                            Some(pc) => Some(pc),
                            None => pc,
                        };
                        match self.emit_single(&token, &location, pc, error_on_failure) {
                            Ok((emit_later, _bytes)) => {
                                if emit_later {
                                    Some(Emittable::Single(pc, location, token))
                                } else {
                                    None
                                }
                            }
                            Err(e) => {
                                self.errors.push(e.into());
                                None
                            }
                        }
                    }
                    Emittable::Nested(scope_name, inner) => {
                        self.symbols.enter(&scope_name);
                        let inner = self.emit_emittables(inner, error_on_failure);
                        self.symbols.leave();
                        match inner.is_empty() {
                            true => None,
                            false => Some(Emittable::Nested(scope_name, inner)),
                        }
                    }
                    Emittable::Segment(segment_name, location, inner) => {
                        let prev_segment = self.segments.current_segment_name();
                        match self.segments.try_get(&segment_name) {
                            Some(_) => {
                                self.segments.set_current(Some(segment_name.clone()));
                                match inner.is_empty() {
                                    true => {
                                        // This segment is set without an inner scope
                                        None
                                    }
                                    false => {
                                        // We have an inner scope, so reset the old segment afterwards
                                        let inner = self.emit_emittables(inner, error_on_failure);
                                        self.segments.set_current(prev_segment);
                                        match inner.is_empty() {
                                            true => None,
                                            false => Some(Emittable::Segment(
                                                segment_name,
                                                location,
                                                inner,
                                            )),
                                        }
                                    }
                                }
                            }
                            None => {
                                if error_on_failure {
                                    self.errors.push(
                                        CodegenError::UnknownIdentifier(
                                            location.clone(),
                                            IdentifierPath::new(&[Identifier(&segment_name)]),
                                        )
                                        .into(),
                                    );
                                    None
                                } else {
                                    Some(Emittable::Segment(segment_name, location, inner))
                                }
                            }
                        }
                    }
                    Emittable::If(ty, expr, if_, else_) => {
                        let expr_result = match ty {
                            IfType::IfExpr => self
                                .evaluate(&expr, pc, error_on_failure)
                                .map(|r| r.map(|r| r > 0)),
                            IfType::IfDef(positive) => {
                                let exists = self.evaluate(&expr, pc, false);
                                match (exists, positive) {
                                    (Ok(Some(_)), true) => Ok(Some(true)),
                                    (Ok(None), true) => Ok(Some(false)),
                                    (Ok(Some(_)), false) => Ok(Some(false)),
                                    (Ok(None), false) => Ok(Some(true)),
                                    (Err(e), _) => Err(e),
                                }
                            }
                        };
                        match expr_result {
                            Ok(Some(res)) => {
                                log::trace!("result of ifdef: {}", res);
                                if res {
                                    let inner = self.emit_emittables(if_, error_on_failure);
                                    match inner.is_empty() {
                                        true => None,
                                        false => Some(Emittable::If(ty, expr, inner, vec![])),
                                    }
                                } else {
                                    let inner = self.emit_emittables(else_, error_on_failure);
                                    match inner.is_empty() {
                                        true => None,
                                        false => Some(Emittable::If(ty, expr, vec![], inner)),
                                    }
                                }
                            }
                            _ => {
                                log::trace!("result of ifdef undetermined");
                                Some(Emittable::If(ty, expr, if_, else_))
                            }
                        }
                    }
                }
            })
            .collect();

        for r in &result {
            log::trace!("Result: {:?}", r);
        }

        result
    }

    fn require_cfg_value<'a, T>(
        &mut self,
        key: &str,
        cfg_location: &Location,
        value: MosResult<Option<Located<'a, T>>>,
    ) -> Option<Located<'a, T>> {
        match value {
            Ok(Some(val)) => Some(val),
            Ok(None) => {
                self.errors.push(
                    CodegenError::InvalidDefinition(
                        cfg_location.clone(),
                        Identifier(key),
                        "missing value",
                    )
                    .into(),
                );
                None
            }
            Err(e) => {
                self.errors.push(e);
                None
            }
        }
    }

    fn generate_emittables<'a>(&mut self, ast: Vec<Located<'a, Token<'a>>>) -> Vec<Emittable<'a>> {
        let mut active_label = None;
        ast.into_iter()
            .filter_map(|lt| match lt.data {
                Token::Definition(id, cfg) => {
                    let id = id.data.as_identifier().0;
                    let cfg = cfg.expect("Found empty definition");
                    let cfg_location = cfg.location;
                    let cfg = cfg.data.into_config_map();

                    active_label = None;
                    match id {
                        "segment" => {
                            // Perform some sanity checks
                            let errors = require_segment_options_fields(&cfg, &cfg_location);
                            if errors.is_empty() {
                                Some(Emittable::SegmentDefinition(cfg))
                            } else {
                                self.errors.extend(errors);
                                None
                            }
                        }
                        _ => panic!("Unknown definition type: {}", id),
                    }
                }
                Token::Segment(id, braces) => {
                    let segment_name = id.data.as_identifier().0;
                    let inner = braces
                        .map(|b| b.data.into_braces())
                        .unwrap_or_else(Vec::new);
                    Some(Emittable::Segment(
                        segment_name.into(),
                        id.location.clone(),
                        self.generate_emittables(inner),
                    ))
                }
                Token::Braces(inner) => {
                    let scope_name = self.symbols.add_child_scope(active_label);
                    self.symbols.enter(&scope_name);
                    let e = Emittable::Nested(scope_name, self.generate_emittables(inner));
                    self.symbols.leave();

                    active_label = None;
                    Some(e)
                }
                Token::If(ty, expr, if_, else_) => {
                    let if_ = self.generate_emittables(vec![*if_]);
                    let else_ = else_
                        .map(|e| self.generate_emittables(vec![*e]))
                        .unwrap_or_else(Vec::new);
                    Some(Emittable::If(ty, expr, if_, else_))
                }
                _ => {
                    // When a label is found we set it as an active label. If it is immediately followed by braces the label name will be
                    // used to set the name of the braces' scope. If it is not followed by braces we unset the active label to make sure
                    // any nested scope doesn't accidentally get the labels' name.
                    match &lt.data {
                        Token::Label(id) => active_label = Some(id.0),
                        _ => active_label = None,
                    };
                    Some(Emittable::Single(None, lt.location, lt.data))
                }
            })
            .collect_vec()
    }
}

pub fn codegen<'a>(
    ast: Vec<Located<'a, Token<'a>>>,
    options: CodegenOptions,
) -> MosResult<CodegenContext> {
    let mut ctx = CodegenContext::new(options);

    let ast = ast.into_iter().map(|t| t.strip_whitespace()).collect_vec();

    let mut to_process = ctx.generate_emittables(ast);

    // Apply passes

    // On the first pass, any error is not a failure since we may encounter unresolved labels and such
    // After the first pass, all labels should be present so any error will be a failure then
    let mut error_on_failure = false;
    let mut num_passes = 0;
    while !to_process.is_empty() && num_passes < 50 {
        log::trace!("==== PASS ================");
        let to_process_len = to_process.len();
        let next_to_process = ctx.emit_emittables(to_process, error_on_failure);

        // If we haven't processed any tokens then the tokens that are left could not be resolved.
        if next_to_process.len() == to_process_len {
            // Is it because there are no segments yet? Then create a default one.
            if ctx.segments.is_empty() {
                log::trace!("Creating default segment");
                let options = SegmentOptions {
                    initial_pc: ctx.options.pc,
                    ..Default::default()
                };
                ctx.segments.insert("default", Segment::new(options));
            } else {
                // Emit an error on the next resolve failure
                error_on_failure = true;
            }
        }
        to_process = next_to_process;
        num_passes += 1;
    }
    if num_passes == 50 {
        panic!("Endless loop");
    }

    if ctx.errors.is_empty() {
        Ok(ctx)
    } else {
        Err(ctx.errors.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::enable_default_tracing;
    use crate::parser::parse;

    use super::*;

    type TestResult = MosResult<()>;

    #[test]
    fn basic() -> TestResult {
        let ctx = test_codegen("lda #123\nlda #$40\nlda #%10010")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xa9, 123, 0xa9, 64, 0xa9, 18]
        );
        Ok(())
    }

    #[test]
    fn basic_with_comments() -> TestResult {
        let ctx = test_codegen("lda /*hello*/ #123")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn basic_with_braces() -> TestResult {
        let ctx = test_codegen("{ lda #123 }")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn can_detect_operand_size_mismatch() -> TestResult {
        let err = test_codegen("lda (foo,x)\nfoo: nop").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:1: error: operand size mismatch"
        );
        Ok(())
    }

    #[test]
    fn expressions() -> TestResult {
        let ctx = test_codegen(
            r"
            lda #1 + 1
            lda #1 - 1
            lda #2 * 4
            lda #8 / 2
            lda #1 + 5 * 4 + 3
            lda #7 == 7
            lda #7 != 7
            lda #7 == 8
            lda #7 != 8
            lda #1 << 2
            lda #4 >> 1
            lda #0 ^ 255
            lda #2 > 2
            lda #2 >= 2
            lda #2 < 2
            lda #2 <= 2
            ",
        )?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![
                0xa9, 2, 0xa9, 0, 0xa9, 8, 0xa9, 4, 0xa9, 24, 0xa9, 1, 0xa9, 0, 0xa9, 0, 0xa9, 1,
                0xa9, 4, 0xa9, 2, 0xa9, 255, 0xa9, 0, 0xa9, 1, 0xa9, 0, 0xa9, 1
            ]
        );
        Ok(())
    }

    #[test]
    fn can_use_variables() -> TestResult {
        let ctx = test_codegen(".var foo=49152\nlda #>foo")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xa9, 0xc0]);
        Ok(())
    }

    #[test]
    fn can_redefine_variables() -> TestResult {
        let ctx = test_codegen(".var foo=49152\n.var foo=foo + 5\nlda #<foo")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xa9, 0x05]);
        Ok(())
    }

    #[test]
    fn can_use_scopes() -> TestResult {
        let ctx = test_codegen(
            r"
                .var a = 1
                {
                    .var a = 2
                    lda #a
                    lda #super.a
                }
                lda #a
            ",
        )?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xa9, 2, 0xa9, 1, 0xa9, 1]
        );
        Ok(())
    }

    #[test]
    fn can_use_named_scopes() -> TestResult {
        let ctx = test_codegen(
            r"
                foo: {
                    .var a = 1
                    nop
                    bar: { .var a = 2 }
                }
                lda #foo.a
                lda #foo.bar.a
            ",
        )?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xea, 0xa9, 1, 0xa9, 2]
        );
        Ok(())
    }

    #[test]
    fn can_define_segments() -> TestResult {
        let ctx =
            test_codegen(".define segment {\nname = foo\nstart = 49152\nwrite = false\n}\nnop")?;
        assert_eq!(ctx.segments().get("foo").range(), &Some(0xc000..0xc001));
        assert_eq!(ctx.segments().get("foo").range_data(), vec![0xea]);
        assert_eq!(ctx.segments().get("foo").options().write, false);
        assert_eq!(ctx.segments().try_get("default").is_none(), true);
        Ok(())
    }

    #[test]
    fn can_use_segments() -> TestResult {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                nop
                .segment b
                rol
                .segment a
                asl
                ",
        )?;
        assert_eq!(ctx.segments().get("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.segments().get("b").range_data(), vec![0x2a]);
        Ok(())
    }

    #[test]
    fn can_use_scoped_segments() -> TestResult {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                nop
                .segment b { rol }
                asl
                ",
        )?;
        assert_eq!(ctx.segments().get("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.segments().get("b").range_data(), vec![0x2a]);
        Ok(())
    }

    #[test]
    fn can_use_forward_references_to_other_segments() -> TestResult {
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = $2000 }
                lda foo
                .segment b { foo: lda bar }
                bar: nop
                ",
        )?;
        assert_eq!(
            ctx.segments().get("a").range_data(),
            vec![0xad, 0x00, 0x20, 0xea]
        );
        assert_eq!(ctx.segments().get("b").range_data(), vec![0xad, 0x03, 0x10]);
        Ok(())
    }

    #[test]
    fn cannot_use_unknown_segments() -> TestResult {
        let err = test_codegen(".segment foo").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:10: error: unknown identifier: foo"
        );
        Ok(())
    }

    #[test]
    fn segments_have_required_fields() -> TestResult {
        let err = test_codegen(".define segment {}").err().unwrap();
        assert_eq!(
            err.to_string()
                .contains("test.asm:1:17: error: required field: name"),
            true
        );
        assert_eq!(
            err.to_string()
                .contains("test.asm:1:17: error: required field: start"),
            true
        );
        Ok(())
    }

    /*#[test]
    fn segments_can_depend_on_each_other() -> TestResult {
        enable_default_tracing();
        let ctx = test_codegen(
            r"
                .define segment { name = a start = $1000 }
                .define segment { name = b start = segments.a.end }
                nop
                .segment b { rol }
                asl
                ",
        )?;
        assert_eq!(ctx.segments().get("a").range_data(), vec![0xea, 0x0a]);
        assert_eq!(ctx.segments().get("b").range_data(), vec![0x2a]);
        assert_eq!(ctx.segments().get("a").range(), &Some(0x1000..0x1002));
        assert_eq!(ctx.segments().get("b").range(), &Some(0x1002..0x1003));
        Ok(())
    }*/

    #[test]
    fn can_use_constants() -> TestResult {
        let ctx = test_codegen(".const foo=49152\nlda #>foo")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xa9, 0xc0]);
        Ok(())
    }

    #[test]
    fn cannot_redefine_constants() -> TestResult {
        let err = test_codegen(".const foo=49152\n.const foo=foo + 5")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:1: error: cannot redefine symbol: foo"
        );
        Ok(())
    }

    #[test]
    fn cannot_redefine_variable_types() -> TestResult {
        let err = test_codegen(".const foo=49152\nfoo: nop").err().unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:2:1: error: cannot redefine symbol: foo"
        );
        Ok(())
    }

    #[test]
    fn if_else() -> TestResult {
        let ctx = test_codegen(
            ".const foo=1\n.if foo { nop } else { rol }\n.if foo > 10 { nop } else { asl }",
        )?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xea, 0x0a]);
        Ok(())
    }

    #[test]
    fn ifdef() -> TestResult {
        let ctx = test_codegen(".const foo=1\n.ifdef foo { nop }\n.ifdef bar { asl }")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xea]);
        Ok(())
    }

    #[test]
    fn ifndef() -> TestResult {
        let ctx = test_codegen(".const foo=1\n.ifndef foo { nop }\n.ifndef bar { asl }")?;
        assert_eq!(ctx.segments().current().range_data(), vec![0x0a]);
        Ok(())
    }

    #[test]
    fn if_or_and() -> TestResult {
        let ctx = test_codegen(
            r"
            .const foo=1
            .const bar=0
            .if foo || bar { nop }
            .if foo && bar { asl }
            .if foo && !bar { rol }
            ",
        )?;
        assert_eq!(ctx.segments().current().range_data(), vec![0xea, 0x2a]);
        Ok(())
    }

    #[test]
    fn unary_negative_and_not() -> TestResult {
        let ctx = test_codegen(".var foo = 1\nlda #-foo\nlda #!foo")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xa9, 0xff, 0xa9, 0xfe]
        );
        Ok(())
    }

    #[test]
    fn can_access_current_pc() -> TestResult {
        let ctx = test_codegen("lda * + 3\nlda *")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xad, 0x03, 0xc0, 0xad, 0x03, 0xc0]
        );
        Ok(())
    }

    #[test]
    fn can_set_current_pc() -> TestResult {
        let ctx = test_codegen("* = $1234\nnop")?;
        assert_eq!(
            ctx.segments().current().range(),
            &Some(0x1234u16..0x1235u16)
        );
        assert_eq!(ctx.segments().current().range_data(), vec![0xea]);
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels() -> TestResult {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0x4c, 0x03, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels_within_scope() -> TestResult {
        let ctx = test_codegen("{ jmp my_label\nmy_label: nop }")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0x4c, 0x03, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn cannot_access_forward_declared_labels_within_nested_scope() -> TestResult {
        let err = test_codegen("jmp my_label\n{ my_label: nop }")
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "test.asm:1:5: error: unknown identifier: my_label"
        );
        Ok(())
    }

    #[test]
    fn accessing_forwarded_labels_will_default_to_absolute_addressing() -> TestResult {
        let ctx = test_codegen("lda my_label\nmy_label: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xad, 0x03, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_modify_addresses() -> TestResult {
        let ctx = test_codegen("lda #<my_label\nlda #>my_label\nmy_label: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xa9, 0x04, 0xa9, 0xc0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn error_unknown_identifier() {
        let err = test_codegen(".byte foo\n.byte foo2").err().unwrap();
        assert_eq!(format!("{}", err), "test.asm:1:7: error: unknown identifier: foo\ntest.asm:2:7: error: unknown identifier: foo2");
    }

    #[test]
    fn can_store_data() -> TestResult {
        let ctx = test_codegen(".byte 123\n.word 123\n.word $fce2")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![123, 123, 0, 0xe2, 0xfc]
        );
        Ok(())
    }

    #[test]
    fn can_store_current_pc_as_data() -> TestResult {
        let ctx = test_codegen(".word *\n.word foo - *\nfoo: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0x00, 0xc0, 0x02, 0x00, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_store_csv_data() -> TestResult {
        let ctx = test_codegen(".word 123, foo, 234\nfoo: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![123, 0, 0x06, 0xc0, 234, 0, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_perform_operations_on_labels() -> TestResult {
        // Create two labels, 'foo' and 'bar', separated by three NOPs.
        // 'foo' is a word label (so, 2 bytes), so 'bar - foo' should be 5 (2 bytes + 3 NOPs).
        let ctx = test_codegen("foo: .word bar - foo\nnop\nnop\nnop\nbar: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0x05, 0x00, 0xea, 0xea, 0xea, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_perform_branch_calculations() -> TestResult {
        let ctx = test_codegen("foo: nop\nbne foo")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xea, 0xd0, 0xfd]
        );
        let ctx = test_codegen("bne foo\nfoo: nop")?;
        assert_eq!(
            ctx.segments().current().range_data(),
            vec![0xd0, 0x00, 0xea]
        );
        Ok(())
    }

    #[test]
    fn cannot_perform_too_far_branch_calculations() -> TestResult {
        let many_nops = std::iter::repeat("nop\n").take(140).collect::<String>();
        let src = format!("foo: {}bne foo", many_nops);
        let ast = parse("test.asm", &src)?;
        let result = codegen(ast, CodegenOptions::default());
        assert_eq!(
            format!("{}", result.err().unwrap()),
            "test.asm:141:1: error: branch too far"
        );
        Ok(())
    }

    #[test]
    fn test_all_non_branch_instructions() {
        code_eq("brk", &[0x00]);
        code_eq("ora ($10,x)", &[0x01, 0x10]);
        code_eq("ora $10", &[0x05, 0x10]);
        code_eq("asl $10", &[0x06, 0x10]);
        code_eq("php", &[0x08]);
        code_eq("ora #$10", &[0x09, 0x10]);
        code_eq("asl", &[0x0a]);
        code_eq("ora $1234", &[0x0d, 0x34, 0x12]);
        code_eq("asl $1234", &[0x0e, 0x34, 0x12]);
        code_eq("ora ($10),y", &[0x11, 0x10]);
        code_eq("ora $10,x", &[0x15, 0x10]);
        code_eq("asl $10,x", &[0x16, 0x10]);
        code_eq("clc", &[0x18]);
        code_eq("ora $1234,y", &[0x19, 0x34, 0x12]);
        code_eq("ora $1234,x", &[0x1d, 0x34, 0x12]);
        code_eq("asl $1234,x", &[0x1e, 0x34, 0x12]);
        code_eq("jsr $1234", &[0x20, 0x34, 0x12]);
        code_eq("and ($10,x)", &[0x21, 0x10]);
        code_eq("bit $10", &[0x24, 0x10]);
        code_eq("and $10", &[0x25, 0x10]);
        code_eq("rol $10", &[0x26, 0x10]);
        code_eq("plp", &[0x28]);
        code_eq("and #$10", &[0x29, 0x10]);
        code_eq("rol", &[0x2a]);
        code_eq("bit $1234", &[0x2c, 0x34, 0x12]);
        code_eq("and $1234", &[0x2d, 0x34, 0x12]);
        code_eq("rol $1234", &[0x2e, 0x34, 0x12]);
        code_eq("and ($10),y", &[0x31, 0x10]);
        code_eq("and $10,x", &[0x35, 0x10]);
        code_eq("rol $10,x", &[0x36, 0x10]);
        code_eq("sec", &[0x38]);
        code_eq("and $1234,y", &[0x39, 0x34, 0x12]);
        code_eq("and $1234,x", &[0x3d, 0x34, 0x12]);
        code_eq("rol $1234,x", &[0x3e, 0x34, 0x12]);
        code_eq("rti", &[0x40]);
        code_eq("eor ($10,x)", &[0x41, 0x10]);
        code_eq("eor $10", &[0x45, 0x10]);
        code_eq("lsr $10", &[0x46, 0x10]);
        code_eq("pha", &[0x48]);
        code_eq("eor #$10", &[0x49, 0x10]);
        code_eq("lsr", &[0x4a]);
        code_eq("jmp $1234", &[0x4c, 0x34, 0x12]);
        code_eq("eor $1234", &[0x4d, 0x34, 0x12]);
        code_eq("lsr $1234", &[0x4e, 0x34, 0x12]);
        code_eq("eor ($10),y", &[0x51, 0x10]);
        code_eq("eor $10,x", &[0x55, 0x10]);
        code_eq("lsr $10,x", &[0x56, 0x10]);
        code_eq("cli", &[0x58]);
        code_eq("eor $1234,y", &[0x59, 0x34, 0x12]);
        code_eq("eor $1234,x", &[0x5d, 0x34, 0x12]);
        code_eq("lsr $1234,x", &[0x5e, 0x34, 0x12]);
        code_eq("adc ($10,x)", &[0x61, 0x10]);
        code_eq("adc $10", &[0x65, 0x10]);
        code_eq("ror $10", &[0x66, 0x10]);
        code_eq("pla", &[0x68]);
        code_eq("adc #$10", &[0x69, 0x10]);
        code_eq("ror", &[0x6a]);
        code_eq("jmp ($1234)", &[0x6c, 0x34, 0x12]);
        code_eq("adc $1234", &[0x6d, 0x34, 0x12]);
        code_eq("ror $1234", &[0x6e, 0x34, 0x12]);
        code_eq("adc ($10),y", &[0x71, 0x10]);
        code_eq("adc $10,x", &[0x75, 0x10]);
        code_eq("ror $10,x", &[0x76, 0x10]);
        code_eq("sei", &[0x78]);
        code_eq("adc $1234,y", &[0x79, 0x34, 0x12]);
        code_eq("adc $1234,x", &[0x7d, 0x34, 0x12]);
        code_eq("ror $1234,x", &[0x7e, 0x34, 0x12]);
        code_eq("sta ($10,x)", &[0x81, 0x10]);
        code_eq("sty $10", &[0x84, 0x10]);
        code_eq("sta $10", &[0x85, 0x10]);
        code_eq("stx $10", &[0x86, 0x10]);
        code_eq("dey", &[0x88]);
        code_eq("txa", &[0x8a]);
        code_eq("sty $1234", &[0x8c, 0x34, 0x12]);
        code_eq("sta $1234", &[0x8d, 0x34, 0x12]);
        code_eq("stx $1234", &[0x8e, 0x34, 0x12]);
        code_eq("sta ($10),y", &[0x91, 0x10]);
        code_eq("sty $10,x", &[0x94, 0x10]);
        code_eq("sta $10,x", &[0x95, 0x10]);
        code_eq("stx $10,y", &[0x96, 0x10]);
        code_eq("tya", &[0x98]);
        code_eq("sta $1234,y", &[0x99, 0x34, 0x12]);
        code_eq("txs", &[0x9a]);
        code_eq("sta $1234,x", &[0x9d, 0x34, 0x12]);
        code_eq("ldy #$10", &[0xa0, 0x10]);
        code_eq("lda ($10,x)", &[0xa1, 0x10]);
        code_eq("ldx #$10", &[0xa2, 0x10]);
        code_eq("ldy $10", &[0xa4, 0x10]);
        code_eq("lda $10", &[0xa5, 0x10]);
        code_eq("ldx $10", &[0xa6, 0x10]);
        code_eq("tay", &[0xa8]);
        code_eq("lda #$10", &[0xa9, 0x10]);
        code_eq("tax", &[0xaa]);
        code_eq("ldy $1234", &[0xac, 0x34, 0x12]);
        code_eq("lda $1234", &[0xad, 0x34, 0x12]);
        code_eq("ldx $1234", &[0xae, 0x34, 0x12]);
        code_eq("lda ($10),y", &[0xb1, 0x10]);
        code_eq("ldy $10,x", &[0xb4, 0x10]);
        code_eq("lda $10,x", &[0xb5, 0x10]);
        code_eq("ldx $10,y", &[0xb6, 0x10]);
        code_eq("clv", &[0xb8]);
        code_eq("lda $1234,y", &[0xb9, 0x34, 0x12]);
        code_eq("tsx", &[0xba]);
        code_eq("ldy $1234,x", &[0xbc, 0x34, 0x12]);
        code_eq("lda $1234,x", &[0xbd, 0x34, 0x12]);
        code_eq("ldx $1234,y", &[0xbe, 0x34, 0x12]);
        code_eq("cpy #$10", &[0xc0, 0x10]);
        code_eq("cmp ($10,x)", &[0xc1, 0x10]);
        code_eq("cpy $10", &[0xc4, 0x10]);
        code_eq("cmp $10", &[0xc5, 0x10]);
        code_eq("dec $10", &[0xc6, 0x10]);
        code_eq("iny", &[0xc8]);
        code_eq("cmp #$10", &[0xc9, 0x10]);
        code_eq("dex", &[0xca]);
        code_eq("cpy $1234", &[0xcc, 0x34, 0x12]);
        code_eq("cmp $1234", &[0xcd, 0x34, 0x12]);
        code_eq("dec $1234", &[0xce, 0x34, 0x12]);
        code_eq("cmp ($10),y", &[0xd1, 0x10]);
        code_eq("cmp $10,x", &[0xd5, 0x10]);
        code_eq("dec $10,x", &[0xd6, 0x10]);
        code_eq("cld", &[0xd8]);
        code_eq("cmp $1234,y", &[0xd9, 0x34, 0x12]);
        code_eq("cmp $1234,x", &[0xdd, 0x34, 0x12]);
        code_eq("dec $1234,x", &[0xde, 0x34, 0x12]);
        code_eq("cpx #$10", &[0xe0, 0x10]);
        code_eq("sbc ($10,x)", &[0xe1, 0x10]);
        code_eq("cpx $10", &[0xe4, 0x10]);
        code_eq("sbc $10", &[0xe5, 0x10]);
        code_eq("inc $10", &[0xe6, 0x10]);
        code_eq("inx", &[0xe8]);
        code_eq("sbc #$10", &[0xe9, 0x10]);
        code_eq("nop", &[0xea]);
        code_eq("cpx $1234", &[0xec, 0x34, 0x12]);
        code_eq("sbc $1234", &[0xed, 0x34, 0x12]);
        code_eq("inc $1234", &[0xee, 0x34, 0x12]);
        code_eq("sbc ($10),y", &[0xf1, 0x10]);
        code_eq("sbc $10,x", &[0xf5, 0x10]);
        code_eq("inc $10,x", &[0xf6, 0x10]);
        code_eq("sed", &[0xf8]);
        code_eq("sbc $1234,y", &[0xf9, 0x34, 0x12]);
        code_eq("sbc $1234,x", &[0xfd, 0x34, 0x12]);
        code_eq("inc $1234,x", &[0xfe, 0x34, 0x12]);
    }

    #[test]
    fn test_all_branch_instructions() {
        code_eq("bpl foo\nfoo: nop", &[0x10, 0x00, 0xea]);
        code_eq("bmi foo\nfoo: nop", &[0x30, 0x00, 0xea]);
        code_eq("bvc foo\nfoo: nop", &[0x50, 0x00, 0xea]);
        code_eq("bvs foo\nfoo: nop", &[0x70, 0x00, 0xea]);
        code_eq("bcc foo\nfoo: nop", &[0x90, 0x00, 0xea]);
        code_eq("bcs foo\nfoo: nop", &[0xb0, 0x00, 0xea]);
        code_eq("bne foo\nfoo: nop", &[0xd0, 0x00, 0xea]);
        code_eq("beq foo\nfoo: nop", &[0xf0, 0x00, 0xea]);
    }

    fn code_eq(code: &'static str, data: &[u8]) {
        let ctx = test_codegen(code).unwrap();
        assert_eq!(ctx.segments().current().range_data(), data);
    }

    fn test_codegen(code: &'static str) -> MosResult<CodegenContext> {
        let ast = parse("test.asm", &code)?;
        codegen(ast, CodegenOptions::default())
    }
}
