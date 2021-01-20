#![allow(dead_code)]

use crate::errors::{MosError, MosResult};
use crate::parser::*;
use smallvec::{smallvec, SmallVec};
use std::collections::HashMap;

pub type CodegenResult<T> = Result<T, CodegenError>;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum CodegenError {
    #[error("unknown identifier: {1}")]
    UnknownIdentifier(Location, Identifier),
    #[error("branch too far")]
    BranchTooFar,
    #[error("unknown code generation error")]
    Unknown,
}

#[derive(Copy, Clone, Debug)]
pub struct ProgramCounter(u16);

impl ProgramCounter {
    pub fn new(pc: u16) -> Self {
        Self(pc)
    }

    pub fn to_le_bytes(&self) -> [u8; 2] {
        self.0.to_le_bytes()
    }
}

pub struct CodegenOptions {
    pub pc: ProgramCounter,
}

pub struct Segment {
    pub data: Vec<u8>,
    pub start_pc: ProgramCounter,
    pc: ProgramCounter,
}

impl Segment {
    fn set(&mut self, pc: ProgramCounter, bytes: &[u8]) -> ProgramCounter {
        self.pc = pc;

        let length = bytes.len();
        let target = (self.pc.0 - self.start_pc.0) as usize;
        if self.data.len() < target + length {
            self.data.resize(target + length, 0);
        }
        for (offset, byte) in bytes.iter().enumerate() {
            self.data[target + offset] = *byte;
        }
        self.pc.0 += length as u16;
        self.pc
    }
}

pub struct CodegenContext<'a> {
    segments: HashMap<&'a str, Segment>,
    current_segment: &'a str,
    labels: HashMap<Identifier, ProgramCounter>,
}

impl<'a> CodegenContext<'a> {
    fn new(options: CodegenOptions) -> Self {
        let mut segments = HashMap::new();
        let default_segment = Segment {
            data: vec![],
            start_pc: options.pc,
            pc: options.pc,
        };
        segments.insert("Default", default_segment);

        Self {
            segments,
            current_segment: "Default",
            labels: HashMap::new(),
        }
    }

    pub fn segment(&self, name: &str) -> Option<&Segment> {
        self.segments.get(name)
    }

    fn segment_mut(&mut self, name: &str) -> Option<&mut Segment> {
        self.segments.get_mut(name)
    }

    fn current_segment(&self) -> &Segment {
        self.segments.get(self.current_segment).unwrap()
    }

    fn current_segment_mut(&mut self) -> &mut Segment {
        self.segments.get_mut(self.current_segment).unwrap()
    }

    fn register_label(&mut self, pc: Option<ProgramCounter>, label: &Identifier) {
        let segment = self.current_segment_mut();
        let pc = pc.unwrap_or(segment.pc);
        self.labels.insert(label.clone(), pc);
    }

    fn evaluate(&self, lt: &LocatedToken, error_on_failure: bool) -> CodegenResult<Option<usize>> {
        match &lt.token {
            Token::Number(n, _) => Ok(Some(*n)),
            Token::BinaryAdd(lhs, rhs)
            | Token::BinarySub(lhs, rhs)
            | Token::BinaryMul(lhs, rhs)
            | Token::BinaryDiv(lhs, rhs) => {
                let lhs = self.evaluate(lhs, error_on_failure)?;
                let rhs = self.evaluate(rhs, error_on_failure)?;
                match (&lt.token, lhs, rhs) {
                    (Token::BinaryAdd(_, _), Some(lhs), Some(rhs)) => Ok(Some(lhs + rhs)),
                    (Token::BinarySub(_, _), Some(lhs), Some(rhs)) => Ok(Some(lhs - rhs)),
                    (Token::BinaryMul(_, _), Some(lhs), Some(rhs)) => Ok(Some(lhs * rhs)),
                    (Token::BinaryDiv(_, _), Some(lhs), Some(rhs)) => Ok(Some(lhs / rhs)),
                    _ => Ok(None),
                }
            }
            Token::Identifier(label_name) => {
                match (self.labels.get(label_name), error_on_failure) {
                    (Some(pc), _) => Ok(Some(pc.0 as usize)),
                    (None, false) => Ok(None),
                    (None, true) => Err(CodegenError::UnknownIdentifier(
                        lt.location.clone(),
                        label_name.clone(),
                    )),
                }
            }
            _ => panic!("Unsupported token: {:?}", lt.token),
        }
    }

    fn evaluate_operand<'b>(
        &self,
        pc: ProgramCounter,
        i: &'b Instruction,
        error_on_failure: bool,
    ) -> CodegenResult<(&'b AddressingMode, Option<usize>, Option<Register>)> {
        match i.operand.as_deref() {
            Some(LocatedToken {
                token: Token::Operand(operand),
                ..
            }) => {
                let register_suffix = operand.suffix.as_deref().map(|s| match s {
                    LocatedToken {
                        token: Token::RegisterSuffix(r),
                        ..
                    } => *r,
                    _ => panic!(),
                });

                let evaluated = self.evaluate(&*operand.expr, error_on_failure)?;
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
                            let cur_pc = (pc.0 + 2) as i64;
                            let mut offset = target_pc - cur_pc;
                            if offset >= -128 && offset <= 127 {
                                if offset < 0 {
                                    offset += 256;
                                }
                                let val = offset as usize;
                                Ok((&operand.addressing_mode, Some(val), register_suffix))
                            } else {
                                Err(CodegenError::BranchTooFar)
                            }
                        }
                        _ => Ok((&operand.addressing_mode, Some(val), register_suffix)),
                    })
                    .unwrap_or_else(|| Ok((&operand.addressing_mode, None, register_suffix)))
            }
            _ => Ok((&AddressingMode::Implied, None, None)),
        }
    }

    fn emit_instruction(
        &mut self,
        pc: Option<ProgramCounter>,
        i: &Instruction,
        error_on_failure: bool,
    ) -> CodegenResult<Option<ProgramCounter>> {
        let pc = pc.unwrap_or(self.current_segment().pc);

        type MM = Mnemonic;
        type AM = AddressingMode;
        use smallvec::smallvec as v;

        let (am, val, suffix) = self.evaluate_operand(pc, i, error_on_failure)?;
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
                        result.unwrap_or_else(|| panic!("Could not determine correct opcode, no matching opcodes for operand value: {}", val))
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

        let segment = self.current_segment_mut();
        segment.set(pc, &bytes);

        if expression_is_valid {
            // Done with this token
            Ok(None)
        } else {
            // Will try to emit later on this pc
            Ok(Some(pc))
        }
    }

    fn emit_data(
        &mut self,
        pc: Option<ProgramCounter>,
        lt: &LocatedToken,
        data_length: usize,
        error_on_failure: bool,
    ) -> CodegenResult<Option<ProgramCounter>> {
        let segment = self.current_segment();
        let pc = pc.unwrap_or(segment.pc);

        let evaluated = self.evaluate(lt, error_on_failure)?;
        let result = match evaluated {
            Some(val) => {
                let segment = self.current_segment_mut();
                match data_length {
                    1 => {
                        segment.set(pc, &[val as u8]);
                        None
                    }
                    2 => {
                        segment.set(pc, &((val as u16).to_le_bytes()));
                        None
                    }
                    4 => {
                        segment.set(pc, &((val as u32).to_le_bytes()));
                        None
                    }
                    _ => panic!(),
                }
            }
            None => {
                let segment = self.current_segment_mut();
                match data_length {
                    1 => {
                        segment.set(pc, &[0_u8]);
                        Some(pc)
                    }
                    2 => {
                        segment.set(pc, &(0_u16.to_le_bytes()));
                        Some(pc)
                    }
                    4 => {
                        segment.set(pc, &(0_u32.to_le_bytes()));
                        Some(pc)
                    }
                    _ => panic!(),
                }
            }
        };

        Ok(result)
    }

    fn emit_token(
        &mut self,
        pc: Option<ProgramCounter>,
        token: &Token,
        error_on_failure: bool,
    ) -> CodegenResult<Option<ProgramCounter>> {
        match token {
            Token::Label(id) => {
                self.register_label(pc, id);
                Ok(None)
            }
            Token::Instruction(i) => self.emit_instruction(pc, i, error_on_failure),
            Token::Data(Some(expr), data_length) => {
                self.emit_data(pc, expr, *data_length, error_on_failure)
            }
            _ => Ok(None),
        }
    }
}

pub fn codegen<'a>(
    ast: Vec<LocatedToken>,
    options: CodegenOptions,
) -> MosResult<CodegenContext<'a>> {
    let mut ctx = CodegenContext::new(options);

    let ast = ast
        .into_iter()
        .map(|t| t.strip_whitespace())
        .collect::<Vec<_>>();

    let mut to_process: Vec<(Option<ProgramCounter>, LocatedToken)> = ast
        .into_iter()
        .map(|token| (None, token))
        .collect::<Vec<_>>();

    // Apply passes
    let mut error_on_failure = false;
    while !to_process.is_empty() {
        let to_process_len = to_process.len();
        let mut next_to_process = vec![];
        for (pc, lt) in to_process {
            let token = &lt.token;
            let process_again_at_pc = match ctx.emit_token(pc, token, error_on_failure) {
                Ok(pc) => pc,
                Err(error) => {
                    let location = match &error {
                        CodegenError::UnknownIdentifier(location, _) => location.clone(),
                        _ => lt.location,
                    };
                    return Err(MosError::Codegen {
                        location,
                        message: format!("{}", error),
                    });
                }
            };

            if let Some(pc) = process_again_at_pc {
                next_to_process.push((Some(pc), lt));
            }
        }

        // If we haven't processed any tokens then the tokens that are left could not be resolved.
        if next_to_process.len() == to_process_len {
            // Emit an error on the next resolve failure
            error_on_failure = true;
        }
        to_process = next_to_process;
    }

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use std::fmt::Display;

    #[test]
    fn basic() -> MosResult<()> {
        let ctx = test_codegen("lda #123")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn basic_with_comments() -> MosResult<()> {
        let ctx = test_codegen("lda /*hello*/ #123")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn expressions() -> MosResult<()> {
        let ctx = test_codegen("lda #1 + 1\nlda #1 - 1\nlda #2 * 4\nlda #8 / 2")?;
        assert_eq!(
            ctx.current_segment().data,
            vec![0xa9, 2, 0xa9, 0, 0xa9, 8, 0xa9, 4]
        );
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels() -> MosResult<()> {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(ctx.current_segment().data, vec![0x4c, 0x03, 0xc0, 0xea]);
        Ok(())
    }

    #[test]
    fn accessing_unknown_labels_will_default_to_absolute_addressing() -> MosResult<()> {
        let ctx = test_codegen("lda my_label\nmy_label: nop")?;
        assert_eq!(ctx.current_segment().data, vec![0xad, 0x03, 0xc0, 0xea]);
        Ok(())
    }

    #[test]
    fn can_store_data() -> MosResult<()> {
        let ctx = test_codegen(".byte 123\n.word 64738")?;
        assert_eq!(ctx.current_segment().data, vec![123, 0xe2, 0xfc]);
        Ok(())
    }

    #[test]
    fn can_perform_operations_on_labels() -> MosResult<()> {
        // Create two labels, 'foo' and 'bar', separated by three NOPs.
        // 'foo' is a word label (so, 2 bytes), so 'bar - foo' should be 5 (2 bytes + 3 NOPs).
        let ctx = test_codegen("foo: .word bar - foo\nnop\nnop\nnop\nbar: nop")?;
        assert_eq!(
            ctx.current_segment().data,
            vec![0x05, 0x00, 0xea, 0xea, 0xea, 0xea]
        );
        Ok(())
    }

    #[test]
    fn can_perform_branch_calculations() -> MosResult<()> {
        let ctx = test_codegen("foo: nop\nbne foo")?;
        assert_eq!(ctx.current_segment().data, vec![0xea, 0xd0, 0xfd]);
        let ctx = test_codegen("bne foo\nfoo: nop")?;
        assert_eq!(ctx.current_segment().data, vec![0xd0, 0x00, 0xea]);
        Ok(())
    }

    #[test]
    fn cannot_perform_too_far_branch_calculations() -> MosResult<()> {
        let many_nops = std::iter::repeat("nop\n").take(140).collect::<String>();
        let (ast, _) = parse("test.asm", &format!("foo: {}bne foo", many_nops));
        let result = codegen(
            ast,
            CodegenOptions {
                pc: ProgramCounter::new(0xc000),
            },
        );
        assert_eq!(
            result.err(),
            Some(MosError::Codegen {
                location: Location {
                    path: "test.asm".to_string(),
                    line: 141,
                    column: 1
                },
                message: "branch too far".to_string()
            })
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

    fn code_eq(code: &str, data: &[u8]) {
        let ctx = test_codegen(code).unwrap();
        assert_eq!(ctx.current_segment().data, data);
    }

    fn test_codegen<'a, S: Display + Into<String>>(code: S) -> MosResult<CodegenContext<'a>> {
        let code = code.into();
        let (ast, errors) = parse("test.asm", &code);
        if !errors.is_empty() {
            println!("source:\n{}\n\nerrors:", code);
            println!("{:?}", errors);
        }
        assert_eq!(errors.is_empty(), true);
        codegen(
            ast,
            CodegenOptions {
                pc: ProgramCounter::new(0xc000),
            },
        )
    }
}
