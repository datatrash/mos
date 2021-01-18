#![allow(dead_code)]

use crate::errors::AsmResult;
use crate::parser::*;
use smallvec::{smallvec, SmallVec};
use std::collections::HashMap;

#[derive(Copy, Clone)]
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

    fn evaluate(&self, expr: &Token) -> Option<usize> {
        match expr {
            Token::Number(n) => Some(*n),
            Token::BinaryAdd(lhs, rhs)
            | Token::BinarySub(lhs, rhs)
            | Token::BinaryMul(lhs, rhs)
            | Token::BinaryDiv(lhs, rhs) => {
                let lhs = self.evaluate(lhs);
                let rhs = self.evaluate(rhs);
                match (expr, lhs, rhs) {
                    (Token::BinaryAdd(_, _), Some(lhs), Some(rhs)) => Some(lhs + rhs),
                    (Token::BinarySub(_, _), Some(lhs), Some(rhs)) => Some(lhs - rhs),
                    (Token::BinaryMul(_, _), Some(lhs), Some(rhs)) => Some(lhs * rhs),
                    (Token::BinaryDiv(_, _), Some(lhs), Some(rhs)) => Some(lhs / rhs),
                    _ => None,
                }
            }
            Token::Identifier(label_name) => self.labels.get(label_name).map(|pc| pc.0 as usize),
            Token::IndirectAddressing((inner, _)) => self.evaluate(inner),
            _ => panic!("Unsupported token: {:?}", expr),
        }
    }

    fn emit_instruction(
        &mut self,
        pc: Option<ProgramCounter>,
        i: &Instruction,
    ) -> Option<ProgramCounter> {
        dbg!(i);

        let imm = matches!(
            &*i.addressing_mode,
            Token::AddressingMode(AddressingMode::Immediate)
        );
        let possible_opcodes: Vec<(u8, usize)> = match (&i.mnemonic, imm, &i.operand.as_deref()) {
            (Mnemonic::Asl, false, Some(_)) => {
                vec![(0x06, 1), (0x0e, 2)]
            }
            (Mnemonic::Asl, false, None) => vec![(0x0a, 0)],
            (Mnemonic::Brk, _, _) => vec![(0x00, 0)],
            (Mnemonic::Inc, false, _) => vec![(0xee, 2)],
            (Mnemonic::Lda, true, _) => vec![(0xa9, 1)],
            (Mnemonic::Lda, false, _) => vec![(0xad, 2)],
            (Mnemonic::Jmp, false, _) => vec![(0x4c, 2)],
            (Mnemonic::Nop, _, _) => vec![(0xea, 0)],
            (Mnemonic::Ora, false, Some(Token::IndirectAddressing((_, Some(reg))))) => match reg {
                Register::X => vec![(0x01, 1)],
                Register::Y => vec![(0x11, 1)],
            },
            (Mnemonic::Ora, false, _) => {
                vec![(0x05, 1), (0x0d, 2)]
            }
            /*(Mnemonic::Ora, false, Some(Token::IndirectAddressing((_, Some(Register::X))))) => {
                vec![(0x15, 1), (0x1d, 2)]
            }*/
            (Mnemonic::Ora, true, _) => vec![(0x09, 1)],
            (Mnemonic::Php, _, _) => vec![(0x08, 0)],
            (Mnemonic::Rts, _, _) => vec![(0x60, 0)],
            (Mnemonic::Sta, false, _) => vec![(0x8d, 2)],
            _ => panic!("Unsupported mnemonic: {:?}", i.mnemonic),
        };

        // For all possible opcodes, pick the one that best matches the operand's size
        let (expression_is_valid, bytes): (bool, SmallVec<[u8; 3]>) = match &i.operand {
            Some(o) => {
                match self.evaluate(&*o) {
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
                        result.expect("Could not determine correct opcode")
                    }
                    None => {
                        // find maximum operand length and use that opcode
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
        let pc = pc.unwrap_or(segment.pc);
        segment.set(pc, &bytes);

        if expression_is_valid {
            // Done with this token
            None
        } else {
            // Will try to emit later on this pc
            Some(pc)
        }
    }

    fn emit_data(
        &mut self,
        pc: Option<ProgramCounter>,
        expr: &Token,
        data_length: usize,
    ) -> Option<ProgramCounter> {
        let segment = self.current_segment();
        let pc = pc.unwrap_or(segment.pc);

        match self.evaluate(expr) {
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
            None => Some(pc),
        }
    }
}

pub fn codegen<'a>(ast: Vec<Token>, options: CodegenOptions) -> AsmResult<CodegenContext<'a>> {
    let mut ctx = CodegenContext::new(options);

    let mut to_process: Vec<(Option<ProgramCounter>, Token)> = ast
        .into_iter()
        .map(|token| (None, token))
        .collect::<Vec<_>>();

    // Apply passes
    while !to_process.is_empty() {
        to_process = to_process
            .into_iter()
            .filter_map(|(pc, token)| match &token {
                Token::Label(id) => {
                    ctx.register_label(pc, id);
                    None
                }
                Token::Instruction(i) => ctx.emit_instruction(pc, i).map(|pc| (Some(pc), token)),
                Token::Data(Some(expr), data_length) => ctx
                    .emit_data(pc, expr, *data_length)
                    .map(|pc| (Some(pc), token)),
                _ => None,
            })
            .collect::<Vec<_>>();
    }

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use std::fmt::Display;

    #[test]
    fn basic() -> AsmResult<()> {
        let ctx = test_codegen("lda #123")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn expressions() -> AsmResult<()> {
        let ctx = test_codegen("lda #1 + 1\nlda #1 - 1\nlda #2 * 4\nlda #8 / 2")?;
        assert_eq!(
            ctx.current_segment().data,
            vec![0xa9, 2, 0xa9, 0, 0xa9, 8, 0xa9, 4]
        );
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels() -> AsmResult<()> {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(ctx.current_segment().data, vec![0x4c, 0x03, 0xc0, 0xea]);
        Ok(())
    }

    #[test]
    fn can_store_data() -> AsmResult<()> {
        let ctx = test_codegen(".byte 123\n.word 64738")?;
        assert_eq!(ctx.current_segment().data, vec![123, 0xe2, 0xfc]);
        Ok(())
    }

    #[test]
    fn can_perform_operations_on_labels() -> AsmResult<()> {
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
    fn test_all_non_branch_instructions() {
        let check = |code: &str, data: &[u8]| {
            let ctx = test_codegen(dbg!(code)).unwrap();
            assert_eq!(ctx.current_segment().data, data);
        };
        check("brk", &[0x00]);
        check("ora ($10,x)", &[0x01, 0x10]);
        check("ora $10", &[0x05, 0x10]);
        check("asl $10", &[0x06, 0x10]);
        check("php", &[0x08]);
        check("ora #$10", &[0x09, 0x10]);
        check("asl", &[0x0a]);
        check("ora $1234", &[0x0d, 0x34, 0x12]);
        check("asl $1234", &[0x0e, 0x34, 0x12]);
        check("nop", &[0xea]);
        check("ora ($10),y", &[0x11, 0x10]);
    }

    fn test_codegen<'a, S: Display + Into<String>>(code: S) -> AsmResult<CodegenContext<'a>> {
        let code = code.into();
        let (ast, errors) = parse(&code);
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
