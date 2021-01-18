#![allow(dead_code)]

use crate::errors::AsmResult;
use crate::parser::*;
use smallvec::{smallvec, SmallVec};
use std::collections::HashMap;

pub struct CodegenOptions {
    pc: usize,
}

pub struct Segment {
    data: Vec<u8>,
    start_pc: usize,
    pc: usize,
}

impl Segment {
    fn set(&mut self, pc: usize, bytes: &[u8]) -> usize {
        self.pc = pc;

        let length = bytes.len();
        let target = self.pc as usize - self.start_pc as usize;
        if self.data.len() < target + length {
            self.data.resize(target + length, 0);
        }
        for (offset, byte) in bytes.iter().enumerate() {
            self.data[target + offset] = *byte;
        }
        self.pc += length;
        self.pc
    }
}

pub struct CodegenContext<'a> {
    segments: HashMap<&'a str, Segment>,
    current_segment: &'a str,
    labels: HashMap<Identifier, usize>,
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

    fn register_label(&mut self, pc: Option<usize>, label: &Identifier) {
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
            Token::Identifier(label_name) => self.labels.get(label_name).copied(),
            _ => panic!("Unsupported token: {:?}", expr),
        }
    }

    fn evaluate_to_bytes(&self, expr: &Token, expected_bytes: usize) -> (bool, SmallVec<[u8; 2]>) {
        match (self.evaluate(expr), expected_bytes) {
            (Some(result), 1) => (true, smallvec![result as u8]),
            (Some(result), 2) => (true, SmallVec::from((result as u16).to_le_bytes())),
            (None, 1) => (false, smallvec![0]),
            (None, 2) => (false, smallvec![0, 0]),
            _ => panic!(),
        }
    }

    fn emit_instruction(&mut self, pc: Option<usize>, i: &Instruction) -> Option<usize> {
        let (opcode, expected_operand_bytes): (u8, usize) = match (&i.mnemonic, &*i.addressing_mode)
        {
            (Mnemonic::Lda, Token::AddressingMode(AddressingMode::Immediate)) => (0xa9, 1),
            (Mnemonic::Jmp, Token::AddressingMode(AddressingMode::Other)) => (0x4c, 2),
            (Mnemonic::Nop, _) => (0xea, 0),
            _ => panic!(),
        };

        let (expression_is_valid, operand_bytes): (bool, SmallVec<[u8; 2]>) =
            if expected_operand_bytes != 0 {
                self.evaluate_to_bytes(i.operand.as_ref().unwrap(), expected_operand_bytes)
            } else {
                (true, smallvec![])
            };

        let mut bytes: SmallVec<[u8; 3]> = smallvec![opcode];
        bytes.extend(operand_bytes);

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

    fn emit_data(&mut self, pc: Option<usize>, expr: &Token, data_length: usize) -> Option<usize> {
        let (expression_is_valid, bytes) = self.evaluate_to_bytes(expr, data_length);

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
}

pub fn codegen<'a>(ast: Vec<Token>, options: CodegenOptions) -> AsmResult<CodegenContext<'a>> {
    let mut ctx = CodegenContext::new(options);

    let mut to_process: Vec<(Option<usize>, Token)> = ast
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

    fn test_codegen<'a>(code: &'static str) -> AsmResult<CodegenContext<'a>> {
        let (ast, errors) = parse(code);
        if !errors.is_empty() {
            println!("{:?}", errors);
        }
        assert_eq!(errors.is_empty(), true);
        codegen(ast, CodegenOptions { pc: 0xc000 })
    }
}
