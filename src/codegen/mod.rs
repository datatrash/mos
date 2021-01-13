#![allow(dead_code)]
use std::collections::HashMap;

use crate::{errors::AsmResult, parser::*};
use smallvec::smallvec;
use smallvec::SmallVec;

pub struct CodegenOptions {
    pc: u16,
}

pub struct Segment {
    data: Vec<u8>,
    start_pc: u16,
    pc: u16,
}

impl Segment {
    fn set(&mut self, pc: u16, bytes: &[u8]) -> u16 {
        self.pc = pc;

        let length = bytes.len();
        let target = self.pc as usize - self.start_pc as usize;
        if self.data.len() < target + length {
            self.data.resize(target + length, 0);
        }
        let mut offset = 0;
        for byte in bytes {
            self.data[target + offset] = *byte;
            offset += 1;
        }
        self.pc += length as u16;
        self.pc
    }
}

pub struct CodegenContext<'a> {
    segments: HashMap<&'a str, Segment>,
    current_segment: &'a str,
    labels: HashMap<&'a str, u16>,
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

    fn register_label(&mut self, label: &'a str) {
        self.labels.insert(label, self.current_segment().pc);
    }

    fn evaluate_expression(&self, expr: &Expression<'a>) -> Option<SmallVec<[u8; 2]>> {
        let (result, missing_labels) = expr.evaluate(&self.labels);
        match missing_labels.is_empty() {
            true => Some(match result {
                ResolvedExpression::U16(u16) => SmallVec::from_buf(u16.to_le_bytes()),
                ResolvedExpression::U8(u8) => smallvec![u8],
            }),
            false => None,
        }
    }

    fn try_evaluate_expression(
        &self,
        am: &AddressingMode,
        opcode: u8,
    ) -> (bool, SmallVec<[u8; 3]>) {
        match self.evaluate_expression(am.value()) {
            Some(val) => (true, smallvec![opcode, val[0], val[1]]),
            None => (false, smallvec![opcode, 0, 0]),
        }
    }

    fn try_evaluate_expression_u8(
        &self,
        am: &AddressingMode,
        opcode: u8,
    ) -> (bool, SmallVec<[u8; 3]>) {
        match self.evaluate_expression(am.value()) {
            Some(val) => (true, smallvec![opcode, val[0]]),
            None => (false, smallvec![opcode, 0]),
        }
    }

    fn generate_instruction_bytes(&mut self, i: &Instruction<'a>) -> (bool, SmallVec<[u8; 3]>) {
        match (&i.mnemonic, &i.addressing_mode) {
            (Mnemonic::Jmp, am) => {
                let opcode = match am {
                    AddressingMode::AbsoluteOrRelativeOrZp(_) => 0x4c,
                    _ => panic!(),
                };
                self.try_evaluate_expression(am, opcode)
            }
            (Mnemonic::Lda, am) => {
                let opcode = match am {
                    AddressingMode::Immediate(_) => 0xa9,
                    _ => panic!(),
                };
                self.try_evaluate_expression_u8(am, opcode)
            }
            (Mnemonic::Nop, _am) => (true, smallvec![0xea]),
            //(Mnemonic::Sta, _am) => panic!(),
            _ => panic!(),
        }
    }

    fn emit_instruction(&mut self, pc: Option<u16>, i: Instruction<'a>) -> Option<EmitResult<'a>> {
        let (could_emit, bytes) = self.generate_instruction_bytes(&i);
        let segment = self.current_segment_mut();
        let pc = pc.unwrap_or(segment.pc);
        segment.set(pc, &bytes);

        if could_emit {
            None
        } else {
            Some(EmitResult::EmitInstructionLater(pc, i))
        }
    }

    fn emit_data(
        &mut self,
        pc: Option<u16>,
        ty: DataType,
        expr: Expression<'a>,
    ) -> Option<EmitResult<'a>> {
        let (resolved, missing_labels) = expr.evaluate(&self.labels);
        let segment = self.current_segment_mut();
        let pc = pc.unwrap_or(segment.pc);

        let could_emit = missing_labels.is_empty();

        let bytes: SmallVec<[u8; 2]> = if could_emit {
            match ty {
                DataType::Byte => match resolved {
                    ResolvedExpression::U8(data) => smallvec![data],
                    ResolvedExpression::U16(data) => smallvec![data as u8],
                },
                DataType::Word => match resolved {
                    ResolvedExpression::U8(data) => SmallVec::from_buf((data as u16).to_le_bytes()),
                    ResolvedExpression::U16(data) => SmallVec::from_buf(data.to_le_bytes()),
                },
            }
        } else {
            // Just emit some placeholders
            match ty {
                DataType::Byte => smallvec![0],
                DataType::Word => smallvec![0, 0],
            }
        };

        segment.set(pc, &bytes);

        if could_emit {
            None
        } else {
            Some(EmitResult::EmitDataLater(pc, ty, expr))
        }
    }
}

enum EmitResult<'a> {
    EmitInstructionLater(u16, Instruction<'a>),
    EmitDataLater(u16, DataType, Expression<'a>),
}

pub fn codegen(ast: Vec<Token>, options: CodegenOptions) -> AsmResult<CodegenContext> {
    let mut ctx = CodegenContext::new(options);

    // First pass
    let mut unprocessed = ast
        .into_iter()
        .filter_map(|token| match token {
            Token::Label(label) => {
                ctx.register_label(label);
                None
            }
            Token::Instruction(i) => ctx.emit_instruction(None, i),
            Token::Data(ty, expr) => ctx.emit_data(None, ty, expr),
        })
        .collect::<Vec<_>>();

    // Other passes
    while !unprocessed.is_empty() {
        unprocessed = unprocessed
            .into_iter()
            .filter_map(|er| match er {
                EmitResult::EmitInstructionLater(pc, i) => ctx.emit_instruction(Some(pc), i),
                EmitResult::EmitDataLater(pc, ty, expr) => ctx.emit_data(Some(pc), ty, expr),
            })
            .collect();
    }

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn most_basic_codegen() -> AsmResult<()> {
        let ctx = test_codegen("lda #123")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 123]);
        Ok(())
    }

    #[test]
    fn most_basic_codegen_with_expression() -> AsmResult<()> {
        let ctx = test_codegen("lda #10 * 12")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 120]);
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
        let ast = parse(code)?.1;
        codegen(ast, CodegenOptions { pc: 0xc000 })
    }
}
