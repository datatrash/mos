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

    fn register_label(&mut self, label: &'a str) {
        self.labels.insert(label, self.current_segment().pc);
    }

    /*fn resolve_label(&mut self, label: &'a str, target_pc: u16) -> Option<u16> {
        match self.labels.get(label) {
            Some(label_pc) => Some(label_pc.clone()),
            None => {
                let to_resolve = self.to_resolve.entry(label).or_insert_with(|| vec![]);
                to_resolve.push(ToResolve {
                    segment: self.current_segment,
                    pc: target_pc,
                });
                None
            }
        }
    }*/

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
    ) -> (bool, u8, SmallVec<[u8; 2]>) {
        match self.evaluate_expression(am.value()) {
            Some(val) => (true, opcode, val),
            None => (false, opcode, smallvec![0, 0]),
        }
    }

    fn try_evaluate_expression_u8(
        &self,
        am: &AddressingMode,
        opcode: u8,
    ) -> (bool, u8, SmallVec<[u8; 2]>) {
        match self.evaluate_expression(am.value()) {
            Some(val) => (true, opcode, smallvec![val[0]]),
            None => (false, opcode, smallvec![0]),
        }
    }

    fn generate_instruction_bytes(&mut self, i: &Instruction<'a>) -> (bool, u8, SmallVec<[u8; 2]>) {
        match (&i.mnemonic.data, &i.addressing_mode.data) {
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
            (Mnemonic::Nop, _am) => (true, 0xea, smallvec![]),
            //(Mnemonic::Sta, _am) => panic!(),
            _ => panic!(),
        }
    }

    fn emit(&mut self, i: Instruction<'a>) -> EmitResult<'a> {
        let (could_emit, opcode, operands) = self.generate_instruction_bytes(&i);
        let operands_len = operands.len() as u16;
        let segment = self.segments.get_mut(self.current_segment).unwrap();
        segment.data.push(opcode);
        segment.data.extend(operands);

        let result = if could_emit {
            EmitResult::Emitted
        } else {
            EmitResult::EmitLater(segment.pc, i)
        };

        segment.pc += 1 + operands_len;

        result
    }

    fn emit_at(&mut self, pc: u16, i: Instruction<'a>) -> EmitResult<'a> {
        let (could_emit, _, operands) = self.generate_instruction_bytes(&i);
        let segment = self.segments.get_mut(self.current_segment).unwrap();

        if could_emit {
            let mut offset = 1;
            for op in operands {
                segment.data[pc as usize - segment.start_pc as usize + offset] = op;
                offset = offset + 1;
            }

            EmitResult::Emitted
        } else {
            EmitResult::EmitLater(segment.pc, i)
        }
    }
}

enum EmitResult<'a> {
    Emitted,
    EmitLater(u16, Instruction<'a>),
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
            Token::Instruction(i) => match ctx.emit(i) {
                EmitResult::Emitted => None,
                EmitResult::EmitLater(pc, i) => Some((pc, i)),
            },
        })
        .collect::<HashMap<_, _>>();

    // Other passes
    while !unprocessed.is_empty() {
        unprocessed = unprocessed
            .into_iter()
            .filter_map(|(pc, i)| match ctx.emit_at(pc, i) {
                EmitResult::Emitted => None,
                EmitResult::EmitLater(pc, i) => Some((pc, i)),
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
        assert_eq!(ctx.current_segment().pc, 0xc002);
        Ok(())
    }

    #[test]
    fn most_basic_codegen_with_expression() -> AsmResult<()> {
        let ctx = test_codegen("lda #10 * 12")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 120]);
        assert_eq!(ctx.current_segment().pc, 0xc002);
        Ok(())
    }

    #[test]
    fn can_access_forward_declared_labels() -> AsmResult<()> {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(ctx.current_segment().data, vec![0x4c, 0x03, 0xc0, 0xea]);
        assert_eq!(ctx.current_segment().pc, 0xc004);
        Ok(())
    }

    /*#[test]
    fn can_perform_operations_on_labels() -> AsmResult<()> {
        // Create two labels, 'foo' and 'bar', separated by three nops.
        // 'foo' is a word label (so, 2 bytes), so 'bar - foo' should be 5 (2 bytes + 3 nops).
        let ctx = test_codegen("foo: .word bar - foo\nnop\nnop\nnop\nbar: nop")?;
        assert_eq!(
            ctx.current_segment().data,
            vec![0x05, 0x00, 0xea, 0xea, 0xea, 0xea]
        );
        assert_eq!(ctx.current_segment().pc, 0xc006);
        Ok(())
    }*/

    fn test_codegen<'a>(code: &'static str) -> AsmResult<CodegenContext<'a>> {
        let ast = parse(code.clone())?.1;
        codegen(ast, CodegenOptions { pc: 0xc000 })
    }
}
