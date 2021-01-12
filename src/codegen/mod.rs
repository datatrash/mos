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

pub struct ToResolve<'a> {
    segment: &'a str,
    pc: u16,
}

pub struct CodegenContext<'a> {
    segments: HashMap<&'a str, Segment>,
    current_segment: &'a str,
    labels: HashMap<&'a str, u16>,
    to_resolve: HashMap<&'a str, Vec<ToResolve<'a>>>,
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
            to_resolve: HashMap::new(),
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

    fn resolve_label(&mut self, label: &'a str, target_pc: u16) -> Option<u16> {
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
    }

    fn resolve_expression(&mut self, expr: &Expression<'a>) -> SmallVec<[u8; 2]> {
        match expr {
            Expression::Label(label) => SmallVec::from_buf(
                self.resolve_label(label, self.current_segment().pc + 1)
                    .unwrap_or(0)
                    .to_le_bytes(),
            ),
            Expression::U16(u16) => SmallVec::from_buf(u16.to_le_bytes()),
            Expression::U8(u8) => smallvec![*u8],
            _ => panic!(),
        }
    }

    fn generate_instruction_bytes(&mut self, i: Instruction<'a>) -> (u8, SmallVec<[u8; 2]>) {
        match (i.mnemonic.data, i.addressing_mode.data) {
            (Mnemonic::Jmp, am) => {
                let opcode = match am {
                    AddressingMode::AbsoluteOrRelativeOrZp(_) => 0x4c,
                    _ => panic!(),
                };
                (opcode, self.resolve_expression(am.value()))
            }
            (Mnemonic::Lda, am) => {
                let opcode = match am {
                    AddressingMode::Immediate(_) => 0xa9,
                    _ => panic!(),
                };
                (opcode, self.resolve_expression(am.value()))
            }
            (Mnemonic::Nop, _am) => (0xea, smallvec![]),
            (Mnemonic::Sta, _am) => panic!(),
            _ => panic!(),
        }
    }

    fn emit(&mut self, i: Instruction<'a>) {
        let (opcode, operands) = self.generate_instruction_bytes(i);
        let operands_len = operands.len() as u16;
        let segment = self.segments.get_mut(self.current_segment).unwrap();
        segment.data.push(opcode);
        segment.data.extend(operands);
        segment.pc += 1 + operands_len;
    }

    fn resolve_all(&mut self) {
        let to_resolve = std::mem::replace(&mut self.to_resolve, HashMap::new());

        self.to_resolve = to_resolve
            .into_iter()
            .filter_map(|(label, to_resolves)| {
                let label_pc = self.labels.get(label).cloned();
                match label_pc {
                    Some(label_pc) => {
                        for to_resolve in to_resolves {
                            let segment = self.segment_mut(to_resolve.segment).unwrap();
                            let offset = (to_resolve.pc - segment.start_pc) as usize;
                            let le = label_pc.to_le_bytes();
                            segment.data[offset] = le[0];
                            segment.data[offset + 1] = le[1];
                        }

                        None
                    }
                    None => Some((label, to_resolves)), // label does not exist yet, so keep it around
                }
            })
            .collect();
    }
}

pub fn codegen(ast: Vec<Token>, options: CodegenOptions) -> AsmResult<CodegenContext> {
    let mut ctx = CodegenContext::new(options);

    // First pass
    for token in ast {
        match token {
            Token::Label(label) => {
                ctx.register_label(label);
            }
            Token::Instruction(i) => ctx.emit(i),
        }
    }

    // Resolve passes
    while !ctx.to_resolve.is_empty() {
        ctx.resolve_all();
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
    fn can_access_forward_declared_labels() -> AsmResult<()> {
        let ctx = test_codegen("jmp my_label\nmy_label: nop")?;
        assert_eq!(ctx.current_segment().data, vec![0x4c, 0x03, 0xc0, 0xea]);
        assert_eq!(ctx.current_segment().pc, 0xc004);
        Ok(())
    }

    fn test_codegen<'a>(code: &'static str) -> AsmResult<CodegenContext<'a>> {
        let ast = parse(code.clone())?.1;
        codegen(ast, CodegenOptions { pc: 0xc000 })
    }
}
