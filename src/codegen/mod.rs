#![allow(dead_code)]
use std::collections::HashMap;

use crate::{errors::AsmResult, parser::*};

pub struct CodegenOptions {
    pc: u16,
}

pub struct Segment {
    data: Vec<u8>,
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

    fn register_label(&mut self, label: &'a str) {
        self.labels.insert(label, 123);
    }

    fn generate_instruction_bytes(&self, i: &Instruction) -> Vec<u8> {
        match (&i.mnemonic.data, &i.addressing_mode.data) {
            (Mnemonic::Lda, am) => match am {
                AddressingMode::Immediate(val) => {
                    vec![0xa9, val.try_u8().unwrap()]
                }
                _ => vec![],
            },
            (Mnemonic::Sta, _am) => vec![],
            _ => vec![],
        }
    }

    fn emit(&mut self, i: &Instruction) {
        let bytes = self.generate_instruction_bytes(i);
        let len = bytes.len() as u16;
        let segment = self.segments.get_mut(self.current_segment).unwrap();
        segment.data.extend(bytes);
        segment.pc += len;
    }
}

pub fn codegen<'a>(ast: &[Token<'a>], options: CodegenOptions) -> AsmResult<CodegenContext<'a>> {
    let mut ctx = CodegenContext::new(options);

    ast.iter().for_each(|token| match token {
        Token::Label(label) => {
            ctx.register_label(label);
        }
        Token::Instruction(i) => ctx.emit(i),
    });

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> AsmResult<()> {
        let ast = parse("LDA #123")?.1;
        let ctx = codegen(&ast, CodegenOptions { pc: 0xc000 })?;
        let segment = ctx.segment("Default").unwrap();
        assert_eq!(segment.data, vec![0xa9, 123]);
        assert_eq!(segment.pc, 0xc002);
        Ok(())
    }
}
