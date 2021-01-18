#![allow(dead_code)]

use crate::errors::AsmResult;
use crate::parser::*;
use smallvec::smallvec;
use smallvec::SmallVec;
use std::collections::HashMap;

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
        for (offset, byte) in bytes.iter().enumerate() {
            self.data[target + offset] = *byte;
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

    fn evaluate(&self, expr: &Token) -> (bool, SmallVec<[u8; 2]>) {
        match expr {
            Token::Number(n) => (true, smallvec![*n as u8]),
            _ => (false, smallvec![]),
        }
    }

    fn emit_instruction(&mut self, pc: Option<u16>, i: &Instruction) -> Option<u16> {
        let opcode: u8 = match &i.mnemonic {
            Mnemonic::Lda => match *i.addressing_mode {
                Token::AddressingMode(AddressingMode::Immediate) => 0xa9,
                _ => panic!(),
            },
            _ => panic!(),
        };

        let (expression_is_valid, operand) = match &i.operand {
            Some(o) => self.evaluate(o),
            None => (true, smallvec![]),
        };

        let mut bytes: SmallVec<[u8; 3]> = smallvec![opcode];
        bytes.extend(operand);

        let segment = self.current_segment_mut();
        let pc = pc.unwrap_or(segment.pc);
        segment.set(pc, &bytes);

        if expression_is_valid {
            None
        } else {
            // Will try to emit later on this pc
            Some(pc)
        }
    }
}

pub fn codegen<'a>(ast: Vec<Token>, options: CodegenOptions) -> AsmResult<CodegenContext<'a>> {
    let mut ctx = CodegenContext::new(options);

    let mut to_process: Vec<(Option<u16>, Token)> = ast
        .into_iter()
        .map(|token| (None, token))
        .collect::<Vec<_>>();

    // Apply passes
    while !to_process.is_empty() {
        to_process = to_process
            .into_iter()
            .filter_map(|(pc, token)| match &token {
                Token::Instruction(i) => ctx.emit_instruction(pc, i).map(|pc| (Some(pc), token)),
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
    fn most_basic_codegen() -> AsmResult<()> {
        let ctx = test_codegen("lda #123")?;
        assert_eq!(ctx.current_segment().data, vec![0xa9, 123]);
        Ok(())
    }

    fn test_codegen<'a>(code: &'static str) -> AsmResult<CodegenContext<'a>> {
        let (ast, errors) = parse(code);
        assert_eq!(errors.is_empty(), true);
        codegen(ast, CodegenOptions { pc: 0xc000 })
    }
}
