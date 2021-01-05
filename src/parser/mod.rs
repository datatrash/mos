use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::map;
use nom::IResult;
use nom::multi::many0;
use nom::sequence::{preceded, tuple};
use nom_locate::LocatedSpan;
use parse_tools::*;

mod parse_tools;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
enum Mnemonic {
    ADC,
    BRK,
    LDA
}

#[derive(Debug, PartialEq)]
enum AddressingMode {
    IMMEDIATE(u8),
    IMPLIED
}

#[derive(Debug, PartialEq)]
struct Instruction {
    mnemonic: Mnemonic,
    addressing_mode: AddressingMode
}

fn mnemonic(data: Span) -> IResult<Span, Mnemonic> {
    alt((
        map( tag_no_case("adc"), |_| Mnemonic::ADC),
        map( tag_no_case("brk"), |_| Mnemonic::BRK),
        map( tag_no_case("lda"), |_| Mnemonic::LDA),
    ))(data)
}

fn immediate_byte(data: Span) -> IResult<Span, u8> {
    alt((
        preceded(tag("$"), hex_u8),
        dec_u8
    ))(data)
}

fn addressing_mode(data: Span) -> IResult<Span, AddressingMode> {
    alt((
        map( preceded(tag("#"), immediate_byte), |val: u8| AddressingMode::IMMEDIATE(val)),
        map( tag(""), |_| AddressingMode::IMPLIED)
    ))(data)
}

fn instruction(data: Span) -> IResult<Span, Instruction> {
    map(
        tuple((ws(mnemonic), ws(addressing_mode))),
        |(mnemonic, addressing_mode)| Instruction { mnemonic, addressing_mode }
    )(data)
}

fn assemble(data: Span) -> IResult<Span, Vec<Instruction>> {
    let mut parse = many0(instruction);
    parse(data)
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn can_assemble() {
        let asm = Span::new("LDA #$0c\nBRK\nADC #1\nCMP #2");
        let result = assemble(asm);
        println!("{:?}", result);
    }

    #[test]
    fn test_am_immediate() {
        instr_eq("lda #255", Instruction { mnemonic: Mnemonic::LDA, addressing_mode: AddressingMode::IMMEDIATE(255) });
    }

    fn instr_eq(src: &str, i: Instruction) {
        assert_eq!(assemble(Span::new(src)).unwrap().1, &[i])
    }
}
