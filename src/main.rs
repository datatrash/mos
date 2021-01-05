use nom::branch::alt;
use nom::bytes::complete::{tag_no_case};
use nom::IResult;
use nom::multi::many0;
use nom::combinator::{map};
use nom::sequence::{tuple, delimited};
use nom::error::ParseError;
use nom::character::complete::multispace0;

fn main() {}

#[derive(Debug)]
enum Mnemonic {
    ADC,
    LDA
}

#[derive(Debug)]
enum AddressingMode {
    IMPLIED
}

#[derive(Debug)]
struct Instruction {
    mnemonic: Mnemonic,
    addressing_mode: AddressingMode
}

fn mnemonic(data: &str) -> IResult<&str, Mnemonic> {
    alt((
        map( tag_no_case("adc"), |_| Mnemonic::ADC),
        map( tag_no_case("lda"), |_| Mnemonic::LDA),
    ))(data)
}

fn addressing_mode(data: &str) -> IResult<&str, AddressingMode> {
    map( tag_no_case("#1"), |_| AddressingMode::IMPLIED)(data)
}

fn instruction(data: &str) -> IResult<&str, Instruction> {
    map(
        tuple((ws(mnemonic), ws(addressing_mode))),
        |(mnemonic, addressing_mode)| Instruction { mnemonic, addressing_mode }
    )(data)
}

fn assemble(data: &str) -> IResult<&str, Vec<Instruction>> {
    let mut parse = many0(instruction);
    parse(data)
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where
        F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(
        multispace0,
        inner,
        multispace0
    )
}
#[cfg(test)]
mod tests {
    use crate::assemble;

    #[test]
    fn can_assemble() {
        let asm = "LDA #1\nADC #1\nCMP #2";
        let result = assemble(asm);
        println!("{:?}", result);
    }
}