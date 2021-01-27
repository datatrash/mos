#![allow(dead_code)]

use std::io::Write;

use clap::{App, Arg, ArgMatches};
use fs_err::{read_to_string, OpenOptions};

use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::*;
use crate::errors::MosResult;

#[cfg(windows)]
const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &str = "\n";

enum Casing {
    Uppercase,
    Lowercase,
}

impl Casing {
    fn format(&self, s: &str) -> String {
        match self {
            Casing::Uppercase => s.to_uppercase(),
            Casing::Lowercase => s.to_lowercase(),
        }
    }
}

struct MnemonicOptions {
    casing: Casing,
}

struct Options {
    mnemonics: MnemonicOptions,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            mnemonics: MnemonicOptions {
                casing: Casing::Lowercase,
            },
        }
    }
}

fn format_expression(token: &Expression, opts: &Options) -> String {
    match token {
        Expression::Number(val, ty) => match ty {
            NumberType::Hex => format!("${:x}", val),
            NumberType::Bin => format!("%{:b}", val),
            NumberType::Dec => format!("{}", val),
        },
        Expression::IdentifierValue(id, modifier) => {
            let modifier = match modifier {
                Some(m) => m.to_string(),
                None => "".to_string(),
            };
            format!("{}{}", modifier, id.0)
        }
        Expression::ExprParens(inner) => format!("[{}]", format_expression(&inner.data, opts)),
        Expression::CurrentProgramCounter => "*".to_string(),
        Expression::BinaryAdd(lhs, rhs) => {
            format!("{} + {}", lhs.data, rhs.data)
        }
        Expression::BinarySub(lhs, rhs) => {
            format!("{} - {}", lhs.data, rhs.data)
        }
        Expression::BinaryMul(lhs, rhs) => {
            format!("{} * {}", lhs.data, rhs.data)
        }
        Expression::BinaryDiv(lhs, rhs) => {
            format!("{} / {}", lhs.data, rhs.data)
        }
        Expression::Ws(lhs, inner, rhs) => {
            format_ws(lhs, format_expression(&inner.data, opts), rhs, opts)
        }
    }
}

fn indent_str(indent: usize) -> String {
    let mut str = "".to_string();
    for _ in 0..indent * 4 {
        str += " "
    }
    str
}

fn format_token(token: &Token, opts: &Options, indent: usize) -> String {
    match token {
        Token::Braces(tokens) => {
            let mut tokens = tokens
                .iter()
                .map(|t| format_token(&t.data, opts, indent + 1))
                .collect::<Vec<_>>()
                .join(LINE_ENDING)
                .trim_end() // if the scope ends with a newline due to for instance a 'rts', we trim it off here
                .to_string();
            if !tokens.is_empty() {
                tokens = format!("{le}{t}{le}", le = LINE_ENDING, t = tokens);
            }

            // Scopes always end with an extra newline
            format!(
                "{ind}{{{t}{ind}}}{le}",
                ind = indent_str(indent + 1),
                t = tokens,
                le = LINE_ENDING
            )
        }
        Token::Instruction(i) => {
            let mnem = opts.mnemonics.casing.format(&i.mnemonic.to_string());
            let operand = i
                .operand
                .as_ref()
                .map(|o| format_token(&o.data, opts, indent));

            let operand = operand.unwrap_or_else(|| "".to_string());
            let extra_newline = match &i.mnemonic {
                Mnemonic::Rts => LINE_ENDING,
                _ => "",
            };
            let ind = indent_str(indent + 1);
            format!("{}{} {}{}", ind, mnem, operand, extra_newline)
        }
        Token::Operand(o) => {
            let expr = format_expression(&o.expr.data, opts);
            let suffix = o
                .suffix
                .as_ref()
                .map(|o| format_token(&o.data, opts, indent))
                .unwrap_or_else(|| "".to_string());

            match &o.addressing_mode {
                AddressingMode::AbsoluteOrZP => format!("{}{}", expr, suffix),
                AddressingMode::Immediate => expr,
                AddressingMode::Implied => expr,
                AddressingMode::Indirect => format!("({}{})", expr, suffix),
                AddressingMode::OuterIndirect => format!("({}){}", expr, suffix),
            }
        }
        Token::IdentifierName(id) => {
            format!("{}", id)
        }
        Token::VariableDefinition(id, val, ty) => {
            let ty = match ty {
                VariableType::Variable => ".var",
                VariableType::Constant => ".const",
            };
            format!("{} {} = {}", ty, id, val.data)
        }
        Token::Data(expr, size) => {
            let expr = expr
                .iter()
                .map(|t| format_expression(&t.data, opts))
                .collect::<Vec<_>>()
                .join(", ");
            match size {
                1 => format!("{}.byte {}", indent_str(indent + 1), expr),
                2 => format!("{}.word {}", indent_str(indent + 1), expr),
                4 => format!("{}.dword {}", indent_str(indent + 1), expr),
                _ => panic!(),
            }
        }
        Token::Ws(lhs, inner, rhs) => {
            format_ws(lhs, format_token(&inner.data, opts, indent), rhs, opts)
        }
        Token::Label(id) => format!("{}:", id.0),
        Token::RegisterSuffix(reg) => match reg {
            Register::X => ", x".to_string(),
            Register::Y => ", y".to_string(),
        },
        Token::Error => panic!("Formatting should not happen on ASTs containing errors"),
    }
}

fn format_ws(lhs: &[Comment], inner: String, rhs: &[Comment], _opts: &Options) -> String {
    let lhs = lhs
        .iter()
        .map(|l| format!("{}", l))
        .collect::<Vec<_>>()
        .join(" ");
    let rhs = rhs
        .iter()
        .map(|l| format!("{}", l))
        .collect::<Vec<_>>()
        .join(" ");
    let lhs_spacing = if lhs.is_empty() {
        "".to_string()
    } else {
        " ".to_string()
    };
    let rhs_spacing = if rhs.is_empty() {
        "".to_string()
    } else {
        " ".to_string()
    };
    format!("{}{}{}{}{}", lhs, lhs_spacing, inner, rhs_spacing, rhs)
}

fn format<'a>(ast: &[Located<'a, Token<'a>>], opts: &Options) -> String {
    ast.iter()
        .map(|lt| {
            let token = &lt.data;
            format_token(token, opts, 0)
        })
        .collect::<Vec<_>>()
        .join(LINE_ENDING)
}

pub fn format_app() -> App<'static> {
    App::new("format").about("Source code formatter").arg(
        Arg::new("input")
            .about("Sets the input file(s) to use")
            .required(true)
            .multiple(true),
    )
}

pub fn format_command(args: &ArgMatches) -> MosResult<()> {
    let input_names = args.values_of("input").unwrap().collect::<Vec<_>>();

    for input_name in input_names {
        let source = read_to_string(input_name)?;
        let ast = parse(input_name, &source)?;
        let _code = codegen(ast.clone(), CodegenOptions::default())?;
        let formatted = format(&ast, &Options::default());
        let mut output_file = OpenOptions::new()
            .truncate(true)
            .write(true)
            .open(input_name)?;
        output_file.write_all(formatted.as_bytes())?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use crate::commands::format::{format, Options};
    use crate::commands::{format_app, format_command};
    use crate::core::parser::parse;

    #[test]
    fn format_valid_code() -> Result<()> {
        let source = include_str!("../../test/cli/format/valid-unformatted.asm");
        let expected = include_str!("../../test/cli/format/valid-formatted.asm");
        let ast = parse("test.asm", source)?;
        assert_eq!(format(&ast, &Options::default()), expected);
        Ok(())
    }

    #[test]
    fn can_invoke_format_on_valid_file() -> Result<()> {
        let root = env!("CARGO_MANIFEST_DIR");
        let unformatted = &format!("{}/test/cli/format/valid-unformatted.asm", root);
        let formatted = &format!("{}/test/cli/format/valid-formatted.asm", root);
        let copied = &format!("{}/target/can_invoke_format.asm", root);
        std::fs::copy(unformatted, copied)?;

        let args = format_app().get_matches_from(vec!["format", copied]);
        format_command(&args)?;

        assert_eq!(
            std::fs::read_to_string(formatted)?,
            std::fs::read_to_string(copied)?
        );
        Ok(())
    }
}
