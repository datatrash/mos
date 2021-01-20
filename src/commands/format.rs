#![allow(dead_code)]
use crate::core::parser::*;
use anyhow::Result;
use clap::{App, Arg, ArgMatches};
use fs_err::{read_to_string, OpenOptions};
use std::io::Write;

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

fn format_token(token: &Token, opts: &Options) -> String {
    match token {
        Token::Instruction(i) => {
            let mnem = opts.mnemonics.casing.format(&i.mnemonic.to_string());
            let operand = i.operand.as_ref().map(|o| format_token(&o, opts));

            match operand {
                Some(operand) => format!("{} {}", mnem, operand),
                None => mnem,
            }
        }
        Token::Operand(o) => {
            let expr = format_token(&*o.expr, opts);
            let suffix = o
                .suffix
                .as_ref()
                .map(|o| format_token(&o, opts))
                .unwrap_or_else(|| "".to_string());

            match &o.addressing_mode {
                AddressingMode::AbsoluteOrZP => format!("{}{}", expr, suffix),
                AddressingMode::Immediate => expr,
                AddressingMode::Implied => expr,
                AddressingMode::Indirect => format!("({}{})", expr, suffix),
                AddressingMode::OuterIndirect => format!("({}){}", expr, suffix),
            }
        }
        Token::Number(val, ty) => match ty {
            NumberType::Hex => format!("${:x}", val),
            NumberType::Dec => format!("{}", val),
        },
        Token::Data(expr, size) => {
            let expr = expr
                .as_ref()
                .map(|t| format_token(t, opts))
                .unwrap_or_else(|| "".to_string());
            match size {
                1 => format!(".byte {}", expr),
                2 => format!(".word {}", expr),
                4 => format!(".dword {}", expr),
                _ => panic!(),
            }
        }
        Token::Ws((lhs, inner, rhs)) => {
            let lhs = lhs
                .iter()
                .map(|l| format!("{}", l))
                .collect::<Vec<_>>()
                .join(" ");
            let inner = format_token(inner, opts);
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
        Token::Identifier(id) => id.0.clone(),
        Token::Label(id) => format!("{}:", id.0),
        Token::RegisterSuffix(reg) => match reg {
            Register::X => ", x".to_string(),
            Register::Y => ", y".to_string(),
        },
        Token::ExprParens(inner) => format!("[{}]", format_token(inner, opts)),
        Token::BinaryAdd(lhs, rhs) => {
            format!("{} + {}", lhs, rhs)
        }
        Token::BinarySub(lhs, rhs) => {
            format!("{} - {}", lhs, rhs)
        }
        Token::BinaryMul(lhs, rhs) => {
            format!("{} * {}", lhs, rhs)
        }
        Token::BinaryDiv(lhs, rhs) => {
            format!("{} / {}", lhs, rhs)
        }
        Token::Error => panic!("Formatting should not happen on ASTs containing errors"),
    }
}

fn format(ast: &[Token], opts: &Options) -> String {
    let mut str = "".to_string();

    let mut prev_token: Option<&Token> = None;
    for token in ast {
        if let Some(pt) = prev_token {
            let require_newline = match (pt, token) {
                (Token::Instruction(_), Token::Instruction(_)) => false,
                (Token::Label(_), Token::Instruction(_)) => false,
                (Token::Instruction(_), _) => true,
                _ => false,
            };

            if require_newline {
                str += LINE_ENDING;
            }
        }

        let t = format_token(token, opts);

        let indent = match token {
            Token::Instruction(_) | Token::Data(_, _) => 4,
            _ => 0,
        };

        for _ in 0..indent {
            str += " "
        }
        str += &t;
        str += LINE_ENDING;

        prev_token = Some(token);
    }

    // Remove last line ending
    for _ in 0..LINE_ENDING.len() {
        str.pop();
    }

    str
}

pub fn format_app() -> App<'static> {
    App::new("format").about("Source code formatter").arg(
        Arg::new("input")
            .about("Sets the input file(s) to use")
            .required(true)
            .multiple(true),
    )
}

pub fn format_command(args: &ArgMatches) -> Result<()> {
    let input_names = args.values_of("input").unwrap().collect::<Vec<_>>();

    for input_name in input_names {
        let source = read_to_string(input_name)?;
        let (ast, errors) = parse(input_name, &source);
        if errors.is_empty() {
            let formatted = format(&ast, &Options::default());
            let mut output_file = OpenOptions::new()
                .truncate(true)
                .write(true)
                .open(input_name)?;
            output_file.write_all(formatted.as_bytes())?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::commands::format::{format, Options};
    use crate::commands::{format_app, format_command};
    use crate::core::parser::parse;
    use anyhow::Result;

    #[test]
    fn format_valid_code() {
        let source = include_str!("../../test/valid-unformatted.asm");
        let expected = include_str!("../../test/valid-formatted.asm");
        let (ast, errors) = parse("test.asm", source);
        assert_eq!(errors.is_empty(), true);
        assert_eq!(format(&ast, &Options::default()), expected);
    }

    #[test]
    fn can_invoke_format_on_valid_file() -> Result<()> {
        let root = env!("CARGO_MANIFEST_DIR");
        let unformatted = &format!("{}/test/valid-unformatted.asm", root);
        let formatted = &format!("{}/test/valid-formatted.asm", root);
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
