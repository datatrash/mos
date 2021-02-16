#![allow(dead_code)]

use std::fmt::Display;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;

use clap::{App, Arg, ArgMatches};
use fs_err::{read_to_string, OpenOptions};
use itertools::Itertools;

use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser::{
    parse, AddressingMode, ArgItem, Expression, ExpressionFactor, Located, NumberType, Token,
};
use crate::errors::MosResult;
use crate::LINE_ENDING;

pub enum Casing {
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

pub struct MnemonicOptions {
    casing: Casing,
    register_casing: Casing,
}

pub struct Options {
    mnemonics: MnemonicOptions,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            mnemonics: MnemonicOptions {
                casing: Casing::Lowercase,
                register_casing: Casing::Lowercase,
            },
        }
    }
}

struct Formatter<'a> {
    ast: Rc<Vec<Located<'a, Token<'a>>>>,
    options: Options,
    state: Vec<FormatterState>,
}

struct FormatterState {
    index: usize,
}

impl<'a> Formatter<'a> {
    fn new(ast: &'a [Located<'a, Token<'a>>], options: Options) -> Self {
        let ast = Rc::new(ast.to_vec());
        Self {
            ast,
            options,
            state: vec![],
        }
    }

    fn format(&mut self) -> String {
        self.format_tokens(&self.ast.clone())
    }

    fn format_tokens(&mut self, tokens: &[Located<Token>]) -> String {
        self.state.push(FormatterState { index: 0 });

        let result = tokens
            .iter()
            .map(|lt| self.format_token(lt))
            .collect_vec()
            .join("");

        self.state.pop();
        result
    }

    fn format_token<'f>(&mut self, lt: &'f Located<Token>) -> String {
        let result = match &lt.data {
            Token::Align { tag, value } => {
                let tag = self.format_located(tag);
                let value = format!(
                    "{}{}",
                    self.format_trivia(value),
                    self.format_expression(&value.data)
                );
                format!("{}{}", tag, value)
            }
            Token::Braces {
                lparen,
                inner,
                rparen,
            }
            | Token::Config {
                lparen,
                inner,
                rparen,
            } => {
                let lparen = self.format_display(lparen);
                let inner = self.format_tokens(&inner.data);
                let rparen = self.format_display(rparen);
                format!("{}{}{}", lparen, inner, rparen)
            }
            Token::ConfigPair { key, eq, value } => {
                let key = self.format_token(key);
                let value = self.format_token(value);
                format!("{}{}{}", key, eq, value)
            }
            Token::Data { values, size } => {
                let values = self.format_args(values);
                let size = self.format_located(size);
                format!("{}{}", size, values)
            }
            Token::Definition { tag, id, value } => {
                let value = self.format_optional_token(value);
                format!("{}{}{}", tag, id, value)
            }
            Token::EolTrivia(eol) => {
                let eol = eol.map_once(|_| LINE_ENDING);
                self.format_located(&eol)
            }
            Token::Eof => "".to_string(),
            Token::Error => panic!("Should not be formatting ASTs that contain errors"),
            Token::Expression(expr) => self.format_expression(expr),
            Token::IdentifierName(id) => id.0.to_string(),
            Token::If {
                tag_if,
                value,
                if_,
                tag_else,
                else_,
            } => {
                let tag_if = self.format_located(tag_if);
                let value = format!(
                    "{}{}",
                    self.format_trivia(value),
                    self.format_expression(&value.data)
                );
                let if_ = self.format_token(if_);
                let tag_else = self.format_optional_located(tag_else);
                let else_ = self.format_optional_token(else_);
                format!("{}{}{}{}{}", tag_if, value, if_, tag_else, else_)
            }
            Token::Include {
                tag,
                lquote,
                filename,
            } => {
                let tag = self.format_located(tag);
                let lquote = self.format_located(lquote);
                let filename = self.format_located(filename);
                format!("{}{}{}\"", tag, lquote, filename)
            }
            Token::Instruction(i) => {
                let mnemonic_data = self
                    .options
                    .mnemonics
                    .casing
                    .format(&i.mnemonic.data.to_string());
                let mnemonic = self.format_located(&i.mnemonic.map_once(|_| mnemonic_data));
                let operand = self.format_optional_token(&i.operand);
                format!("{}{}{}", self.format_trivia(lt), mnemonic, operand)
            }
            Token::Label { id, colon } => {
                let id = self.format_located(id);
                let colon = self.format_optional_located(colon);
                format!("{}{}", id, colon)
            }
            Token::Operand(o) => {
                let lchar = self.format_optional_located(&o.lchar);
                let rchar = self.format_optional_located(&o.rchar);
                let expr = format!(
                    "{}{}",
                    self.format_trivia(&o.expr),
                    self.format_expression(&o.expr.data)
                );
                let suffix = match &o.suffix {
                    Some(s) => self.format_token(s),
                    None => "".to_string(),
                };

                match &o.addressing_mode {
                    AddressingMode::AbsoluteOrZP => {
                        format!("{}{}", expr, suffix)
                    }
                    AddressingMode::Immediate => {
                        format!("{}{}", lchar, expr)
                    }
                    AddressingMode::Implied => "".to_string(),
                    AddressingMode::OuterIndirect => {
                        format!("{}{}{}{}", lchar, expr, rchar, suffix)
                    }
                    AddressingMode::Indirect => {
                        format!("{}{}{}{}", lchar, expr, suffix, rchar)
                    }
                }
            }
            Token::ProgramCounterDefinition { star, eq, value } => {
                let star = self.format_located(star);
                let eq = self.format_located(eq);
                let value = format!(
                    "{}{}",
                    self.format_trivia(value),
                    self.format_expression(&value.data)
                );
                format!("{}{}{}", star, eq, value)
            }
            Token::RegisterSuffix { comma, register } => {
                let comma = self.format_located(comma);
                let register = register.map(|r| {
                    self.options
                        .mnemonics
                        .register_casing
                        .format(&format!("{}", r))
                });
                let register = self.format_located(&register);
                format!("{}{}", comma, register)
            }
            Token::Segment { tag, id, inner } => {
                let tag = self.format_located(tag);
                let id = self.format_token(id);
                let inner = self.format_optional_token(inner);
                format!("{}{}{}", tag, id, inner)
            }
            Token::VariableDefinition { ty, id, eq, value } => {
                let ty = self.format_located(ty);
                let id = self.format_located(id);
                let eq = self.format_located(eq);
                let value = format!(
                    "{}{}",
                    self.format_trivia(&value),
                    self.format_expression(&value.data)
                );
                format!("{}{}{}{}", ty, id, eq, value)
            }
        };

        format!("{}{}", self.format_trivia(lt), result)
    }

    fn format_located<'f, T: Display>(&self, lt: &Located<'f, T>) -> String {
        format!("{}{}", self.format_trivia(lt), lt.data)
    }

    fn format_optional_located<'f, T: Display>(&self, lt: &Option<Located<'f, T>>) -> String {
        lt.as_ref()
            .map(|lt| format!("{}{}", self.format_trivia(&lt), lt.data))
            .unwrap_or_else(|| "".to_string())
    }

    fn format_optional_token<'f, T: Deref<Target = Located<'f, Token<'f>>>>(
        &mut self,
        lt: &'f Option<T>,
    ) -> String {
        lt.as_ref()
            .map(|t| self.format_token(t.deref()))
            .unwrap_or_else(|| "".to_string())
    }

    fn format_trivia<'f, T>(&self, lt: &Located<'f, T>) -> String {
        match &lt.trivia {
            Some(t) => t.data.iter().map(|item| format!("{}", item)).join(""),
            None => "".to_string(),
        }
    }

    fn format_display<T: Display>(&mut self, display: &T) -> String {
        format!("{}", display)
    }

    fn format_optional_display<T: Display>(&mut self, display: &Option<T>) -> String {
        display
            .as_ref()
            .map(|d| format!("{}", d))
            .unwrap_or_else(|| "".to_string())
    }

    fn format_expression(&mut self, expr: &Expression) -> String {
        match expr {
            Expression::BinaryExpression(be) => {
                let op = self.format_located(&be.op);
                let lhs = format!(
                    "{}{}",
                    self.format_trivia(&be.lhs),
                    self.format_expression(&be.lhs.data)
                );
                let rhs = format!(
                    "{}{}",
                    self.format_trivia(&be.rhs),
                    self.format_expression(&be.rhs.data)
                );
                format!("{}{}{}", lhs, op, rhs)
            }
            Expression::Factor {
                factor,
                tag_not,
                tag_neg,
                ..
            } => {
                let tag_not = self.format_optional_display(tag_not);
                let tag_neg = self.format_optional_display(tag_neg);
                let factor = format!(
                    "{}{}",
                    self.format_trivia(factor),
                    self.format_expression_factor(&factor.data)
                );
                format!("{}{}{}", tag_not, tag_neg, factor)
            }
        }
    }

    fn format_expression_factor(&mut self, factor: &ExpressionFactor) -> String {
        match factor {
            ExpressionFactor::CurrentProgramCounter(star) => self.format_located(star),
            ExpressionFactor::ExprParens {
                lparen,
                inner,
                rparen,
            } => {
                let lparen = self.format_located(lparen);
                let inner = format!(
                    "{}{}",
                    self.format_trivia(inner),
                    self.format_expression(&inner.data)
                );
                let rparen = self.format_located(rparen);
                format!("{}{}{}", lparen, inner, rparen)
            }
            ExpressionFactor::FunctionCall {
                name,
                lparen,
                args,
                rparen,
            } => {
                let name = self.format_token(name);
                let lparen = self.format_located(lparen);
                let args = self.format_args(args);
                let rparen = self.format_located(rparen);
                format!("{}{}{}{}", name, lparen, args, rparen)
            }
            ExpressionFactor::IdentifierValue { path, modifier } => {
                let path = self.format_located(path);
                let modifier = self.format_optional_display(modifier);
                format!("{}{}", modifier, path)
            }
            ExpressionFactor::Number { ty, value } => {
                let value = value.map(|v| match &ty.data {
                    NumberType::Hex => {
                        if *v < 256 {
                            format!("{:02x}", v)
                        } else {
                            format!("{:04x}", v)
                        }
                    }
                    NumberType::Bin => format!("{:b}", v),
                    NumberType::Dec => format!("{}", v),
                });
                let value = self.format_located(&value);
                let ty = self.format_located(ty);
                format!("{}{}", ty, value)
            }
        }
    }

    fn format_args(&self, args: &[ArgItem]) -> String {
        args.iter()
            .map(|(arg, comma)| {
                let arg = self.format_located(arg);
                let comma = self.format_optional_located(comma);
                format!("{}{}", arg, comma)
            })
            .join("")
    }
}

fn format<'a>(ast: &[Located<'a, Token<'a>>], options: Options) -> String {
    let mut fmt = Formatter::new(ast, options);
    fmt.format()
}

pub fn format_app() -> App<'static> {
    App::new("format").about("Formats input file(s)").arg(
        Arg::new("input")
            .about("Sets the input file(s) to use")
            .required(true)
            .multiple(true),
    )
}

pub fn format_command(args: &ArgMatches) -> MosResult<()> {
    let input_names = args.values_of("input").unwrap().collect_vec();

    for input_name in input_names {
        let source = read_to_string(input_name)?;
        let ast = parse(input_name.as_ref(), &source)?;
        let _code = codegen(ast.clone(), CodegenOptions::default())?;
        let formatted = format(&ast, Options::default());
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
    use crate::commands::format::{format, Options};
    use crate::core::parser::parse;
    use crate::errors::MosResult;

    #[test]
    fn passthrough_formatting_should_result_in_original() -> MosResult<()> {
        let source = include_str!("../../../test/cli/format/valid-unformatted.asm");
        let expected = include_str!("../../../test/cli/format/valid-unformatted.asm");
        let ast = parse("test.asm".as_ref(), source)?;
        let actual = format(&ast, Options::default());
        assert_eq!(actual, expected);
        Ok(())
    }
}
