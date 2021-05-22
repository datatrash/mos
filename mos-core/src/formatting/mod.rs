use crate::parser::{
    AddressModifier, AddressingMode, ArgItem, BinaryOp, Block, Expression, ExpressionFactor,
    Identifier, IdentifierPath, ImportArgs, ImportAs, Located, Number, NumberType, Operand,
    ParseTree, RegisterSuffix, SpecificImportArg, TextEncoding, Token, Trivia,
};
use serde::Deserialize;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
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

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct MnemonicOptions {
    pub casing: Casing,
    pub register_casing: Casing,
}

impl Default for MnemonicOptions {
    fn default() -> Self {
        Self {
            casing: Casing::Lowercase,
            register_casing: Casing::Lowercase,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum BracePosition {
    SameLine,
    NewLine,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct BraceOptions {
    pub position: BracePosition,
}

impl Default for BraceOptions {
    fn default() -> Self {
        Self {
            position: BracePosition::SameLine,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct WhitespaceOptions {
    pub indent: usize,
    pub label_margin: usize,
    pub code_margin: usize,
}

impl Default for WhitespaceOptions {
    fn default() -> Self {
        Self {
            indent: 4,
            label_margin: 20,
            code_margin: 30,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "snake_case")]
pub struct FormattingOptions {
    pub mnemonics: MnemonicOptions,
    pub braces: BraceOptions,
    pub whitespace: WhitespaceOptions,
}

impl Default for FormattingOptions {
    fn default() -> Self {
        Self {
            mnemonics: MnemonicOptions::default(),
            braces: BraceOptions::default(),
            whitespace: WhitespaceOptions::default(),
        }
    }
}

struct CodeFormatter {
    tree: Arc<ParseTree>,
    options: FormattingOptions,
    indent: usize,
    chunks: Vec<Chunk>,
    spc_if_next: bool,
}

#[derive(Copy, Clone, Debug)]
enum ChunkType {
    Label,
    Comment,
}

#[derive(Debug)]
struct Chunk {
    ty: Option<ChunkType>,
    indent: usize,
    str: String,
}

impl CodeFormatter {
    fn new(tree: Arc<ParseTree>, options: FormattingOptions) -> Self {
        Self {
            tree,
            options,
            indent: 0,
            chunks: vec![],
            spc_if_next: false,
        }
    }

    fn format<P: Into<PathBuf>>(mut self, path: P) -> String {
        let path = path.into();
        let tree = self.tree.clone();
        self.format_tokens(
            &tree.try_get_file(path).expect("File not found").tokens,
            false,
        );
        join_chunks(self.chunks, &self.options)
    }

    fn format_tokens(&mut self, tokens: &[Token], trim_leading_trivia: bool) {
        // Leading trivia
        let chunk_index = self.chunks.len();
        self.format_line(tokens, None);

        if trim_leading_trivia {
            // If we want to trim leading trivia, we'll remove any new-line chunks that were added
            while let Some(chunk) = self.chunks.get(chunk_index) {
                if chunk.str == "\n" {
                    self.chunks.remove(chunk_index);
                } else {
                    break;
                }
            }
        }

        for token_idx in 0..tokens.len() {
            let token = &tokens[token_idx];

            // Determine if we need some additional newlines before this token
            if let Token::Error(_) = token {
                // Don't do anything special
            } else if let Some(prev_token) = if token_idx > 0 {
                tokens.get(token_idx - 1)
            } else {
                None
            } {
                let (newline_if_same, newline_if_diff) = match token {
                    Token::Braces { .. } => (true, true),
                    Token::Data { .. } | Token::Text { .. } => (
                        false,
                        !matches!(prev_token, Token::Data { .. } | Token::Text { .. }),
                    ),
                    Token::Definition { .. } => (true, true),
                    Token::If { .. } => (true, true),
                    Token::Import { block, .. } => (block.is_some(), true),
                    Token::Instruction(_) | Token::MacroInvocation { .. } => (
                        false,
                        !matches!(
                            prev_token,
                            Token::Instruction(_) | Token::MacroInvocation { .. }
                        ),
                    ),
                    Token::Label { block, .. } => (
                        block.is_some(),
                        block.is_some()
                            || matches!(prev_token, Token::Braces { .. } | Token::If { .. }),
                    ),
                    Token::Loop { .. } => (true, true),
                    Token::Segment { .. } => (true, true),
                    _ => (false, true),
                };

                let token_type = std::mem::discriminant(token);
                let prev_token_type = std::mem::discriminant(prev_token);

                if (token_type == prev_token_type && newline_if_same)
                    || (token_type != prev_token_type && newline_if_diff)
                {
                    self.push("\n");
                }
            }

            self.format_line(tokens, Some(token_idx));
        }
    }

    fn format_line(&mut self, tokens: &[Token], token_idx: Option<usize>) {
        if let Some(idx) = token_idx {
            let token = &tokens[idx];
            self.format_token(token)
        };

        let trivia_idx = match token_idx {
            Some(idx) => idx + 1,
            None => 0,
        };
        if let Some(next_token) = &tokens.get(trivia_idx) {
            next_token.trivia().map(|t| self.fmt(t));
        }
    }

    fn format_token(&mut self, token: &Token) {
        match token {
            Token::Align { tag, value } => {
                self.push(&tag.data).push(" ").fmt(value.as_ref());
            }
            Token::Braces { block, .. } | Token::Config(block) => {
                self.format_block(block);
            }
            Token::ConfigPair { key, eq, value } => {
                self.push(&key.data)
                    .push(" ")
                    .fmt(eq)
                    .push(" ")
                    .fmt(value.as_ref());
            }
            Token::Data { values, size } => {
                self.push(&size.data).push(" ").fmt(values);
            }
            Token::Definition { tag, id, value } => {
                self.push(&tag.data).push(" ").fmt(id).push(" ").fmt(value);
            }
            Token::Error(e) => {
                self.push(&e.data);
            }
            Token::Eof(_) => (),
            Token::Expression(expr) => self.format_expression(expr),
            Token::File {
                tag,
                lquote,
                filename,
            } => {
                self.push(&tag.data)
                    .push(" ")
                    .fmt(lquote)
                    .fmt(filename)
                    .push("\"");
            }
            Token::If {
                tag_if,
                value,
                if_,
                tag_else,
                else_,
            } => {
                self.push(&tag_if.data)
                    .push(" ")
                    .fmt(value)
                    .push(" ")
                    .fmt(if_);

                if let Some(tag_else) = tag_else {
                    self.push(" ")
                        .fmt(tag_else.as_ref())
                        .push(" ")
                        .fmt(else_.as_ref().unwrap());
                }
            }
            Token::Import {
                tag,
                args,
                from,
                lquote,
                filename,
                block,
                ..
            } => {
                self.push(&tag.data).push(" ");

                match args {
                    ImportArgs::All(c, as_) => {
                        self.fmt(c.as_ref())
                            .spc_if_next()
                            .fmt(as_)
                            .clear_spc_if_next();
                    }
                    ImportArgs::Specific(args) => {
                        self.fmt(args);
                    }
                };

                self.push(" ")
                    .fmt(from.as_ref())
                    .push(" ")
                    .fmt(lquote.as_ref())
                    .fmt(filename.as_ref())
                    .push("\"")
                    .spc_if_next()
                    .fmt(block);
            }
            Token::Instruction(i) => {
                self.push(
                    &self
                        .options
                        .mnemonics
                        .casing
                        .format(&i.mnemonic.data.to_string()),
                )
                .spc_if_next()
                .fmt(&i.operand)
                .clear_spc_if_next();
            }
            Token::Label {
                id,
                colon: _,
                block,
            } => {
                self.push_type(ChunkType::Label, format!("{}:", &id.data))
                    .fmt(block);
            }
            Token::Loop {
                tag,
                loop_scope: _,
                expr,
                block,
            } => {
                self.push(&tag.data)
                    .push(" ")
                    .fmt(expr.as_ref())
                    .push(" ")
                    .fmt(block);
            }
            Token::MacroDefinition {
                tag,
                id,
                lparen,
                args,
                rparen,
                block,
            } => {
                self.push(&tag.data)
                    .push(" ")
                    .fmt(id.as_ref())
                    .fmt(lparen.as_ref())
                    .fmt(args)
                    .fmt(rparen.as_ref())
                    .spc_if_next()
                    .fmt(block);
            }
            Token::MacroInvocation {
                id,
                lparen,
                args,
                rparen,
            } => {
                self.push(&id.data)
                    .fmt(lparen.as_ref())
                    .fmt(args)
                    .fmt(rparen.as_ref());
            }
            Token::ProgramCounterDefinition { star, eq, value } => {
                self.push(&star.data)
                    .push(" ")
                    .fmt(eq.as_ref())
                    .push(" ")
                    .fmt(value.as_ref());
            }
            Token::Segment { tag, id, block } => {
                self.push(&tag.data)
                    .push(" ")
                    .fmt(id.as_ref())
                    .push(" ")
                    .fmt(block);
            }
            Token::Text {
                tag,
                encoding,
                lquote,
                text,
            } => {
                self.push(&tag.data)
                    .push(" ")
                    .fmt(encoding)
                    .push(" ")
                    .fmt(lquote.as_ref())
                    .fmt(text.as_ref())
                    .push("\"");
            }
            Token::VariableDefinition { ty, id, eq, value } => {
                self.push(&ty.data)
                    .push(" ")
                    .fmt(id)
                    .push(" ")
                    .fmt(eq)
                    .push(" ")
                    .fmt(value);
            }
        }
    }

    fn format_block(&mut self, block: &Block) {
        self.push(&block.lparen.data).push("\n");

        // Since we want to deal with tokens and the trivia _after_ the token,
        // we have to grab the trivia from the 'rparen' and attach it to the inner token,
        // so that format_tokens can deal with the full Vec<Token> correctly.
        let mut inner = block.inner.clone();
        inner.push(Token::Eof(block.rparen.clone().map_into(|_| ())));
        self.indent += self.options.whitespace.indent;
        self.format_tokens(&inner, true);
        self.indent -= self.options.whitespace.indent;

        // Make sure the inner chunk ends with exactly one new-line.
        while let Some(last_chunk) = self.chunks.last() {
            if last_chunk.str == "\n" {
                self.chunks.pop();
            } else {
                break;
            }
        }
        self.push("\n");

        // Now, in order to prevent the rparen trivia to be handled twice, we don't use rparen's trivia.
        self.push(&block.rparen.data);
    }

    fn format_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::BinaryExpression(b) => {
                self.fmt(b.lhs.as_ref())
                    .push(" ")
                    .fmt(&b.op)
                    .push(" ")
                    .fmt(b.rhs.as_ref());
            }
            Expression::Factor {
                factor,
                flags: _,
                tag_not,
                tag_neg,
            } => {
                self.fmt(tag_not).fmt(tag_neg).fmt(factor.as_ref());
            }
        }
    }

    fn format_expression_factor(&mut self, factor: &ExpressionFactor) {
        match factor {
            ExpressionFactor::CurrentProgramCounter(star) => {
                self.fmt(star);
            }
            ExpressionFactor::ExprParens {
                lparen,
                inner,
                rparen,
            } => {
                self.fmt(lparen).fmt(inner.as_ref()).fmt(rparen);
            }
            ExpressionFactor::FunctionCall {
                name,
                lparen,
                args,
                rparen,
            } => {
                self.fmt(name).fmt(lparen).fmt(args).fmt(rparen);
            }
            ExpressionFactor::IdentifierValue { path, modifier } => {
                self.fmt(modifier).fmt(path);
            }
            ExpressionFactor::Number { ty, value } => {
                self.fmt(ty).fmt(value);
            }
        }
    }

    fn fmt<F: Formattable>(&mut self, f: F) -> &mut Self {
        f.format(self);
        self
    }

    fn spc_if_next(&mut self) -> &mut Self {
        self.spc_if_next = true;
        self
    }

    fn clear_spc_if_next(&mut self) -> &mut Self {
        self.spc_if_next = false;
        self
    }

    fn push<S: Display>(&mut self, str: S) -> &mut Self {
        self.push_type(None, str)
    }

    fn push_type<TY: Into<Option<ChunkType>>, S: Display>(&mut self, ty: TY, str: S) -> &mut Self {
        let mut str = str.to_string();
        if str.is_empty() {
            return self;
        }

        if self.spc_if_next {
            str = format!(" {}", str);
            self.spc_if_next = false;
        }

        self.chunks.push(Chunk {
            ty: ty.into(),
            indent: self.indent,
            str,
        });
        self
    }
}

trait Formattable {
    fn format(&self, formatter: &mut CodeFormatter);
}

macro_rules! basic_format {
    ($s:ty) => {
        impl Formattable for $s {
            fn format(&self, formatter: &mut CodeFormatter) {
                formatter.push(self);
            }
        }
    };
}

basic_format!(&char);
basic_format!(&str);
basic_format!(&String);
basic_format!(&AddressModifier);
basic_format!(&BinaryOp);
basic_format!(&Identifier);
basic_format!(&IdentifierPath);
basic_format!(&NumberType);
basic_format!(&Number);
basic_format!(&TextEncoding);

impl Formattable for &Block {
    fn format(&self, formatter: &mut CodeFormatter) {
        formatter.format_block(self);
    }
}

impl Formattable for &Operand {
    fn format(&self, formatter: &mut CodeFormatter) {
        let fmt_suffix = |formatter: &mut CodeFormatter, suffix: &Option<RegisterSuffix>| {
            if let Some(s) = suffix {
                formatter.fmt(&s.comma).spc_if_next();
                formatter.fmt(&s.register.map(|r| {
                    formatter
                        .options
                        .mnemonics
                        .register_casing
                        .format(&r.to_string())
                }));
                formatter.clear_spc_if_next();
            }
        };

        formatter.fmt(&self.lchar).fmt(&self.expr);

        match self.addressing_mode {
            AddressingMode::Indirect => {
                fmt_suffix(formatter, &self.suffix);
                formatter.fmt(&self.rchar);
            }
            AddressingMode::OuterIndirect => {
                formatter.fmt(&self.rchar);
                fmt_suffix(formatter, &self.suffix);
            }
            _ => {
                fmt_suffix(formatter, &self.suffix);
            }
        }
    }
}

impl Formattable for &Token {
    fn format(&self, formatter: &mut CodeFormatter) {
        formatter.format_token(self);
    }
}

impl Formattable for &Box<Token> {
    fn format(&self, formatter: &mut CodeFormatter) {
        formatter.format_token(self);
    }
}

impl Formattable for &Expression {
    fn format(&self, formatter: &mut CodeFormatter) {
        formatter.format_expression(self);
    }
}

impl Formattable for &ExpressionFactor {
    fn format(&self, formatter: &mut CodeFormatter) {
        formatter.format_expression_factor(self);
    }
}

impl Formattable for &ImportAs {
    fn format(&self, formatter: &mut CodeFormatter) {
        formatter
            .fmt(self.tag.as_ref())
            .push(" ")
            .fmt(self.path.as_ref());
    }
}

impl Formattable for &Vec<ArgItem<Expression>> {
    fn format(&self, formatter: &mut CodeFormatter) {
        for (expr, comma) in *self {
            formatter.fmt(expr).fmt(comma).spc_if_next();
        }
        formatter.clear_spc_if_next();
    }
}

impl Formattable for &Vec<ArgItem<Identifier>> {
    fn format(&self, formatter: &mut CodeFormatter) {
        for (id, comma) in *self {
            formatter.fmt(id).fmt(comma).spc_if_next();
        }
        formatter.clear_spc_if_next();
    }
}

impl Formattable for &Vec<ArgItem<SpecificImportArg>> {
    fn format(&self, formatter: &mut CodeFormatter) {
        for (path, comma) in *self {
            formatter
                .fmt(&path.data.path)
                .spc_if_next()
                .fmt(&path.data.as_)
                .fmt(comma)
                .spc_if_next();
        }
        formatter.clear_spc_if_next();
    }
}

impl<'a, T> Formattable for &'a Option<T>
where
    &'a T: Formattable,
    T: 'a,
{
    fn format(&self, formatter: &mut CodeFormatter) {
        if let Some(s) = self.as_ref() {
            s.format(formatter)
        }
    }
}

impl<'a, T> Formattable for &'a Located<T>
where
    &'a T: Formattable,
    T: 'a,
{
    fn format(&self, formatter: &mut CodeFormatter) {
        if let Some(t) = self.trivia.as_ref() {
            formatter.fmt(&t.data);
        }

        formatter.fmt(&self.data);
    }
}

impl Formattable for &Vec<Trivia> {
    fn format(&self, formatter: &mut CodeFormatter) {
        for triv in *self {
            match triv {
                Trivia::CStyle(comment) | Trivia::CppStyle(comment) => {
                    formatter.push_type(ChunkType::Comment, comment);
                }
                Trivia::Whitespace(_) => (),
                Trivia::NewLine => {
                    formatter.push("\n");
                }
            };
        }
    }
}

pub fn format<P: Into<PathBuf>>(
    path: P,
    ast: Arc<ParseTree>,
    options: FormattingOptions,
) -> String {
    let fmt = CodeFormatter::new(ast, options);
    fmt.format(path)
}

fn join_chunks(chunks: Vec<Chunk>, options: &FormattingOptions) -> String {
    let mut result = vec![];
    let num_chunks = chunks.len();

    let mut line: String = "".into();
    let mut indent = None;
    let mut had_standalone_comment = false;
    let mut prev_newlines = 0;

    for (idx, chunk) in chunks.iter().enumerate() {
        if indent.is_none() {
            indent = Some(chunk.indent);
        }

        for str in chunk.str.split_inclusive("\n") {
            let mut ignore = false;

            match chunk.ty {
                Some(ChunkType::Label) => {
                    if line.len() > options.whitespace.label_margin {
                        line += format!("{} ", str).as_str();
                    } else {
                        line += format!(
                            "{:>width$}",
                            format!("{} ", str),
                            width = options.whitespace.label_margin
                        )
                        .as_str();
                    }
                }
                None => {
                    if str == "\n"
                        && !line.is_empty()
                        && line.len() <= options.whitespace.label_margin
                    {
                        // If we just have a label so far, let's ignore the new-line so we can maybe put the code on the same line as
                        // the label
                        ignore = true;
                    } else {
                        line = format!(
                            "{:<width$}{}",
                            line,
                            str,
                            width = options.whitespace.label_margin
                        );
                    }
                }
                Some(ChunkType::Comment) => {
                    let mut is_eol = false;

                    // Is the next chunk a newline? Then this comment is at the end of the line.
                    if let Some(next_chunk) = chunks.get(idx + 1) {
                        if next_chunk.str == "\n" {
                            is_eol = true;
                        }
                    } else {
                        // eof, so definitely eol :)
                        is_eol = true;
                    }

                    if is_eol {
                        // An EOL comment will be formatted in a separate column
                        line = format!(
                            "{:<width$}{}",
                            line,
                            str,
                            width =
                                options.whitespace.label_margin + options.whitespace.code_margin
                        );
                    } else {
                        // Add comment in-line (making sure 'line' has at least a minimum width) and add a space at the end to cleanly separate it
                        line = format!(
                            "{:<width$}{} ",
                            line,
                            str,
                            width = options.whitespace.label_margin
                        );
                    }
                }
            }

            if (!ignore && str.contains('\n')) || idx == num_chunks - 1 {
                let should_add;

                if line.trim().is_empty() {
                    // We should only add empty lines if:
                    // - The previous line was not a standalone comment
                    // - We did not have more than 1 empty line already
                    should_add = !had_standalone_comment && prev_newlines == 0;
                    if should_add {
                        prev_newlines += 1;
                    }
                } else {
                    // If the line only consists of comments, move them to the 'code' column
                    if line.len() > options.whitespace.label_margin + options.whitespace.code_margin
                    {
                        let (label_code, comment) = line.split_at(
                            options.whitespace.label_margin + options.whitespace.code_margin,
                        );
                        if label_code.trim().is_empty() {
                            line = format!(
                                "{:<width$}{}",
                                "",
                                comment,
                                width = options.whitespace.label_margin
                            );
                            had_standalone_comment = true;
                            prev_newlines = 0;
                            should_add = true;
                        } else {
                            had_standalone_comment = false;
                            prev_newlines = 0;
                            should_add = true;
                        }
                    } else {
                        had_standalone_comment = false;
                        prev_newlines = 0;
                        should_add = true;
                    }
                }

                if should_add {
                    let formatted = format!(
                        "{:<indent$}{}",
                        "",
                        line,
                        indent = indent.unwrap_or_default()
                    )
                    .trim_end()
                    .to_string();
                    result.push(formatted);
                }

                line = "".into();
                indent = None;
            }
        }
    }

    result.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::CoreResult;
    use crate::parser::parse_or_err;
    use crate::parser::source::{InMemoryParsingSource, ParsingSource};
    use crate::testing::xplat_eq;
    use std::sync::{Arc, Mutex};

    #[test]
    fn default_formatting() -> CoreResult<()> {
        let source = include_str!("../../test-data/format/valid-unformatted.asm");
        let expected = include_str!("../../test-data/format/valid-formatted.asm");
        let ast = parse_or_err("test.asm".as_ref(), get_source(source))?;
        let actual = format("test.asm", ast, FormattingOptions::default());
        xplat_eq(actual, expected);
        Ok(())
    }

    #[test]
    fn roundtrip_formatting() -> CoreResult<()> {
        let source = include_str!("../../test-data/format/valid-formatted.asm");
        let expected = include_str!("../../test-data/format/valid-formatted.asm");
        let ast = parse_or_err("test.asm".as_ref(), get_source(source))?;
        let actual = format("test.asm", ast, FormattingOptions::default());
        xplat_eq(actual, expected);
        Ok(())
    }

    #[test]
    fn can_join_chunks() {
        let chunks = vec![
            chunk(Some(ChunkType::Comment), 0, "// hello"),
            chunk(None, 0, "\n"),
            chunk(None, 0, "\n"),
            chunk(None, 0, "\n"),
            chunk(None, 0, "foo "),
            chunk(Some(ChunkType::Comment), 0, "/* hi */\n/* ho */"),
            chunk(Some(ChunkType::Label), 0, "label:"),
            chunk(None, 0, "bar"),
            chunk(Some(ChunkType::Comment), 0, "// comment"),
            chunk(None, 0, "\n"),
            chunk(None, 0, "\n"),
            chunk(None, 0, "\n"),
            chunk(Some(ChunkType::Label), 4, "label:"),
            chunk(None, 4, "baz"),
        ];
        assert_eq!(
            join_chunks(chunks, &FormattingOptions::default()),
            "                    // hello\n                    foo /* hi */\n                    /* ho */ label: bar           // comment\n\n                 label: baz"
        );
    }

    fn chunk<S: Into<String>>(ty: Option<ChunkType>, indent: usize, str: S) -> Chunk {
        Chunk {
            ty,
            indent,
            str: str.into(),
        }
    }

    fn get_source(src: &str) -> Arc<Mutex<dyn ParsingSource>> {
        InMemoryParsingSource::new()
            .add("test.asm", src)
            .add("other.asm", "{nop}")
            .into()
    }
}
