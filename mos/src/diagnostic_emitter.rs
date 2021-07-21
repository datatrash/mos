use crate::{Args, ErrorStyle};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};
use codespan_reporting::term::{Config, DisplayStyle};
use mos_core::errors::Diagnostics;
use mos_core::parser::code_map::CodeMap;
use std::io;

pub type MosResult<T> = anyhow::Result<T>;

pub struct DiagnosticEmitter {
    writer: Box<dyn WriteColor>,
    config: Config,
}

impl DiagnosticEmitter {
    pub fn stdout(args: &Args) -> Self {
        let color_choice = if args.no_color {
            ColorChoice::Never
        } else {
            ColorChoice::Auto
        };

        let display_style = match args.error_style {
            ErrorStyle::Short => DisplayStyle::Short,
            ErrorStyle::Medium => DisplayStyle::Medium,
            ErrorStyle::Rich => DisplayStyle::Rich,
        };

        let config = Config {
            display_style,
            ..Default::default()
        };

        Self {
            writer: Box::new(StandardStream::stdout(color_choice)),
            config,
        }
    }

    pub fn emit(&mut self, error: anyhow::Error) {
        match error.downcast_ref::<Diagnostics>() {
            Some(d) => {
                self.emit_diagnostics(d);
            }
            None => {
                self.emit_diagnostics(&Diagnostic::error().with_message(error.to_string()).into());
            }
        }
    }

    pub fn emit_diagnostics(&mut self, diagnostics: &Diagnostics) {
        let dummy_code_map = CodeMap::default();
        let code_map = diagnostics.code_map().unwrap_or(&dummy_code_map);
        for diag in diagnostics.iter() {
            // Go over all labels and fix up the range, which needs to be the bytes inside the source file to extract
            let labels = diag
                .labels
                .iter()
                .map(|label| {
                    let sl = code_map.look_up_span(label.file_id);
                    let range = ((label.file_id.low() - sl.file.span.low()) as usize)
                        ..((label.file_id.high() - sl.file.span.low()) as usize);

                    Label {
                        style: label.style,
                        file_id: label.file_id,
                        range,
                        message: label.message.clone(),
                    }
                })
                .collect();
            let mut diag = diag.clone();
            diag.labels = labels;

            codespan_reporting::term::emit(&mut self.writer, &self.config, &code_map, &diag)
                .unwrap();
        }
    }
}

#[derive(Default)]
struct TestLogStream {
    bytes: Vec<u8>,
}

impl io::Write for TestLogStream {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.bytes.extend(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl WriteColor for TestLogStream {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _: &ColorSpec) -> io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::diagnostic_emitter::{DiagnosticEmitter, TestLogStream};
    use mos_core::codegen::{codegen, CodegenContext, CodegenOptions};
    use mos_core::errors::CoreResult;
    use mos_core::parser::parse_or_err;
    use mos_core::parser::source::{InMemoryParsingSource, ParsingSource};
    use std::path::Path;
    use std::sync::{Arc, Mutex};

    #[test]
    fn error_unknown_identifiers() {
        let src = InMemoryParsingSource::new()
            .add("test.asm", r#".import * from "wrong.asm""#)
            .add("wrong.asm", ".byte foo")
            .into();
        let err = test_codegen_parsing_source(src, CodegenOptions::default())
            .err()
            .unwrap();
        assert_eq!(
            err.to_string(),
            "wrong.asm:1:7: error: unknown identifier: foo"
        );
        let mut emitter = DiagnosticEmitter {
            writer: Box::new(TestLogStream::default()),
            config: Default::default(),
        };
        emitter.emit_diagnostics(&err);
    }

    pub(super) fn test_codegen_parsing_source(
        src: Arc<Mutex<dyn ParsingSource>>,
        options: CodegenOptions,
    ) -> CoreResult<CodegenContext> {
        let ast = parse_or_err(&Path::new("test.asm"), src)?;
        let (ctx, err) = codegen(ast, options);
        if err.is_empty() {
            Ok(ctx.unwrap())
        } else {
            Err(err)
        }
    }
}
