use crate::parser::code_map::{CodeMap, Span, SpanLoc};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::files::{Error, Files};
use itertools::Itertools;
use pathdiff::diff_paths;
use std::fmt::{Display, Formatter};
use std::ops::Range;

pub type CoreResult<T> = Result<T, Diagnostics>;

#[derive(Debug, Default)]
pub struct Diagnostics {
    diags: Vec<Diagnostic<Span>>,
    code_map: Option<CodeMap>,
}

impl PartialEq for Diagnostics {
    fn eq(&self, other: &Self) -> bool {
        self.diags == other.diags
    }
}

impl std::error::Error for Diagnostics {}

impl Diagnostics {
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic<Span>> {
        self.diags.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.diags.is_empty()
    }

    pub fn len(&self) -> usize {
        self.diags.len()
    }

    pub fn first(&self) -> Option<&Diagnostic<Span>> {
        self.diags.first()
    }

    pub fn push(&mut self, diag: Diagnostic<Span>) {
        self.diags.push(diag);
    }

    pub fn extend(&mut self, other: Diagnostics) {
        self.diags.extend(other.diags);
    }

    pub fn code_map(&self) -> Option<&CodeMap> {
        self.code_map.as_ref()
    }

    pub fn with_code_map(mut self, code_map: &CodeMap) -> Self {
        self.code_map = Some(code_map.clone());
        self
    }

    pub fn location(&self) -> Option<SpanLoc> {
        self.code_map.as_ref().and_then(|cm| {
            self.diags.first().and_then(|diag| {
                diag.labels
                    .first()
                    .map(|label| cm.look_up_span(label.file_id))
            })
        })
    }

    fn format(&self) -> String {
        self.diags
            .iter()
            .sorted_by_key(|s| s.labels.first().map(|l| l.file_id))
            .map(|diag| {
                let mut msg = "".to_string();
                if let Some(cm) = &self.code_map {
                    if let Some(label) = diag.labels.first() {
                        let sl = cm.look_up_span(label.file_id);
                        msg += format!(
                            "{}:{}:{}: ",
                            sl.file.name(),
                            sl.begin.line + 1,
                            sl.begin.column + 1
                        )
                        .as_str();
                    }
                }
                let severity = match diag.severity {
                    Severity::Error => "error",
                    _ => unimplemented!(),
                };
                msg += format!("{}: ", severity).as_str();
                msg += diag.message.as_str();
                msg
            })
            .collect_vec()
            .join("\n")
    }
}

impl Display for Diagnostics {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

impl From<Vec<Diagnostic<Span>>> for Diagnostics {
    fn from(diags: Vec<Diagnostic<Span>>) -> Self {
        Diagnostics {
            diags,
            code_map: None,
        }
    }
}

impl From<Diagnostic<Span>> for Diagnostics {
    fn from(diag: Diagnostic<Span>) -> Self {
        Diagnostics {
            diags: vec![diag],
            code_map: None,
        }
    }
}

pub fn map_io_error(e: std::io::Error) -> Diagnostics {
    Diagnostic::error().with_message(e.to_string()).into()
}

impl<'a> Files<'a> for &'a CodeMap {
    type FileId = Span;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, file: Span) -> Result<Self::Name, Error> {
        let mut filename = self.find_file(file.low()).name().to_string();
        if let Some(relative_from) = &self.paths_relative_from {
            if let Some(path) = diff_paths(&filename, relative_from) {
                filename = path.to_str().unwrap().into();
            }
        }
        Ok(filename)
    }

    fn source(&'a self, file: Span) -> Result<Self::Source, Error> {
        Ok(self.find_file(file.low()).source())
    }

    fn line_index(&'a self, file: Span, byte_in_file: usize) -> Result<usize, Error> {
        let file = self.look_up_span(file).file;
        let pos = file.span.low() + byte_in_file as u64;
        Ok(file.find_line(pos))
    }

    fn line_range(&'a self, file: Span, line_index: usize) -> Result<Range<usize>, Error> {
        let file = self.look_up_span(file).file;
        let line_span = file.line_span(line_index);
        let low = line_span.low().as_usize() - file.span.low().as_usize();
        let high = line_span.high().as_usize() - file.span.low().as_usize();
        Ok(low..high)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::code_map::CodeMap;
    use codespan_reporting::files::Files;

    #[test]
    fn files() {
        let mut codemap = CodeMap::default();
        let a = codemap.add_file("a".into(), "1234567890\nabcde".into());
        let b = codemap.add_file("b".into(), "ABCDE\nFGHIJKLMNO".into());

        let cm = &codemap;
        assert_eq!(cm.line_index(a.span, 5).unwrap(), 0);
        assert_eq!(cm.line_index(a.span, 15).unwrap(), 1);
        assert_eq!(cm.line_index(b.span, 3).unwrap(), 0);
        assert_eq!(cm.line_index(b.span, 8).unwrap(), 1);

        assert_eq!(cm.line_range(a.span, 0).unwrap(), 0..11);
        assert_eq!(cm.line_range(a.span, 1).unwrap(), 11..16);
        assert_eq!(cm.line_range(b.span, 0).unwrap(), 0..6);
    }
}
