use crate::codegen::{CodegenContext, Segment};
use crate::errors::{map_io_error, CoreResult};
use crate::parser::Identifier;
use codespan_reporting::diagnostic::Diagnostic;
use fs_err as fs;
use indexmap::map::{Entry, IndexMap};
use itertools::Itertools;
use std::cmp::{max, min, Ordering};
use std::collections::HashMap;
use std::io::Write;
use std::ops::Range;
use std::path::Path;

pub struct Bank {
    range: Range<usize>,
    data: Vec<u8>,
    options: BankOptions,
}

#[derive(Clone)]
pub struct BankOptions {
    pub size: Option<usize>,
    pub fill: Option<u8>,
    pub filename: Option<String>,
}

impl Default for BankOptions {
    fn default() -> Self {
        Self {
            size: None,
            fill: None,
            filename: None,
        }
    }
}

impl Bank {
    pub fn new(options: BankOptions) -> Self {
        Self {
            range: 0..0,
            data: vec![],
            options,
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn merge(&mut self, segment: &Segment) {
        self.range = if self.range.is_empty() {
            let new_range = segment.range();
            self.data = vec![self.options.fill.unwrap_or_default(); new_range.len()];
            new_range
        } else {
            let new_range = Range {
                start: min(self.range.start, segment.range().start),
                end: max(self.range.end, segment.range().end),
            };

            if new_range.start < self.range.start {
                let mut data =
                    vec![self.options.fill.unwrap_or_default(); self.range.start - new_range.start];
                data.extend(&self.data);
                self.data = data;
            }
            if new_range.end > self.range.end {
                self.data.extend(vec![
                    self.options.fill.unwrap_or_default();
                    new_range.end - self.range.end
                ]);
            }

            new_range
        };

        let mut seg_rng = segment.range();
        seg_rng.start -= self.range.start;
        seg_rng.end -= self.range.start;
        self.data[seg_rng].copy_from_slice(segment.range_data());
    }

    pub fn prg_header(pc: usize) -> Bank {
        debug_assert!(pc < 65536);
        Bank {
            range: 0..2,
            data: vec![(pc & 255) as u8, ((pc >> 8) & 255) as u8],
            options: Default::default(),
        }
    }
}

pub struct BinaryWriter;

impl BinaryWriter {
    pub fn merge_segments(&mut self, ctx: &CodegenContext) -> CoreResult<Vec<Bank>> {
        let mut errors = vec![];

        // Check if all segments are assigned to a valid bank
        for (segment_name, segment) in ctx.segments() {
            let bank_name = segment.options().bank_or_default();
            if !ctx.banks().contains_key(&bank_name) {
                errors.push(Diagnostic::error().with_message(format!(
                    "segment '{}' is assigned to bank '{}' but this bank does not exist",
                    segment_name, bank_name,
                )));
            }
        }

        let mut banks: IndexMap<Identifier, Bank> = IndexMap::new();
        for (bank_name, bank_options) in ctx.banks() {
            let segments: Vec<(&Identifier, &Segment)> = ctx
                .segments()
                .iter()
                .filter(|(_, segment)| {
                    &segment.options().bank_or_default() == bank_name && segment.options().write
                })
                .collect();
            log::trace!(
                "Bank '{}' consists of segments: {}",
                bank_name,
                segments.iter().map(|(name, _)| name).join(", ")
            );

            let bank = match banks.entry(bank_name.clone()) {
                Entry::Occupied(o) => o.into_mut(),
                Entry::Vacant(e) => e.insert(Bank {
                    range: Default::default(),
                    data: vec![],
                    options: bank_options.clone(),
                }),
            };
            for (_segment_name, segment) in segments {
                bank.merge(&segment);
            }
            if let Some(size) = bank_options.size {
                match bank.data.len().cmp(&size) {
                    Ordering::Less => match bank_options.fill {
                        Some(fill) => {
                            bank.range.end += size - bank.data.len();
                            bank.data.extend(vec![fill; size - bank.data.len()])
                        }
                        None => {
                            errors.push(Diagnostic::error()
                                    .with_message(format!("bank '{}' should have be {} bytes but only {} byte(s) were written. \
                                No fill value was specified to pad it.", bank_name, size, bank.data.len())));
                        }
                    },
                    Ordering::Greater => {
                        errors.push(Diagnostic::error().with_message(format!(
                            "bank '{}' exceeds maximum size of {} bytes: {} bytes were written.",
                            bank_name,
                            size,
                            bank.data.len()
                        )));
                    }
                    Ordering::Equal => (),
                }
            }

            log::trace!(
                "Bank '{}' range: ${:04X} - ${:04X}",
                bank_name,
                bank.range().start,
                bank.range().end
            );
        }

        if errors.is_empty() {
            Ok(banks.into_iter().map(|(_, bank)| bank).collect())
        } else {
            Err(errors.into())
        }
    }

    pub fn write_banks(
        &mut self,
        banks: Vec<Bank>,
        target_dir: &Path,
        filename: &str,
    ) -> CoreResult<()> {
        let mut files = HashMap::new();
        for bank in banks {
            let filename = bank.options.filename.as_deref().unwrap_or(filename);
            let path = target_dir.join(filename);
            let file = match files.entry(path.clone()) {
                std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
                std::collections::hash_map::Entry::Vacant(e) => {
                    let file = fs::File::create(&path).map_err(map_io_error)?;
                    e.insert(file)
                }
            };

            log::trace!(
                "Writing: (${:04x} - ${:04x}) to {:?}",
                bank.range().start,
                bank.range().end,
                path
            );
            file.write_all(&bank.data).map_err(map_io_error)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::tests::test_codegen;
    use crate::codegen::SegmentOptions;
    use crate::errors::CoreResult;
    use tempfile::tempdir;

    #[test]
    fn can_merge_banks() {
        let mut seg1 = Segment::new(SegmentOptions {
            bank: Some("bank".into()),
            initial_pc: 0x2000.into(),
            ..Default::default()
        });
        let mut seg2 = Segment::new(SegmentOptions {
            bank: Some("bank".into()),
            initial_pc: 0x4000.into(),
            ..Default::default()
        });
        seg1.emit(&[1, 2, 3, 4]);
        seg2.emit(&[5, 6, 7]);

        let mut bank = Bank::new(BankOptions {
            fill: Some(123),
            ..Default::default()
        });
        bank.merge(&seg1);
        bank.merge(&seg2);
        assert_eq!(bank.range, 0x2000..0x4003);

        let mut expected = vec![1, 2, 3, 4];
        expected.extend(vec![123; 0x2000 - 4]);
        expected.extend(vec![5, 6, 7]);
        assert_eq!(bank.data, expected);
    }

    #[test]
    fn merge_segments() -> CoreResult<()> {
        let ctx = test_codegen(SOURCE)?;
        let mut bw = BinaryWriter {};
        let result = bw.merge_segments(&ctx)?;
        assert_eq!(result[0].range, 0x0000..0x0040);
        assert_eq!(result[0].data, *vec(&[1, 2, 3, 4]).and(&[0; 60]));

        assert_eq!(result[1].range, 0x2000..0x4001);
        assert_eq!(result[1].data, *vec(&[0xea]).and(&[0; 0x1fff]).and(&[0x0a]));

        assert_eq!(result[2].range, 0x0000..0x0003);
        assert_eq!(result[2].data, *vec(&[5, 6, 7]));

        Ok(())
    }

    #[test]
    fn write_banks() -> CoreResult<()> {
        let target = tempdir().unwrap();
        let ctx = test_codegen(SOURCE)?;
        let mut bw = BinaryWriter {};
        let banks = bw.merge_segments(&ctx)?;
        bw.write_banks(banks, target.path(), "out.bin")?;

        let out_bytes = std::fs::read(target.path().join("out.bin")).map_err(map_io_error)?;
        assert_eq!(
            out_bytes,
            *vec(&[1, 2, 3, 4])
                .and(&[0; 60])
                .and(&[0xea])
                .and(&[0; 0x1fff])
                .and(&[0x0a])
                .and(&[5, 6, 7])
        );

        Ok(())
    }

    #[test]
    fn write_banks_to_multiple_files() -> CoreResult<()> {
        let target = tempdir().unwrap();
        let ctx = test_codegen(
            r#"
        .define bank    { name = "bank1" }
        .define bank    { name = "bank2"      filename = "foo.bin" }
        .define segment { name = "a"          bank = "bank1" }
        .define segment { name = "b"          bank = "bank2" }
        
        .segment "a" { .byte 1, 2, 3, 4 }
        .segment "b" { .byte 5, 6, 7 }
        }
        "#,
        )?;
        let mut bw = BinaryWriter {};
        let banks = bw.merge_segments(&ctx)?;
        bw.write_banks(banks, target.path(), "out.bin")?;

        assert_eq!(
            std::fs::read(target.path().join("out.bin")).map_err(map_io_error)?,
            &[1, 2, 3, 4]
        );
        assert_eq!(
            std::fs::read(target.path().join("foo.bin")).map_err(map_io_error)?,
            &[5, 6, 7]
        );

        Ok(())
    }

    #[test]
    fn respect_write_flag() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"
        .define segment { name = "a" }
        .define segment { name = "b" write = false }
        
        .segment "a" { .byte 1, 2, 3, 4 }
        .segment "b" { .byte 5, 6, 7 }
        }
        "#,
        )?;
        let mut bw = BinaryWriter {};
        let banks = bw.merge_segments(&ctx)?;

        assert_eq!(&banks.first().unwrap().data, &[1, 2, 3, 4]);

        Ok(())
    }

    #[test]
    fn respect_max_size() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"
        .define bank { name = "bank" size = 4 }
        .define segment { name = "a" bank = "bank" }
        
        .segment "a" { .byte 1, 2, 3, 4, 5 }
        "#,
        )?;
        let mut bw = BinaryWriter {};
        let err = bw.merge_segments(&ctx).err().unwrap();

        assert_eq!(
            err.to_string(),
            "error: bank \'bank\' exceeds maximum size of 4 bytes: 5 bytes were written."
        );

        Ok(())
    }

    struct VecBuilder<T> {
        data: Vec<T>,
    }

    impl<T> std::ops::Deref for VecBuilder<T> {
        type Target = Vec<T>;

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    impl<T: Clone> VecBuilder<T> {
        fn and(mut self, data: &[T]) -> Self {
            self.data.extend(data.to_vec());
            self
        }
    }

    fn vec<T: Clone>(data: &[T]) -> VecBuilder<T> {
        VecBuilder {
            data: data.to_vec(),
        }
    }

    // This will first write header, then default (consisting of default_lo and default_hi), then header2
    const SOURCE: &str = r#"
        .define bank    { name = "header"         size = 64               fill = 0 }
        .define bank    { name = "default" }
        .define bank    { name = "header2" }
        .define segment { name = "header"         bank = "header" }
        .define segment { name = "header2"        bank = "header2" }
        .define segment { name = "default_lo"     bank = "default"          start = $2000 }
        .define segment { name = "default_hi"     bank = "default"          start = $4000 }

        .segment "header" { .byte 1, 2, 3, 4 }
        .segment "header2" { .byte 5, 6, 7 }
        .segment "default_lo" { nop }
        .segment "default_hi" { asl }
        }
        "#;
}
