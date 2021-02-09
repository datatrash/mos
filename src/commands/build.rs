use std::cmp::{max, min};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::ops::Range;
use std::path::PathBuf;
use std::str::FromStr;

use clap::{App, Arg, ArgMatches};
use fs_err as fs;
use itertools::Itertools;

use crate::core::codegen::{codegen, CodegenOptions, Segment};
use crate::core::parser;
use crate::errors::{MosError, MosResult};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum SymbolType {
    Vice,
}

impl FromStr for SymbolType {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "vice" => Ok(SymbolType::Vice),
            _ => Err("no match"),
        }
    }
}

pub fn build_app() -> App<'static> {
    App::new("build")
        .about("Assembles input file(s)")
        .arg(
            Arg::new("input")
                .about("Sets the input file to use")
                .required(true)
                .multiple(true),
        )
        .arg(
            Arg::new("target-dir")
                .about("Directory for generated files")
                .long("target-dir")
                .default_value("."),
        )
        .arg(
            Arg::new("symbols")
                .about("Generate symbols")
                .case_insensitive(true)
                .long("symbols")
                .possible_values(&["vice"]),
        )
}

struct MergingSegment<'a> {
    data: [u8; 65536],
    range: Option<Range<u16>>,
    sources: HashMap<&'a str, &'a Segment>,
}

impl<'a> MergingSegment<'a> {
    fn merge(&mut self, segment_name: &'a str, segment: &'a Segment) {
        let sr = segment.range().as_ref().unwrap();
        let sr_usize = self.range_usize(sr);
        self.sources.insert(segment_name, segment);
        self.data[sr_usize].copy_from_slice(segment.range_data());

        match &mut self.range {
            Some(br) => {
                br.start = min(br.start, sr.start);
                br.end = max(br.end, sr.end);
            }
            None => self.range = Some(sr.start..sr.end),
        }
    }

    fn range_usize(&self, range: &Range<u16>) -> Range<usize> {
        Range {
            start: range.start as usize,
            end: range.end as usize,
        }
    }

    fn overlaps_with_sources(&self, new_range: &Range<u16>) -> Vec<(&&'a str, &&'a Segment)> {
        self.sources
            .iter()
            .filter_map(|(segment_name, segment)| {
                let sr = segment.range().as_ref().unwrap();
                if (new_range.start >= sr.start && new_range.start <= sr.end)
                    || (new_range.end >= sr.start && new_range.end <= sr.end)
                {
                    Some((segment_name, segment))
                } else {
                    None
                }
            })
            .collect()
    }
}

struct SegmentMerger<'a> {
    targets: HashMap<PathBuf, MergingSegment<'a>>,
    default_target: PathBuf,
    errors: Vec<MosError>,
}

impl<'a> SegmentMerger<'a> {
    fn new(default_target: PathBuf) -> Self {
        Self {
            targets: HashMap::new(),
            default_target,
            errors: vec![],
        }
    }

    fn merge(&mut self, segment_name: &'a str, segment: &'a Segment) -> MosResult<()> {
        if let Some(seg_range) = segment.range() {
            let target_name = &self.default_target;
            let target = match self.targets.entry(target_name.clone()) {
                Entry::Occupied(o) => o.into_mut(),
                Entry::Vacant(e) => e.insert(MergingSegment {
                    data: [0; 65536],
                    range: None,
                    sources: HashMap::new(),
                }),
            };

            let overlaps = target.overlaps_with_sources(&seg_range);
            if !overlaps.is_empty() {
                let overlaps = overlaps
                    .into_iter()
                    .map(|(name, segment)| {
                        let sr = segment.range().as_ref().unwrap();
                        format!("segment '{}' (${:04x} - ${:04x})", name, sr.start, sr.end)
                    })
                    .join(", ");
                self.errors.push(MosError::BuildError(format!(
                    "in target '{}': segment '{}' (${:04x} - ${:04x}) overlaps with: {}",
                    target_name.to_string_lossy(),
                    segment_name,
                    seg_range.start,
                    seg_range.end,
                    overlaps
                )));
            }

            target.merge(segment_name, segment);
        }

        Ok(())
    }
}

pub fn build_command(args: &ArgMatches) -> MosResult<()> {
    let input_names = args.values_of("input").unwrap().collect_vec();
    let target_dir = PathBuf::from(args.value_of("target-dir").unwrap());

    for input_name in input_names {
        let input_path = PathBuf::from(input_name);
        let output_path = PathBuf::from(format!(
            "{}.prg",
            input_path.file_stem().unwrap().to_string_lossy()
        ));
        let symbol_path = PathBuf::from(format!(
            "{}.vs",
            input_path.file_stem().unwrap().to_string_lossy()
        ));

        let mut file = fs::File::open(input_path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;

        let ast = parser::parse(input_name, source.as_str())?;

        let generated_code = codegen(
            ast,
            CodegenOptions {
                pc: 0x2000u16.into(),
            },
        )?;

        let mut merger = SegmentMerger::new(output_path);
        for segment_name in generated_code.segments().keys() {
            let segment = generated_code.segments().get(segment_name);
            if segment.options().write {
                merger.merge(segment_name, segment)?;
            }
        }

        if !merger.errors.is_empty() {
            return Err(MosError::Multiple(merger.errors));
        }

        for (path, m) in &merger.targets {
            if let Some(range) = &m.range {
                let mut out = fs::File::create(target_dir.join(path))?;
                out.write_all(&range.start.to_le_bytes())?;
                out.write_all(&m.data[m.range_usize(range)])?;
            }
        }

        if args
            .values_of_t::<SymbolType>("symbols")
            .unwrap_or_else(|_| vec![])
            .contains(&SymbolType::Vice)
        {
            let mut out = fs::File::create(target_dir.join(symbol_path))?;
            out.write_all(
                generated_code
                    .symbol_table()
                    .to_symbols(SymbolType::Vice)
                    .as_bytes(),
            )?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use itertools::Itertools;

    use crate::commands::{build_app, build_command};

    #[test]
    fn can_invoke_build() -> Result<()> {
        let root = env!("CARGO_MANIFEST_DIR");
        let input = &format!("{}/test/cli/build/valid.asm", root);

        let args = build_app().get_matches_from(vec![
            "build",
            input,
            "--target-dir",
            &format!("{}/target", root),
            "--symbols",
            "vice",
        ]);
        build_command(&args)?;

        let out_path = &format!("{}/target/valid.prg", root);
        let out_bytes = std::fs::read(out_path)?;
        let prg_path = &format!("{}/test/cli/build/valid.prg", root);
        let prg_bytes = std::fs::read(prg_path)?;
        assert_eq!(out_bytes, prg_bytes);

        let vs_path = &format!("{}/target/valid.vs", root);
        let vs_bytes = std::fs::read_to_string(vs_path)?;
        let vs_lines = vs_bytes.lines().collect_vec();
        assert_eq!(vs_lines, vec!["al C:2007 .data"]);

        Ok(())
    }
}
