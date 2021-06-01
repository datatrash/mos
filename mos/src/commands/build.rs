use clap::App;
use fs_err as fs;
use serde::Deserialize;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use crate::config::Config;
use crate::errors::MosResult;
use mos_core::codegen::{codegen, CodegenOptions};
use mos_core::errors::CoreError;
use mos_core::io::{to_vice_symbols, SegmentMerger};
use mos_core::parser;
use mos_core::parser::source::FileSystemParsingSource;

#[derive(Debug, Clone, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
pub struct BuildOptions {
    pub entry: String,
    pub target_directory: String,
    pub symbols: Vec<SymbolType>,
}

impl Default for BuildOptions {
    fn default() -> Self {
        Self {
            entry: "main.asm".into(),
            target_directory: "target".into(),
            symbols: vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
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
    App::new("build").about("Assembles input file(s)")
}

pub fn build_command(root: &Path, cfg: &Config) -> MosResult<()> {
    let target_dir = root.join(&cfg.build.target_directory);
    fs::create_dir_all(&target_dir)?;

    let input_path = root.join(PathBuf::from(&cfg.build.entry));
    let output_path = target_dir.join(format!(
        "{}.prg",
        input_path.file_stem().unwrap().to_string_lossy()
    ));

    let source = FileSystemParsingSource::new();
    let (tree, error) = parser::parse(&input_path, source.into());
    if let Some(e) = error {
        return Err(e.into());
    }
    let tree = tree.unwrap();
    let (generated_code, error) = codegen(
        tree,
        CodegenOptions {
            pc: 0x2000.into(),
            ..Default::default()
        },
    );
    if let Some(error) = error {
        return Err(error.into());
    }
    let generated_code = generated_code.unwrap();

    let mut merger = SegmentMerger::new(output_path);
    for (segment_name, segment) in generated_code.segments() {
        if segment.options().write {
            merger.merge(segment_name, segment)?;
        }
    }

    if merger.has_errors() {
        return Err(CoreError::Multiple(merger.errors()).into());
    }

    for (path, m) in merger.targets() {
        log::trace!(
            "Writing: (${:04x} - ${:04x})",
            m.range().start,
            m.range().end
        );
        log::trace!("Writing: {:?}", m.range_data());
        let mut out = fs::File::create(target_dir.join(path))?;
        out.write_all(&(m.range().start as u16).to_le_bytes())?;
        out.write_all(&m.range_data())?;
    }

    for symbol_type in &cfg.build.symbols {
        match symbol_type {
            SymbolType::Vice => {
                let symbol_path =
                    format!("{}.vs", input_path.file_stem().unwrap().to_string_lossy());
                let mut out = fs::File::create(target_dir.join(symbol_path))?;
                out.write_all(to_vice_symbols(generated_code.symbols()).as_bytes())?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::commands::{build_command, BuildOptions, SymbolType};
    use crate::config::Config;
    use anyhow::Result;
    use itertools::Itertools;
    use std::ffi::OsStr;
    use std::path::PathBuf;

    #[test]
    fn can_invoke_build() -> Result<()> {
        let entry = test_cli_build().join("valid.asm");
        let cfg = Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                symbols: vec![SymbolType::Vice],
            },
            ..Default::default()
        };
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target().join("valid.prg"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes);

        let vs_bytes = std::fs::read_to_string(target().join("valid.vs"))?;
        let vs_lines = vs_bytes.lines().collect_vec();
        assert_eq!(vs_lines, vec!["al C:2007 .data"]);

        Ok(())
    }

    #[test]
    fn build_multiple_segments() -> Result<()> {
        build_and_compare("multiple_segments.asm")
    }

    #[test]
    fn build_include() -> Result<()> {
        build_and_compare("include.asm")
    }

    #[test]
    fn build_all_examples() -> Result<()> {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("examples");
        build_all_examples_in(path)
    }

    fn build_all_examples_in(path: PathBuf) -> Result<()> {
        log::trace!("Enumerating: {:?}", &path);
        for item in std::fs::read_dir(&path)? {
            let item = item?;

            // Contains mos.toml?
            if item.path().is_file() && item.path().file_name() == Some(OsStr::new("mos.toml")) {
                // Yes, so build
                log::trace!("`--> Running a build in: {:?}", &path);
                let toml = std::fs::read_to_string(item.path())?;
                build_command(&path, &Config::from_toml(toml.as_str())?)?;
            }

            // Recurse further
            if item.path().is_dir()
                && !item
                    .path()
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .starts_with(".")
                && item.path().file_name() != Some(OsStr::new("target"))
            {
                build_all_examples_in(item.path())?;
            }
        }
        Ok(())
    }

    fn build_and_compare(input: &str) -> Result<()> {
        let entry = test_cli_build().join(input);

        let cfg = Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            ..Default::default()
        };
        build_command(root().as_path(), &cfg)?;

        let actual_path = target().join(PathBuf::from(input).with_extension("prg"));
        let actual_bytes = std::fs::read(actual_path)?;
        let expected_prg_path = PathBuf::from(entry).with_extension("prg").into_os_string();
        let expected_prg_bytes = std::fs::read(expected_prg_path)?;
        assert_eq!(actual_bytes, expected_prg_bytes);

        Ok(())
    }

    fn root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..")
    }

    fn target() -> PathBuf {
        root().join(PathBuf::from("target"))
    }

    fn test_cli_build() -> PathBuf {
        root()
            .join(PathBuf::from("mos"))
            .join(PathBuf::from("test-data").join(PathBuf::from("build")))
    }
}
