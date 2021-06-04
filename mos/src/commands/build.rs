use crate::config::Config;
use crate::diagnostic_emitter::MosResult;
use clap::App;
use fs_err as fs;
use mos_core::codegen::{codegen, CodegenOptions};
use mos_core::errors::map_io_error;
use mos_core::io::{to_listing, to_vice_symbols, SegmentMerger};
use mos_core::parser;
use mos_core::parser::source::FileSystemParsingSource;
use serde::Deserialize;
use std::io::Write;
use std::path::{Path, PathBuf};
use strum::EnumString;

#[derive(Debug, Clone, Deserialize, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
pub struct BuildOptions {
    pub entry: String,
    pub target_directory: String,
    pub listing: bool,
    pub symbols: Vec<SymbolType>,
    pub output_format: OutputFormat,
    pub output_filename: Option<String>,
}

impl Default for BuildOptions {
    fn default() -> Self {
        Self {
            entry: "main.asm".into(),
            target_directory: "target".into(),
            listing: false,
            symbols: vec![],
            output_format: OutputFormat::Prg,
            output_filename: None,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, EnumString)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub enum OutputFormat {
    Prg,
    Bin,
    BinSegments,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, EnumString)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub enum SymbolType {
    Vice,
}

pub fn build_app() -> App<'static> {
    App::new("build").about("Assembles input file(s)")
}

pub fn build_command(root: &Path, cfg: &Config) -> MosResult<()> {
    let target_dir = root.join(&cfg.build.target_directory);
    fs::create_dir_all(&target_dir)?;

    let input_path = root.join(PathBuf::from(&cfg.build.entry));

    let source = FileSystemParsingSource::new();
    let (tree, error) = parser::parse(&input_path, source.into());
    if !error.is_empty() {
        return Err(error.into());
    }
    let tree = tree.unwrap();
    let (generated_code, error) = codegen(
        tree,
        CodegenOptions {
            pc: 0x2000.into(),
            move_macro_source_map_to_invocation: cfg.build.listing,
            ..Default::default()
        },
    );
    if !error.is_empty() {
        return Err(error.into());
    }
    let generated_code = generated_code.unwrap();

    let ext = match cfg.build.output_format {
        OutputFormat::Prg => "prg",
        OutputFormat::Bin | OutputFormat::BinSegments => "bin",
    };

    // Do we want to ignore the segment filename and just group everything together into one file?
    let merge_to_single_segment = cfg.build.output_format == OutputFormat::Prg
        || cfg.build.output_format == OutputFormat::Bin;

    let filename = match cfg.build.output_filename.as_ref() {
        Some(f) => f.clone(),
        None => format!(
            "{}.{}",
            input_path.file_stem().unwrap().to_string_lossy(),
            ext
        ),
    };
    let output_path = target_dir.join(filename);

    let mut merger = SegmentMerger::new(output_path);
    for (segment_name, segment) in generated_code.segments() {
        if segment.options().write {
            merger.merge(segment_name, segment, merge_to_single_segment)?;
        }
    }

    if merger.has_errors() {
        return Err(merger.errors().into());
    }

    for (path, m) in merger.targets() {
        log::trace!(
            "Writing: (${:04x} - ${:04x})",
            m.range().start,
            m.range().end
        );
        log::trace!("Writing: {:?}", m.range_data());
        let mut out = fs::File::create(target_dir.join(path)).map_err(map_io_error)?;
        if cfg.build.output_format == OutputFormat::Prg {
            out.write_all(&(m.range().start as u16).to_le_bytes())
                .map_err(map_io_error)?;
        }
        out.write_all(&m.range_data()).map_err(map_io_error)?;
    }

    if cfg.build.listing {
        for (source_path, contents) in
            to_listing(&generated_code, cfg.formatting.listing.num_bytes_per_line)?
        {
            let listing_path =
                format!("{}.lst", source_path.file_stem().unwrap().to_string_lossy());
            let mut out = fs::File::create(target_dir.join(listing_path)).map_err(map_io_error)?;
            out.write_all(contents.as_bytes()).map_err(map_io_error)?;
        }
    }

    for symbol_type in &cfg.build.symbols {
        match symbol_type {
            SymbolType::Vice => {
                let symbol_path =
                    format!("{}.vs", input_path.file_stem().unwrap().to_string_lossy());
                let mut out =
                    fs::File::create(target_dir.join(symbol_path)).map_err(map_io_error)?;
                out.write_all(to_vice_symbols(generated_code.symbols()).as_bytes())
                    .map_err(map_io_error)?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::commands::{build_command, BuildOptions, OutputFormat, SymbolType};
    use crate::config::Config;
    use anyhow::Result;
    use fs_err::File;
    use itertools::Itertools;
    use std::ffi::OsStr;
    use std::io::Read;
    use std::path::PathBuf;

    #[test]
    fn can_invoke_default_build() -> Result<()> {
        let entry = test_cli_build().join("valid.asm");
        let mut cfg = config(entry);
        cfg.build.listing = true;
        cfg.build.symbols = vec![SymbolType::Vice];
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target().join("valid.prg"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes);

        let vs_bytes = std::fs::read_to_string(target().join("valid.vs"))?;
        let vs_lines = vs_bytes.lines().collect_vec();
        assert_eq!(vs_lines, vec!["al C:2007 .data"]);

        let lst_bytes = std::fs::read_to_string(target().join("valid.lst"))?;
        assert!(!lst_bytes.is_empty());

        Ok(())
    }

    #[test]
    fn build_multiple_segments() -> Result<()> {
        build_and_compare("multiple_segments.asm")
    }

    #[test]
    fn can_invoke_prg_build_with_filename() -> Result<()> {
        let entry = test_cli_build().join("valid.asm");
        let mut cfg = config(entry);
        cfg.build.output_format = OutputFormat::Prg;
        cfg.build.output_filename = Some("bob.foo".into());
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target().join("bob.foo"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes);

        Ok(())
    }

    #[test]
    fn can_invoke_bin_build() -> Result<()> {
        let entry = test_cli_build().join("valid.asm");
        let mut cfg = config(entry);
        cfg.build.output_format = OutputFormat::Bin;
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target().join("valid.bin"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes[2..]);

        Ok(())
    }

    #[test]
    fn can_invoke_bin_segments_build() -> Result<()> {
        let entry = test_cli_build().join("multiple_segments.asm");
        let mut cfg = config(entry);
        cfg.build.output_format = OutputFormat::BinSegments;
        build_command(root().as_path(), &cfg)?;

        assert!(target().join("a.bin").exists());
        assert!(target().join("bc.seg").exists());

        // 'bc.seg' should contain merged segments 'b' and 'c' containing NOP and ASL
        let mut buffer = vec![];
        File::open(target().join("bc.seg"))
            .unwrap()
            .read_to_end(&mut buffer)
            .unwrap();
        assert_eq!(buffer, vec![0xea, 0x0a]);

        Ok(())
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

    fn config(entry: PathBuf) -> Config {
        Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target().to_string_lossy().into(),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    fn test_cli_build() -> PathBuf {
        root()
            .join(PathBuf::from("mos"))
            .join(PathBuf::from("test-data").join(PathBuf::from("build")))
    }
}
