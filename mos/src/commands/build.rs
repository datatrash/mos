use crate::config::Config;
use crate::diagnostic_emitter::MosResult;
use clap::App;
use codespan_reporting::diagnostic::Diagnostic;
use fs_err as fs;
use mos_core::codegen::{codegen, CodegenOptions};
use mos_core::errors::map_io_error;
use mos_core::errors::Diagnostics;
use mos_core::io::{to_listing, to_vice_symbols, Bank, BinaryWriter};
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
    pub output_format: Option<OutputFormat>,
    pub output_filename: Option<String>,
}

impl Default for BuildOptions {
    fn default() -> Self {
        Self {
            entry: "main.asm".into(),
            target_directory: "target".into(),
            listing: false,
            symbols: vec![],
            output_format: None,
            output_filename: None,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, EnumString)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub enum OutputFormat {
    Prg,
    Bin,
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

    let output_format = cfg.build.output_format.unwrap_or_else(|| {
        if generated_code.banks().len() == 1 {
            OutputFormat::Prg
        } else {
            OutputFormat::Bin
        }
    });
    if output_format == OutputFormat::Prg && generated_code.banks().len() != 1 {
        return Err(Diagnostics::from(Diagnostic::error().with_message(
                r#"A program with 'output-format = "prg" must contain a single bank only."#,
            ).with_notes(vec![
                "Tip: If you do not define one, a default bank will be automatically generated.".into()
            ]))
            .into());
    }

    let ext = match output_format {
        OutputFormat::Prg => "prg",
        OutputFormat::Bin => "bin",
    };

    let filename = match cfg.build.output_filename.as_ref() {
        Some(f) => f.clone(),
        None => format!(
            "{}.{}",
            input_path.file_stem().unwrap().to_string_lossy(),
            ext
        ),
    };

    let mut bw = BinaryWriter {};
    let mut banks = bw.merge_segments(&generated_code)?;

    if output_format == OutputFormat::Prg {
        // Add the two-byte PRG header as a first bank
        let mut new_banks = vec![Bank::prg_header(banks[0].range().start)];
        new_banks.extend(banks);
        banks = new_banks;
    }

    bw.write_banks(banks, &target_dir, &filename)?;

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
    use itertools::Itertools;
    use std::ffi::OsStr;
    use std::path::{Path, PathBuf};
    use tempfile::tempdir;

    #[test]
    fn can_invoke_default_build() -> Result<()> {
        let target = tempdir().unwrap();
        let entry = test_cli_build().join("valid.asm");
        let mut cfg = config(entry, target.path());
        cfg.build.listing = true;
        cfg.build.symbols = vec![SymbolType::Vice];
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target.path().join("valid.prg"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes);

        let vs_bytes = std::fs::read_to_string(target.path().join("valid.vs"))?;
        let vs_lines = vs_bytes.lines().collect_vec();
        assert_eq!(vs_lines, vec!["al C:2007 .data"]);

        let lst_bytes = std::fs::read_to_string(target.path().join("valid.lst"))?;
        assert!(!lst_bytes.is_empty());

        Ok(())
    }

    #[test]
    fn build_multiple_segments() -> Result<()> {
        build_and_compare("multiple_segments.asm")
    }

    #[test]
    fn can_invoke_prg_build_with_filename() -> Result<()> {
        let target = tempdir().unwrap();
        let entry = test_cli_build().join("valid.asm");
        let mut cfg = config(entry, target.path());
        cfg.build.output_format = Some(OutputFormat::Prg);
        cfg.build.output_filename = Some("bob.foo".into());
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target.path().join("bob.foo"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes);

        Ok(())
    }

    #[test]
    fn can_invoke_bin_build() -> Result<()> {
        let target = tempdir().unwrap();
        let entry = test_cli_build().join("valid.asm");
        let mut cfg = config(entry, target.path());
        cfg.build.output_format = Some(OutputFormat::Bin);
        build_command(root().as_path(), &cfg)?;

        let out_bytes = std::fs::read(target.path().join("valid.bin"))?;
        let prg_bytes = std::fs::read(test_cli_build().join("valid.prg"))?;
        assert_eq!(out_bytes, prg_bytes[2..]);

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
        let target = tempdir().unwrap();
        let entry = test_cli_build().join(input);

        let cfg = Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target.path().to_string_lossy().into(),
                ..Default::default()
            },
            ..Default::default()
        };
        build_command(root().as_path(), &cfg)?;

        let actual_path = target
            .path()
            .join(PathBuf::from(input).with_extension("prg"));
        let actual_bytes = std::fs::read(actual_path)?;
        let expected_prg_path = PathBuf::from(entry).with_extension("prg").into_os_string();
        let expected_prg_bytes = std::fs::read(expected_prg_path)?;
        assert_eq!(actual_bytes, expected_prg_bytes);

        Ok(())
    }

    fn root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..")
    }

    fn config(entry: PathBuf, target: &Path) -> Config {
        Config {
            build: BuildOptions {
                entry: entry.clone().to_string_lossy().into(),
                target_directory: target.to_string_lossy().into(),
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
