use std::io::{Read, Write};
use std::path::PathBuf;

use clap::{App, Arg, ArgMatches};
use fs_err as fs;

use crate::core::codegen::{codegen, CodegenOptions};
use crate::core::parser;
use crate::errors::MosResult;

pub fn build_app() -> App<'static> {
    App::new("build")
        .about("Build")
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
}

pub fn build_command(args: &ArgMatches) -> MosResult<()> {
    let input_names = args.values_of("input").unwrap().collect::<Vec<_>>();
    let target_dir = PathBuf::from(args.value_of("target-dir").unwrap());

    for input_name in input_names {
        let input_path = PathBuf::from(input_name);
        let output_path = PathBuf::from(format!(
            "{}.prg",
            input_path.file_stem().unwrap().to_string_lossy()
        ));

        let mut file = fs::File::open(input_path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;

        let ast = parser::parse(input_name, source.as_str())?;

        let generated_code = codegen(ast, CodegenOptions { pc: 0x2000.into() })?;
        let segment = generated_code.segments().current();
        let mut out = fs::File::create(target_dir.join(output_path))?;
        if let Some(range) = segment.range() {
            let start = range.start;
            out.write_all(&start.to_le_bytes())?;
            out.write_all(&segment.range_data())?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::commands::{build_app, build_command};
    use anyhow::Result;

    #[test]
    fn can_invoke_build() -> Result<()> {
        let root = env!("CARGO_MANIFEST_DIR");
        let input = &format!("{}/test/cli/build/valid.asm", root);

        let args = build_app().get_matches_from(vec![
            "build",
            input,
            "--target-dir",
            &format!("{}/target", root),
        ]);
        build_command(&args)?;

        let out_path = &format!("{}/target/valid.prg", root);
        let out_bytes = std::fs::read(out_path)?;
        let prg_path = &format!("{}/test/cli/build/valid.prg", root);
        let prg_bytes = std::fs::read(prg_path)?;
        assert_eq!(out_bytes, prg_bytes);

        Ok(())
    }
}
