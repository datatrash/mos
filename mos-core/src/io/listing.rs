use crate::codegen::CodegenContext;
use crate::errors::CoreResult;
use crate::LINE_ENDING;
use itertools::Itertools;
use std::collections::HashMap;
use std::path::PathBuf;

pub fn to_listing(
    ctx: &CodegenContext,
    num_bytes_per_line: usize,
) -> CoreResult<HashMap<PathBuf, String>> {
    let mut listing = HashMap::new();

    for file in ctx.tree().code_map.files() {
        let mut result = vec![];
        for line_idx in 0..file.num_lines() {
            let offsets = ctx.source_map().line_col_to_offsets(
                &ctx.tree().code_map,
                file.name(),
                line_idx,
                None,
            );

            let mut data = vec![];
            for offset in &offsets {
                for segment in ctx.segments().values() {
                    if segment.range().start <= offset.pc.start
                        && segment.range().end >= offset.pc.end
                    {
                        let mut start = offset.pc.start - segment.range().start;
                        let end = start + (offset.pc.end - offset.pc.start);

                        let mut pc = offset.pc.start;
                        while start < end {
                            data.push((pc, segment.range_data()[start]));
                            start += 1;
                            pc += 1;
                        }
                        break;
                    }
                }
            }

            if data.is_empty() {
                let mut line = vec![];
                line.push(format!("{:>5}", line_idx + 1));
                line.push(format!("{:5}", ""));
                line.push(format!("{:width$}", "", width = num_bytes_per_line * 3));
                line.push(file.source_line(line_idx).to_string());
                result.push(line.join(" "));
            } else {
                let mut source_line_emitted = false;

                let chunks = data.chunks(num_bytes_per_line);
                for chunk in chunks {
                    let pc = chunk.iter().next().unwrap().0;
                    let bytes = chunk.iter().map(|(_, bytes)| bytes).collect_vec();
                    let formatted_bytes = bytes.into_iter().map(|b| format!("{:02X}", b)).join(" ");

                    let mut line = vec![];
                    line.push(format!("{:>5}", line_idx + 1));
                    line.push(format!("{:04X}:", pc));
                    line.push(format!(
                        "{:width$}",
                        formatted_bytes,
                        width = num_bytes_per_line * 3
                    ));
                    if !source_line_emitted {
                        line.push(file.source_line(line_idx).to_string());
                        source_line_emitted = true;
                    }
                    result.push(line.join(" ").trim_end().into());
                }
            }
        }

        let result = result.join(LINE_ENDING);
        let result = result.trim_end().into();
        listing.insert(PathBuf::from(file.name()), result);
    }

    Ok(listing)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::tests::{test_codegen, test_codegen_with_options};
    use crate::codegen::CodegenOptions;

    #[test]
    fn create_listing() -> CoreResult<()> {
        let ctx = test_codegen(
            r#"lda $d020
// hello
jmp foo
foo: nop
.byte 1,2,3,4,5,6,7,8,9,10
.text "hello there""#,
        )?;
        assert_eq!(
            to_listing(&ctx, 8)?
                .get(&PathBuf::from("test.asm"))
                .unwrap(),
            &r#"    1 C000: AD 20 D0                 lda $d020
    2                                // hello
    3 C003: 4C 06 C0                 jmp foo
    4 C006: EA                       foo: nop
    5 C007: 01 02 03 04 05 06 07 08  .byte 1,2,3,4,5,6,7,8,9,10
    5 C00F: 09 0A
    6 C011: 68 65 6C 6C 6F 20 74 68  .text "hello there"
    6 C019: 65 72 65"#
                .replace('\n', LINE_ENDING)
        );

        Ok(())
    }

    #[test]
    fn create_macro_listing() -> CoreResult<()> {
        let ctx = test_codegen_with_options(
            r#".macro outer() {
    inner()
    brk
}
.macro inner() {
    nop
}
outer()"#,
            CodegenOptions {
                move_macro_source_map_to_invocation: true,
                ..Default::default()
            },
        )?;
        assert_eq!(
            to_listing(&ctx, 8)?
                .get(&PathBuf::from("test.asm"))
                .unwrap(),
            &r#"    1                                .macro outer() {
    2                                    inner()
    3                                    brk
    4                                }
    5                                .macro inner() {
    6                                    nop
    7                                }
    8 C000: EA 00                    outer()"#
                .replace('\n', LINE_ENDING)
        );

        Ok(())
    }
}
