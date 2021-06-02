use crate::codegen::CodegenContext;
use crate::errors::CoreResult;
use crate::LINE_ENDING;
use itertools::Itertools;
use std::collections::HashMap;
use std::path::PathBuf;

pub fn to_listing(ctx: &CodegenContext) -> CoreResult<HashMap<PathBuf, String>> {
    let mut listing = HashMap::new();

    for file in ctx.tree().code_map.files() {
        let mut result = vec![];
        for line_idx in 0..file.num_lines() {
            let mut line = vec![];
            line.push(format!("{:>5}", line_idx + 1));

            let offsets = ctx.source_map().line_col_to_offsets(
                &ctx.tree().code_map,
                file.name(),
                line_idx,
                None,
            );

            if offsets.is_empty() {
                // No bytes were emitted for this source line
                line.push(format!("{:<17}", ""));
            } else {
                for offset in offsets {
                    let mut data = vec![];
                    for segment in ctx.segments().values() {
                        if segment.range().start <= offset.pc.start
                            && segment.range().end >= offset.pc.end
                        {
                            let start = offset.pc.start - segment.range().start;
                            let end = start + (offset.pc.end - offset.pc.start);
                            data = segment.range_data()[start..end].to_vec();
                            break;
                        }
                    }

                    if !data.is_empty() {
                        line.push(format!("{:04X}:", offset.pc.start));

                        let bytes = data
                            .into_iter()
                            .map(|b| format!("{:02X}", b))
                            .collect_vec()
                            .join(" ");
                        line.push(format!("{:<11}", bytes));
                    }
                }
            }

            line.push(file.source_line(line_idx).to_string());
            result.push(line.join(" "));
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
    use crate::codegen::tests::test_codegen;

    #[test]
    fn create_listing() -> CoreResult<()> {
        let ctx = test_codegen(
            r"lda $d020
// hello
jmp foo
foo: nop",
        )?;
        assert_eq!(
            to_listing(&ctx)?.get(&PathBuf::from("test.asm")).unwrap(),
            &r"    1 C000: AD 20 D0    lda $d020
    2                   // hello
    3 C003: 4C 06 C0    jmp foo
    4 C006: EA          foo: nop"
                .replace("\n", LINE_ENDING)
        );

        Ok(())
    }
}
