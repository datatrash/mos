use crate::diagnostic_emitter::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler, to_line_col, to_range, uri_to_path};
use lsp_types::request::HoverRequest;
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind};

pub struct HoverRequestHandler;

impl_request_handler!(HoverRequestHandler);

impl RequestHandler<HoverRequest> for HoverRequestHandler {
    fn handle(&self, ctx: &mut LspContext, params: HoverParams) -> MosResult<Option<Hover>> {
        if ctx.codegen.is_none() {
            return Ok(None);
        }
        let codegen = ctx.codegen().unwrap();
        let codegen = codegen.lock().unwrap();
        let analysis = codegen.analysis();

        let defs = ctx.find_definitions(analysis, &params.text_document_position_params);
        if let Some((_, def)) = defs.first() {
            if let Some(location) = def.location.as_ref() {
                let hover_path =
                    uri_to_path(&params.text_document_position_params.text_document.uri);
                let hover_position = to_line_col(&params.text_document_position_params.position);
                // The span being hovered over, so the client can highlight just that symbol.
                let hover_range = def
                    .definition_and_usages()
                    .into_iter()
                    .map(|location| analysis.look_up(location.span))
                    .find(|source| {
                        source.file.name() == hover_path.to_string_lossy()
                            && hover_position.line >= source.begin.line
                            && hover_position.line <= source.end.line
                            && hover_position.column >= source.begin.column
                            && hover_position.column <= source.end.column
                    })
                    .map(to_range);

                let mut comments = vec![];
                let sl = analysis.look_up(location.span);
                let mut line = sl.begin.line;
                while line > 0 {
                    line -= 1;
                    let source_line = sl.file.source_line(line).trim();
                    // Any comment style counts, not just `///`.
                    if source_line.starts_with("//") {
                        comments.push(source_line.trim_start_matches('/').trim());
                    } else {
                        break;
                    }
                }
                comments.reverse();

                // A trailing comment on the definition itself also documents it.
                let definition_line = sl.file.source_line(sl.begin.line);
                if let Some((_, comment)) = definition_line.split_once("//") {
                    let comment = comment.trim();
                    if !comment.is_empty() {
                        comments.push(comment);
                    }
                }

                return if comments.is_empty() {
                    Ok(None)
                } else {
                    let value = comments.join("\n");

                    Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value,
                        }),
                        range: hover_range,
                    }))
                };
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::LspServer;
    use crate::lsp::testing::test_root;
    use lsp_types::Position;

    #[test]
    fn get_hover_coments() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            "/// some other comment\n\n/// hello\n/// foo\nlabel: nop\n\nlda label",
        )?;
        let response = server.hover(test_root().join("main.asm"), Position::new(6, 4))?;
        assert_eq!(
            response,
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: "hello\nfoo".to_string()
                }),
                range: Some(lsp_types::Range::new(
                    Position::new(6, 4),
                    Position::new(6, 9)
                ))
            })
        );
        Ok(())
    }

    #[test]
    fn get_hover_comment_on_definition_line() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(
            test_root().join("main.asm"),
            ".const border = $d020 // Border color\nlda border",
        )?;
        let response = server.hover(test_root().join("main.asm"), Position::new(1, 4))?;
        assert_eq!(
            response,
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: "Border color".to_string()
                }),
                range: Some(lsp_types::Range::new(
                    Position::new(1, 4),
                    Position::new(1, 10)
                ))
            })
        );
        Ok(())
    }

    #[test]
    fn cannot_get_invalid_hover_coments() -> MosResult<()> {
        let mut server = LspServer::new(LspContext::new());
        server.did_open_text_document(test_root().join("main.asm"), "label: nop\n\nlda label")?;
        assert_eq!(
            server.hover(test_root().join("main.asm"), Position::new(2, 4))?,
            None
        );
        assert_eq!(
            server.hover(test_root().join("main.asm"), Position::new(0, 0))?,
            None
        );
        Ok(())
    }
}
