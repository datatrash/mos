use crate::diagnostic_emitter::MosResult;
use crate::impl_request_handler;
use crate::lsp::{LspContext, RequestHandler};
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
                let mut comments = vec![];
                let sl = analysis.look_up(location.span);
                let mut line = sl.begin.line;
                while line > 0 {
                    line -= 1;
                    let source_line = sl.file.source_line(line).trim();
                    if source_line.starts_with("///") {
                        let (_prefix, rest) = source_line.split_at(3);
                        comments.push(rest.trim());
                    } else {
                        break;
                    }
                }
                comments.reverse();

                return if comments.is_empty() {
                    Ok(None)
                } else {
                    let value = comments.join("\n");

                    Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value,
                        }),
                        range: None,
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
    use crate::lsp::testing::test_root;
    use crate::lsp::LspServer;
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
                range: None
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
