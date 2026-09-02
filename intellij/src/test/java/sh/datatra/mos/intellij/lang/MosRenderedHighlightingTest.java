package sh.datatra.mos.intellij.lang;

import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.editor.highlighter.EditorHighlighterFactory;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.testFramework.LightPlatformTestCase;
import com.intellij.testFramework.LightVirtualFile;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

public final class MosRenderedHighlightingTest extends LightPlatformTestCase {
    public void testRenderedDarkColorsAreDistinct() {
        String source = """
                // comment
                .const border = $d020
                .var frame = 0
                start: lda #border
                """;
        LightVirtualFile file = new LightVirtualFile("main.asm", MosFileType.INSTANCE, source);
        EditorHighlighter highlighter = EditorHighlighterFactory.getInstance()
                .createEditorHighlighter(getProject(), file);
        highlighter.setText(source);

        Map<String, Color> colors = collectColors(source, highlighter);
        assertNotNull(colors.get(".const"));
        assertNotNull(colors.get("border"));
        assertNotNull(colors.get("$d020"));
        assertNotNull(colors.get("lda"));
        assertFalse(colors.get(".const").equals(colors.get("$d020")));
        assertFalse(colors.get("lda").equals(colors.get("border")));
        assertFalse(colors.get("// comment").equals(colors.get("lda")));
    }

    private static Map<String, Color> collectColors(
            String source,
            EditorHighlighter highlighter
    ) {
        Map<String, Color> result = new HashMap<>();
        HighlighterIterator iterator = highlighter.createIterator(0);
        while (!iterator.atEnd()) {
            String token = source.substring(iterator.getStart(), iterator.getEnd());
            result.put(token, iterator.getTextAttributes().getForegroundColor());
            iterator.advance();
        }
        return result;
    }
}
