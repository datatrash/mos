package sh.datatra.mos.intellij.codelens;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class MosApplicationStartsTest {
    @Test
    void findsBasicStartLabelAndProgramCounters() {
        String source = """
                basic_start(entry_point)
                * = $0801
                entry_point: lda #0
                    * = $c000
                """;

        assertEquals(
                source.indexOf("* = $0801"),
                MosApplicationStarts.find(source).get(0).offset()
        );
        assertEquals(
                source.indexOf("entry_point:"),
                MosApplicationStarts.find(source).get(1).offset()
        );
        assertEquals(3, MosApplicationStarts.find(source).size());
    }

    @Test
    void ignoresMarkersInStringsAndNestedComments() {
        String source = """
                // basic_start(fake)
                /* outer /* inner */ * = $c000 */
                .text ascii "basic_start(fake) * = 1"
                """;

        assertTrue(MosApplicationStarts.find(source).isEmpty());
    }

    @Test
    void parsesBuildEntryAndDefaultsToMain() {
        assertEquals(
                "src/app.asm",
                MosApplicationStarts.parseBuildEntry("[build]\nentry = 'src/app.asm'")
        );
        assertEquals(
                "src/app.asm",
                MosApplicationStarts.parseBuildEntry("build . entry = \"src/app.asm\"")
        );
        assertEquals("main.asm", MosApplicationStarts.parseBuildEntry("[build]\nlisting = true"));
    }
}
