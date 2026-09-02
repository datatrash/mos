package sh.datatra.mos.intellij.codelens;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

final class MosApplicationCodeVisionProviderTest {
    @Test
    void runAndDebugUseDistinctProviderIds() {
        assertNotEquals(
                new MosRunApplicationCodeVisionProvider().getId(),
                new MosDebugApplicationCodeVisionProvider().getId()
        );
    }
}
