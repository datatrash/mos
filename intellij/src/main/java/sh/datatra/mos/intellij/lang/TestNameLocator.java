package sh.datatra.mos.intellij.lang;

import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import org.jetbrains.annotations.Nullable;

/** Resolves the name of a {@code .test} block from the flat token stream following the header. */
final class TestNameLocator {
    private TestNameLocator() {
    }

    /**
     * Returns the test name for a {@code .test} token, i.e. the content of the adjacent string
     * literal {@code "name"}, or {@code null} if the header is malformed.
     */
    @Nullable
    static String find(PsiElement testDirective) {
        PsiElement sibling = testDirective.getNextSibling();
        while (sibling != null && sibling.getNode() != null
                && sibling.getNode().getElementType() == TokenType.WHITE_SPACE) {
            sibling = sibling.getNextSibling();
        }
        if (sibling == null || sibling.getNode() == null
                || sibling.getNode().getElementType() != MosTokenTypes.STRING) {
            return null;
        }
        String text = sibling.getText();
        if (text.length() >= 2 && text.startsWith("\"") && text.endsWith("\"")) {
            return text.substring(1, text.length() - 1).trim();
        }
        return text.trim();
    }
}