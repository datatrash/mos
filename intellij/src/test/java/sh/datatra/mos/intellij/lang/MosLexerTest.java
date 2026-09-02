package sh.datatra.mos.intellij.lang;

import com.intellij.lexer.Lexer;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class MosLexerTest {
    @Test
    void distinguishesCommonMosConstructsWithoutBadCharacters() {
        List<IElementType> types = lex("""
                .const border = $d020
                .var frame = 0
                .macro clear() {
                }
                start: lda #$00
                    sta border,x // color register
                    custom_macro(border)
                """);

        assertTrue(types.contains(MosTokenTypes.DIRECTIVE));
        assertTrue(types.contains(MosTokenTypes.CONSTANT_DEFINITION));
        assertTrue(types.contains(MosTokenTypes.VARIABLE_DEFINITION));
        assertTrue(types.contains(MosTokenTypes.MACRO_DEFINITION));
        assertTrue(types.contains(MosTokenTypes.LABEL));
        assertTrue(types.contains(MosTokenTypes.MNEMONIC));
        assertTrue(types.contains(MosTokenTypes.NUMBER));
        assertTrue(types.contains(MosTokenTypes.REGISTER));
        assertTrue(types.contains(MosTokenTypes.FUNCTION_CALL));
        assertTrue(types.contains(MosTokenTypes.OPERATOR));
        assertTrue(types.contains(MosTokenTypes.PUNCTUATION));
        assertTrue(types.contains(MosTokenTypes.COMMENT));
        assertFalse(types.contains(TokenType.BAD_CHARACTER));

        MosSyntaxHighlighter highlighter = new MosSyntaxHighlighter();
        assertNotEquals(
                highlighter.getTokenHighlights(MosTokenTypes.CONSTANT_DEFINITION)[0],
                highlighter.getTokenHighlights(MosTokenTypes.VARIABLE_DEFINITION)[0]
        );
        assertNotEquals(
                highlighter.getTokenHighlights(MosTokenTypes.CONSTANT_DEFINITION)[0],
                highlighter.getTokenHighlights(MosTokenTypes.IDENTIFIER)[0]
        );
    }

    /**
     * Member access must not swallow the dot into the identifier token. If it does, the PSI leaf for
     * "xscroll" starts on the dot and the IDE asks the language server about that offset, which
     * resolves to the enclosing scope ("vic") and underlines ".xscroll".
     */
    @Test
    void memberAccessKeepsTheDotSeparateFromTheIdentifier() {
        String source = "sta vic.xscroll";

        assertEquals(
                List.of("sta", " ", "vic", ".", "xscroll"),
                lexText(source)
        );
    }

    @Test
    void leadingDotStillFormsADirectiveToken() {
        assertEquals(List.of(".const", " ", "border"), lexText(".const border"));
        assertEquals(List.of(".byte", " ", "1"), lexText(".byte 1"));
    }

    private static List<String> lexText(String source) {
        Lexer lexer = new MosLexer();
        lexer.start(source);
        List<String> result = new ArrayList<>();
        while (lexer.getTokenType() != null) {
            result.add(source.substring(lexer.getTokenStart(), lexer.getTokenEnd()));
            lexer.advance();
        }
        return result;
    }

    private static List<IElementType> lex(String source) {
        Lexer lexer = new MosLexer();
        lexer.start(source);
        List<IElementType> result = new ArrayList<>();
        while (lexer.getTokenType() != null) {
            result.add(lexer.getTokenType());
            lexer.advance();
        }
        return result;
    }
}
