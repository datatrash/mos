package sh.datatra.mos.intellij.lang;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import java.util.Map;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;
import static java.util.Map.entry;

public final class MosSyntaxHighlighter extends SyntaxHighlighterBase {
    public static final TextAttributesKey COMMENT =
            createTextAttributesKey("MOS_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);
    public static final TextAttributesKey STRING =
            createTextAttributesKey("MOS_STRING", DefaultLanguageHighlighterColors.STRING);
    public static final TextAttributesKey LABEL =
            createTextAttributesKey("MOS_LABEL", DefaultLanguageHighlighterColors.FUNCTION_DECLARATION);
    public static final TextAttributesKey SYMBOL_DEFINITION =
            createTextAttributesKey("MOS_SYMBOL_DEFINITION", DefaultLanguageHighlighterColors.CLASS_NAME);
    public static final TextAttributesKey CONSTANT_DEFINITION =
            createTextAttributesKey("MOS_CONSTANT_DEFINITION", DefaultLanguageHighlighterColors.CONSTANT);
    public static final TextAttributesKey VARIABLE_DEFINITION =
            createTextAttributesKey("MOS_VARIABLE_DEFINITION", DefaultLanguageHighlighterColors.LOCAL_VARIABLE);
    public static final TextAttributesKey MACRO_DEFINITION =
            createTextAttributesKey("MOS_MACRO_DEFINITION", DefaultLanguageHighlighterColors.FUNCTION_DECLARATION);
    public static final TextAttributesKey DIRECTIVE =
            createTextAttributesKey("MOS_DIRECTIVE", DefaultLanguageHighlighterColors.KEYWORD);
    public static final TextAttributesKey KEYWORD =
            createTextAttributesKey("MOS_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);
    public static final TextAttributesKey ENCODING =
            createTextAttributesKey("MOS_ENCODING", DefaultLanguageHighlighterColors.CLASS_NAME);
    public static final TextAttributesKey MNEMONIC =
            createTextAttributesKey("MOS_MNEMONIC", DefaultLanguageHighlighterColors.KEYWORD);
    public static final TextAttributesKey FUNCTION_CALL =
            createTextAttributesKey("MOS_FUNCTION_CALL", DefaultLanguageHighlighterColors.FUNCTION_CALL);
    public static final TextAttributesKey REGISTER =
            createTextAttributesKey("MOS_REGISTER", DefaultLanguageHighlighterColors.PREDEFINED_SYMBOL);
    public static final TextAttributesKey NUMBER =
            createTextAttributesKey("MOS_NUMBER", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey IDENTIFIER =
            createTextAttributesKey("MOS_IDENTIFIER", DefaultLanguageHighlighterColors.GLOBAL_VARIABLE);
    public static final TextAttributesKey OPERATOR =
            createTextAttributesKey("MOS_OPERATOR", DefaultLanguageHighlighterColors.OPERATION_SIGN);
    public static final TextAttributesKey PUNCTUATION =
            createTextAttributesKey("MOS_PUNCTUATION", DefaultLanguageHighlighterColors.PARENTHESES);
    public static final TextAttributesKey BAD_CHARACTER =
            createTextAttributesKey("MOS_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER);

    private static final Map<IElementType, TextAttributesKey> ATTRIBUTES = Map.ofEntries(
            entry(MosTokenTypes.COMMENT, COMMENT),
            entry(MosTokenTypes.STRING, STRING),
            entry(MosTokenTypes.LABEL, LABEL),
            entry(MosTokenTypes.SYMBOL_DEFINITION, SYMBOL_DEFINITION),
            entry(MosTokenTypes.CONSTANT_DEFINITION, CONSTANT_DEFINITION),
            entry(MosTokenTypes.VARIABLE_DEFINITION, VARIABLE_DEFINITION),
            entry(MosTokenTypes.MACRO_DEFINITION, MACRO_DEFINITION),
            entry(MosTokenTypes.TEST_DEFINITION, DIRECTIVE),
            entry(MosTokenTypes.DIRECTIVE, DIRECTIVE),
            entry(MosTokenTypes.KEYWORD, KEYWORD),
            entry(MosTokenTypes.ENCODING, ENCODING),
            entry(MosTokenTypes.MNEMONIC, MNEMONIC),
            entry(MosTokenTypes.FUNCTION_CALL, FUNCTION_CALL),
            entry(MosTokenTypes.REGISTER, REGISTER),
            entry(MosTokenTypes.NUMBER, NUMBER),
            entry(MosTokenTypes.IDENTIFIER, IDENTIFIER),
            entry(MosTokenTypes.OPERATOR, OPERATOR),
            entry(MosTokenTypes.PUNCTUATION, PUNCTUATION),
            entry(TokenType.BAD_CHARACTER, BAD_CHARACTER)
    );

    @Override
    public @NotNull Lexer getHighlightingLexer() {
        return new MosLexer();
    }

    @Override
    public TextAttributesKey @NotNull [] getTokenHighlights(IElementType tokenType) {
        return pack(ATTRIBUTES.get(tokenType));
    }
}
