package sh.datatra.mos.intellij.lang;

import com.intellij.psi.tree.IElementType;

final class MosTokenTypes {
    static final IElementType COMMENT = token("COMMENT");
    static final IElementType STRING = token("STRING");
    static final IElementType LABEL = token("LABEL");
    static final IElementType SYMBOL_DEFINITION = token("SYMBOL_DEFINITION");
    static final IElementType CONSTANT_DEFINITION = token("CONSTANT_DEFINITION");
    static final IElementType VARIABLE_DEFINITION = token("VARIABLE_DEFINITION");
    static final IElementType MACRO_DEFINITION = token("MACRO_DEFINITION");
    static final IElementType DIRECTIVE = token("DIRECTIVE");
    static final IElementType KEYWORD = token("KEYWORD");
    static final IElementType ENCODING = token("ENCODING");
    static final IElementType MNEMONIC = token("MNEMONIC");
    static final IElementType FUNCTION_CALL = token("FUNCTION_CALL");
    static final IElementType REGISTER = token("REGISTER");
    static final IElementType NUMBER = token("NUMBER");
    static final IElementType IDENTIFIER = token("IDENTIFIER");
    static final IElementType OPERATOR = token("OPERATOR");
    static final IElementType PUNCTUATION = token("PUNCTUATION");

    private MosTokenTypes() {
    }

    private static IElementType token(String name) {
        return new IElementType(name, MosLanguage.INSTANCE);
    }
}
