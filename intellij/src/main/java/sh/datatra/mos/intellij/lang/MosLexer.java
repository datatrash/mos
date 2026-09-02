package sh.datatra.mos.intellij.lang;

import com.intellij.lexer.LexerBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Locale;
import java.util.Set;

final class MosLexer extends LexerBase {
    private static final Set<String> DIRECTIVES = Set.of(
            "align", "assert", "byte", "const", "define", "dword", "file", "if", "import",
            "loop", "macro", "segment", "test", "text", "trace", "var", "word"
    );
    private static final Set<String> KEYWORDS = Set.of("as", "else", "from");
    private static final Set<String> ENCODINGS = Set.of("ascii", "petscii", "petscreen");
    private static final Set<String> BOOLEAN_LITERALS = Set.of("true", "false");
    private static final Set<String> MNEMONICS = Set.of(
            "adc", "and", "asl", "bcc", "bcs", "beq", "bit", "bmi", "bne", "bpl", "brk",
            "bvc", "bvs", "clc", "cld", "cli", "clv", "cmp", "cpx", "cpy", "dec", "dex",
            "dey", "eor", "inc", "inx", "iny", "jmp", "jsr", "lda", "ldx", "ldy", "lsr",
            "nop", "ora", "pha", "php", "pla", "plp", "rol", "ror", "rti", "rts", "sbc",
            "sec", "sed", "sei", "sta", "stx", "sty", "tax", "tay", "tsx", "txa", "txs", "tya"
    );

    private CharSequence buffer = "";
    private int endOffset;
    private int tokenStart;
    private int tokenEnd;
    private IElementType tokenType;

    @Override
    public void start(
            @NotNull CharSequence buffer,
            int startOffset,
            int endOffset,
            int initialState
    ) {
        this.buffer = buffer;
        this.endOffset = endOffset;
        tokenStart = startOffset;
        locateToken();
    }

    @Override
    public int getState() {
        return 0;
    }

    @Override
    public @Nullable IElementType getTokenType() {
        return tokenType;
    }

    @Override
    public int getTokenStart() {
        return tokenStart;
    }

    @Override
    public int getTokenEnd() {
        return tokenEnd;
    }

    @Override
    public void advance() {
        tokenStart = tokenEnd;
        locateToken();
    }

    @Override
    public @NotNull CharSequence getBufferSequence() {
        return buffer;
    }

    @Override
    public int getBufferEnd() {
        return endOffset;
    }

    private void locateToken() {
        if (tokenStart >= endOffset) {
            tokenEnd = tokenStart;
            tokenType = null;
            return;
        }

        char current = buffer.charAt(tokenStart);
        if (Character.isWhitespace(current)) {
            tokenEnd = consumeWhile(tokenStart + 1, Character::isWhitespace);
            tokenType = TokenType.WHITE_SPACE;
            return;
        }
        if (startsWith(tokenStart, "//")) {
            tokenEnd = tokenStart + 2;
            while (tokenEnd < endOffset && buffer.charAt(tokenEnd) != '\n') {
                tokenEnd++;
            }
            tokenType = MosTokenTypes.COMMENT;
            return;
        }
        if (startsWith(tokenStart, "/*")) {
            tokenEnd = consumeBlockComment();
            tokenType = MosTokenTypes.COMMENT;
            return;
        }
        if (current == '"') {
            tokenEnd = consumeString();
            tokenType = MosTokenTypes.STRING;
            return;
        }

        int numberEnd = consumeNumber(tokenStart);
        if (numberEnd > tokenStart) {
            tokenEnd = numberEnd;
            tokenType = MosTokenTypes.NUMBER;
            return;
        }

        // A leading '.' only belongs to the token when it introduces a directive such as ".const".
        // In member access like "vic.xscroll" the dot is a separator: swallowing it into the
        // identifier would shift the token start onto the dot, which makes the IDE ask the language
        // server about the wrong offset (resolving "vic" instead of "xscroll").
        if (current == '.' && tokenStart + 1 < endOffset && isIdentifierStart(buffer.charAt(tokenStart + 1))) {
            boolean memberAccess = tokenStart > 0 && isIdentifierPart(buffer.charAt(tokenStart - 1));
            if (!memberAccess) {
                int wordEnd = consumeIdentifier(tokenStart + 1);
                String directive = buffer.subSequence(tokenStart + 1, wordEnd)
                        .toString()
                        .toLowerCase(Locale.ROOT);
                if (directive.equals("test")) {
                    tokenEnd = wordEnd;
                    tokenType = MosTokenTypes.TEST_DEFINITION;
                    return;
                }
                if (DIRECTIVES.contains(directive)) {
                    tokenEnd = wordEnd;
                    tokenType = MosTokenTypes.DIRECTIVE;
                    return;
                }
            }
            tokenEnd = tokenStart + 1;
            tokenType = MosTokenTypes.PUNCTUATION;
            return;
        }

        if (isIdentifierStart(current)) {
            tokenEnd = consumeIdentifier(tokenStart);
            String word = buffer.subSequence(tokenStart, tokenEnd).toString().toLowerCase(Locale.ROOT);
            int lookahead = consumeWhile(tokenEnd, Character::isWhitespace);
            IElementType definitionType = definitionTokenType(tokenStart);
            if (lookahead < endOffset && buffer.charAt(lookahead) == ':') {
                tokenType = MosTokenTypes.LABEL;
            } else if (definitionType != null) {
                tokenType = definitionType;
            } else if (MNEMONICS.contains(word)) {
                tokenType = MosTokenTypes.MNEMONIC;
            } else if (KEYWORDS.contains(word)) {
                tokenType = MosTokenTypes.KEYWORD;
            } else if (BOOLEAN_LITERALS.contains(word)) {
                tokenType = MosTokenTypes.KEYWORD;
            } else if (ENCODINGS.contains(word)) {
                tokenType = MosTokenTypes.ENCODING;
            } else if (lookahead < endOffset && buffer.charAt(lookahead) == '(') {
                tokenType = MosTokenTypes.FUNCTION_CALL;
            } else if (isRegister(word, tokenStart)) {
                tokenType = MosTokenTypes.REGISTER;
            } else {
                tokenType = MosTokenTypes.IDENTIFIER;
            }
            return;
        }

        tokenEnd = tokenStart + 1;
        tokenType = isOperator(current) ? MosTokenTypes.OPERATOR : MosTokenTypes.PUNCTUATION;
    }

    private int consumeBlockComment() {
        int offset = tokenStart + 2;
        int depth = 1;
        while (offset < endOffset && depth > 0) {
            if (startsWith(offset, "/*")) {
                depth++;
                offset += 2;
            } else if (startsWith(offset, "*/")) {
                depth--;
                offset += 2;
            } else {
                offset++;
            }
        }
        return offset;
    }

    private int consumeString() {
        int offset = tokenStart + 1;
        boolean escaped = false;
        while (offset < endOffset) {
            char current = buffer.charAt(offset++);
            if (!escaped && current == '"') {
                break;
            }
            escaped = !escaped && current == '\\';
        }
        return offset;
    }

    private int consumeNumber(int start) {
        int offset = start;
        if (buffer.charAt(offset) == '#') {
            offset++;
            if (offset >= endOffset) {
                return start;
            }
        }
        if (buffer.charAt(offset) == '$') {
            int digits = consumeWhile(offset + 1, MosLexer::isHexDigit);
            return digits > offset + 1 ? digits : start;
        }
        if (buffer.charAt(offset) == '%') {
            int digits = consumeWhile(offset + 1, value -> value == '0' || value == '1');
            return digits > offset + 1 ? digits : start;
        }
        if (Character.isDigit(buffer.charAt(offset))) {
            return consumeWhile(offset + 1, Character::isDigit);
        }
        return start;
    }

    private int consumeIdentifier(int start) {
        return consumeWhile(start + 1, MosLexer::isIdentifierPart);
    }

    private IElementType definitionTokenType(int start) {
        int offset = start - 1;
        while (offset >= 0 && isHorizontalWhitespace(buffer.charAt(offset))) {
            offset--;
        }
        int end = offset + 1;
        while (offset >= 0 && (isIdentifierPart(buffer.charAt(offset)) || buffer.charAt(offset) == '.')) {
            offset--;
        }
        while (offset >= 0 && isHorizontalWhitespace(buffer.charAt(offset))) {
            offset--;
        }
        if (offset >= 0 && buffer.charAt(offset) != '\n' && buffer.charAt(offset) != '\r') {
            return null;
        }
        String previous = buffer.subSequence(offset + 1, end).toString().toLowerCase(Locale.ROOT);
        return switch (previous) {
            case ".const", ".define" -> MosTokenTypes.CONSTANT_DEFINITION;
            case ".var" -> MosTokenTypes.VARIABLE_DEFINITION;
            case ".macro" -> MosTokenTypes.MACRO_DEFINITION;
            case ".segment", ".test" -> MosTokenTypes.SYMBOL_DEFINITION;
            default -> null;
        };
    }

    private boolean isRegister(String word, int start) {
        if (!word.equals("a") && !word.equals("x") && !word.equals("y")) {
            return false;
        }
        int offset = start - 1;
        while (offset >= 0 && isHorizontalWhitespace(buffer.charAt(offset))) {
            offset--;
        }
        return word.equals("a") || offset >= 0 && buffer.charAt(offset) == ',';
    }

    private int consumeWhile(int start, CharacterPredicate predicate) {
        int offset = start;
        while (offset < endOffset && predicate.test(buffer.charAt(offset))) {
            offset++;
        }
        return offset;
    }

    private boolean startsWith(int offset, String value) {
        if (offset + value.length() > endOffset) {
            return false;
        }
        for (int index = 0; index < value.length(); index++) {
            if (buffer.charAt(offset + index) != value.charAt(index)) {
                return false;
            }
        }
        return true;
    }

    private static boolean isIdentifierStart(char value) {
        return value == '_' || Character.isLetter(value);
    }

    private static boolean isIdentifierPart(char value) {
        return isIdentifierStart(value) || Character.isDigit(value);
    }

    private static boolean isHexDigit(char value) {
        return Character.digit(value, 16) >= 0;
    }

    private static boolean isHorizontalWhitespace(char value) {
        return value == ' ' || value == '\t' || value == '\f';
    }

    private static boolean isOperator(char value) {
        return "+-*/=<>!&|^~#%".indexOf(value) >= 0;
    }

    @FunctionalInterface
    private interface CharacterPredicate {
        boolean test(char value);
    }
}
