package sh.datatra.mos.intellij.lang;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.Icon;
import java.util.Map;

public final class MosColorSettingsPage implements ColorSettingsPage {
    private static final AttributesDescriptor[] DESCRIPTORS = {
            new AttributesDescriptor("Comment", MosSyntaxHighlighter.COMMENT),
            new AttributesDescriptor("String", MosSyntaxHighlighter.STRING),
            new AttributesDescriptor("Label", MosSyntaxHighlighter.LABEL),
            new AttributesDescriptor("Namespace definition", MosSyntaxHighlighter.SYMBOL_DEFINITION),
            new AttributesDescriptor("Constant definition", MosSyntaxHighlighter.CONSTANT_DEFINITION),
            new AttributesDescriptor("Variable definition", MosSyntaxHighlighter.VARIABLE_DEFINITION),
            new AttributesDescriptor("Macro definition", MosSyntaxHighlighter.MACRO_DEFINITION),
            new AttributesDescriptor("Directive", MosSyntaxHighlighter.DIRECTIVE),
            new AttributesDescriptor("Keyword", MosSyntaxHighlighter.KEYWORD),
            new AttributesDescriptor("Text encoding", MosSyntaxHighlighter.ENCODING),
            new AttributesDescriptor("Mnemonic", MosSyntaxHighlighter.MNEMONIC),
            new AttributesDescriptor("Function or macro call", MosSyntaxHighlighter.FUNCTION_CALL),
            new AttributesDescriptor("Register", MosSyntaxHighlighter.REGISTER),
            new AttributesDescriptor("Number", MosSyntaxHighlighter.NUMBER),
            new AttributesDescriptor("Symbol reference", MosSyntaxHighlighter.IDENTIFIER),
            new AttributesDescriptor("Operator", MosSyntaxHighlighter.OPERATOR),
            new AttributesDescriptor("Punctuation", MosSyntaxHighlighter.PUNCTUATION)
    };

    @Override
    public @Nullable Icon getIcon() {
        return MosIcons.FILE;
    }

    @Override
    public @NotNull SyntaxHighlighter getHighlighter() {
        return new MosSyntaxHighlighter();
    }

    @Override
    public @NotNull String getDemoText() {
        return """
                // MOS 6502 assembly
                .const border = $d020
                .var frame = 0
                .macro clear() {
                    lda #0
                }
                .segment Code
                * = $0801
                basic_start(main)

                main:
                    lda #$00
                    sta border
                    rts
                """;
    }

    @Override
    public AttributesDescriptor @NotNull [] getAttributeDescriptors() {
        return DESCRIPTORS;
    }

    @Override
    public ColorDescriptor @NotNull [] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @Override
    public @NotNull String getDisplayName() {
        return "MOS";
    }

    @Override
    public @Nullable Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return null;
    }
}
