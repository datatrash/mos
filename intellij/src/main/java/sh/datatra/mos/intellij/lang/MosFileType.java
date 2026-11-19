package sh.datatra.mos.intellij.lang;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.Icon;

public final class MosFileType extends LanguageFileType {
    public static final MosFileType INSTANCE = new MosFileType();

    private MosFileType() {
        super(MosLanguage.INSTANCE);
    }

    /** Persisted identifier for user file type associations; must match plugin.xml and never change. */
    @Override
    public @NotNull String getName() {
        return "MOS 6502 Assembly";
    }

    @Override
    public @Nls @NotNull String getDisplayName() {
        return "MOS Assembly";
    }

    @Override
    public @NotNull String getDescription() {
        return "MOS assembly source";
    }

    @Override
    public @NotNull String getDefaultExtension() {
        return "asm";
    }

    @Override
    public @Nullable Icon getIcon() {
        return MosIcons.FILE;
    }
}
