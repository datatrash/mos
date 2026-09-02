package sh.datatra.mos.intellij.lang;

import com.intellij.lang.Language;

public final class MosLanguage extends Language {
    public static final MosLanguage INSTANCE = new MosLanguage();

    private MosLanguage() {
        super("MOS6502");
    }
}
