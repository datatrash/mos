package sh.datatra.mos.intellij.lang;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;

final class MosFile extends PsiFileBase {
    MosFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, MosLanguage.INSTANCE);
    }

    @Override
    public @NotNull FileType getFileType() {
        return MosFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "MOS Assembly";
    }
}
