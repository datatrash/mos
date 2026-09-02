package sh.datatra.mos.intellij.lang;

import com.intellij.psi.tree.IFileElementType;

final class MosElementTypes {
    static final IFileElementType FILE = new IFileElementType(MosLanguage.INSTANCE);

    private MosElementTypes() {
    }
}
