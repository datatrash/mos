package sh.datatra.mos.intellij.lang;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.openapi.fileTypes.FileTypeRegistry;
import com.intellij.testFramework.LightPlatformTestCase;
import com.intellij.testFramework.LightVirtualFile;

public final class MosParserDefinitionTest extends LightPlatformTestCase {
    public void testSymbolPsiElementUsesTokenRange() {
        String source = "target: nop\n    jmp target\n";
        PsiFile file = PsiFileFactory.getInstance(getProject())
                .createFileFromText("main.asm", MosFileType.INSTANCE, source);

        PsiElement symbol = file.findElementAt(source.lastIndexOf("target"));

        assertNotNull(symbol);
        assertEquals("target", symbol.getText());
        assertTrue(symbol.getTextRange().getLength() < file.getTextLength());
    }

    public void testAsmNavigationKeepsMosFileType() {
        LightVirtualFile target = new LightVirtualFile("shared.asm");

        assertSame(
                MosFileType.INSTANCE,
                FileTypeRegistry.getInstance().getFileTypeByFile(target)
        );
    }

}
