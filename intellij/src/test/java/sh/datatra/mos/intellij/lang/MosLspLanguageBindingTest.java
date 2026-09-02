package sh.datatra.mos.intellij.lang;

import com.intellij.lang.Language;
import com.intellij.lang.LanguageParserDefinitions;
import com.intellij.psi.FileViewProviderFactory;
import com.intellij.psi.LanguageFileViewProviders;
import com.intellij.testFramework.LightPlatformTestCase;
import com.redhat.devtools.lsp4ij.LanguageServersRegistry;
import com.redhat.devtools.lsp4ij.features.semanticTokens.viewProvider.LSPSemanticTokensFileViewProviderFactory;

/**
 * LSP4IJ binds most of its language features to the "TEXT" and "textmate" languages only, so a plugin
 * that contributes its own {@code Language} has to declare them for that language explicitly.
 *
 * <p>The semantic tokens file view provider is the one that matters for Ctrl+hover: without it
 * {@code LSPTargetElementEvaluator} falls back to a synthetic {@code LSPPsiElement}, which drops the
 * reference underline and renders a generic element description in the Ctrl+hover popup.
 */
public final class MosLspLanguageBindingTest extends LightPlatformTestCase {
    public void testSemanticTokensFileViewProviderIsBoundToMosLanguage() {
        FileViewProviderFactory factory =
                LanguageFileViewProviders.INSTANCE.forLanguage(MosLanguage.INSTANCE);

        assertNotNull(
                "MOS must declare lang.fileViewProviderFactory so LSP4IJ can resolve Ctrl+hover targets",
                factory
        );
        assertTrue(
                "Expected LSP4IJ's semantic tokens view provider but got " + factory.getClass().getName(),
                factory instanceof LSPSemanticTokensFileViewProviderFactory
        );
    }

    public void testMosLanguageKeepsItsOwnParserDefinition() {
        assertTrue(
                LanguageParserDefinitions.INSTANCE.forLanguage(MosLanguage.INSTANCE)
                        instanceof MosParserDefinition
        );
    }

    /**
     * LSP4IJ silently ignores a {@code languageMapping} whose language ID cannot be resolved, which
     * would leave MOS files without any language server association.
     */
    public void testMosLanguageIsAssociatedWithTheMosServer() {
        LanguageServersRegistry registry = LanguageServersRegistry.getInstance();

        assertTrue(
                "MOS server definition must be registered",
                registry.getServerDefinitions().stream().anyMatch(d -> "mos".equals(d.getId()))
        );
        assertTrue(
                "MOS6502 must be associated with a language server; supported="
                        + registry.getSupportedLanguages().stream().map(Language::getID).sorted().toList(),
                registry.getSupportedLanguages().contains(MosLanguage.INSTANCE)
        );
    }
}
