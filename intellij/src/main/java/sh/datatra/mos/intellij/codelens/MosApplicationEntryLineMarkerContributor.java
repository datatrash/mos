package sh.datatra.mos.intellij.codelens;

import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.actions.MosRunConfigurations;
import sh.datatra.mos.intellij.settings.MosSettings;

/**
 * Renders a green run icon in the gutter on the MOS application build entry (the {@code basic_start}
 * entry label or the {@code * =} program-counter line), with a popup offering Run and Debug, matching
 * the run/debug gutter actions used for the {@code .test} blocks.
 */
public final class MosApplicationEntryLineMarkerContributor extends RunLineMarkerContributor {
    @Override
    public @Nullable Info getInfo(@NotNull PsiElement element) {
        return null;
    }

    @Override
    public @Nullable Info getSlowInfo(@NotNull PsiElement element) {
        return infoFor(element);
    }

    private @Nullable Info infoFor(@NotNull PsiElement element) {
        if (!MosSettings.getInstance().getState().applicationCodeLens) {
            return null;
        }
        PsiFile psiFile = element.getContainingFile();
        if (psiFile == null || !psiFile.isValid()) {
            return null;
        }
        var virtualFile = psiFile.getVirtualFile();
        if (virtualFile == null) {
            return null;
        }
        MosApplicationMarkerIndexer indexer = MosApplicationMarkerIndexer.getInstance(element.getProject());
        if (!indexer.isBuildEntryFile(virtualFile)) {
            return null;
        }
        if (!indexer.startOffsets(virtualFile).contains(element.getTextOffset())) {
            return null;
        }
        return new Info(
                AllIcons.RunConfigurations.TestState.Run,
                new AnAction[]{
                        new ApplicationAction("Run MOS", AllIcons.Actions.RunAll, false),
                        new ApplicationAction("Debug MOS", AllIcons.Actions.StartDebugger, true)
                },
                e -> "Run/Debug MOS application"
        );
    }

    private static final class ApplicationAction extends AnAction implements DumbAware {
        private final boolean debug;

        ApplicationAction(@NotNull String text, @Nullable javax.swing.Icon icon, boolean debug) {
            super(text, null, icon);
            this.debug = debug;
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent event) {
            MosRunConfigurations.execute(event.getProject(), null, debug);
        }
    }
}
