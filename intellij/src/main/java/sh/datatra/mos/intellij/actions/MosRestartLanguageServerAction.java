package sh.datatra.mos.intellij.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.redhat.devtools.lsp4ij.LanguageServerManager;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.lsp.MosProjectRuntime;

public final class MosRestartLanguageServerAction extends MosProjectAction {
    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        if (event.getProject() == null) {
            return;
        }
        LanguageServerManager.StartOptions options = new LanguageServerManager.StartOptions()
                .setForceStart(true)
                .setForceRestart(true);
        LanguageServerManager.getInstance(event.getProject())
                .start(MosProjectRuntime.SERVER_ID, options);
    }
}
