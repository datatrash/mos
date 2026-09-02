package sh.datatra.mos.intellij.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.NotNull;

public final class MosInstallOrUpdateAction extends MosProjectAction {
    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        MosToolchainActions.installOrUpdate(event.getProject());
    }
}
