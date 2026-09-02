package sh.datatra.mos.intellij.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.NotNull;

public final class MosBuildAction extends MosProjectAction {
    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        MosToolchainActions.runCommand(event.getProject(), "build");
    }
}
