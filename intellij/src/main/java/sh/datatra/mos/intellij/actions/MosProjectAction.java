package sh.datatra.mos.intellij.actions;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.MosProject;

/** Hides the MOS actions in projects that are not MOS projects. */
abstract class MosProjectAction extends AnAction {
    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(MosProject.isMosProject(event.getProject()));
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        // isMosProject touches the file system, which is not allowed on the event dispatch thread.
        return ActionUpdateThread.BGT;
    }
}
