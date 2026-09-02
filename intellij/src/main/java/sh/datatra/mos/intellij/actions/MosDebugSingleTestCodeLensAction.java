package sh.datatra.mos.intellij.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.Project;
import com.redhat.devtools.lsp4ij.commands.LSPCommand;
import com.redhat.devtools.lsp4ij.commands.LSPCommandAction;
import org.jetbrains.annotations.NotNull;

public final class MosDebugSingleTestCodeLensAction extends LSPCommandAction {
    @Override
    protected void commandPerformed(@NotNull LSPCommand command, @NotNull AnActionEvent event) {
        Project project = event.getProject();
        String testCase = command.getArgumentAt(0, String.class);
        if (project == null || testCase == null || testCase.isBlank()) {
            return;
        }
        MosRunConfigurations.execute(project, testCase, true);
    }
}
