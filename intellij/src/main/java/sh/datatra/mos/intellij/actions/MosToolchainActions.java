package sh.datatra.mos.intellij.actions;

import com.intellij.execution.ExecutionException;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.redhat.devtools.lsp4ij.LanguageServerManager;
import com.redhat.devtools.lsp4ij.server.LanguageServerException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.lsp.MosProjectRuntime;
import sh.datatra.mos.intellij.toolchain.MosBinaryManager;
import sh.datatra.mos.intellij.toolchain.MosCommandRunner;

import java.io.IOException;

final class MosToolchainActions {
    private MosToolchainActions() {
    }

    static void runCommand(@Nullable Project project, @NotNull String command) {
        if (project == null) {
            return;
        }
        ProgressManager.getInstance().run(new Task.Backgroundable(
                project,
                command.equals("build") ? "Building MOS project" : "Running MOS tests",
                true
        ) {
            @Override
            public void run(@NotNull ProgressIndicator indicator) {
                try {
                    MosCommandRunner.runBlocking(project, command);
                    showNotification(project, "MOS " + command + " completed.", NotificationType.INFORMATION);
                } catch (ExecutionException error) {
                    showNotification(project, error.getMessage(), NotificationType.ERROR);
                }
            }
        });
    }

    static void installOrUpdate(@Nullable Project project) {
        if (project == null) {
            return;
        }
        ProgressManager.getInstance().run(new Task.Backgroundable(
                project,
                "Installing MOS toolchain",
                true
        ) {
            @Override
            public void run(@NotNull ProgressIndicator indicator) {
                try {
                    MosBinaryManager.getInstance().getExecutable(project, true);
                    LanguageServerManager.StartOptions options = new LanguageServerManager.StartOptions()
                            .setForceStart(true)
                            .setForceRestart(true);
                    LanguageServerManager.getInstance(project).start(MosProjectRuntime.SERVER_ID, options);
                    showNotification(project, "MOS is installed and up to date.", NotificationType.INFORMATION);
                } catch (IOException | ExecutionException | LanguageServerException error) {
                    showNotification(
                            project,
                            "Could not install MOS: " + error.getMessage(),
                            NotificationType.ERROR
                    );
                }
            }
        });
    }

    private static void showNotification(Project project, String content, NotificationType type) {
        NotificationGroupManager.getInstance()
                .getNotificationGroup("MOS")
                .createNotification(content == null ? "MOS command failed." : content, type)
                .notify(project);
    }
}
