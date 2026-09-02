package sh.datatra.mos.intellij.toolchain;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.Path;

public final class MosCommandRunner {
    private MosCommandRunner() {
    }

    public static void runBlocking(@NotNull Project project, @NotNull String command)
            throws ExecutionException {
        String basePath = project.getBasePath();
        if (basePath == null) {
            throw new ExecutionException("Open a project containing mos.toml first.");
        }
        Path executable;
        try {
            executable = MosBinaryManager.getInstance().getExecutable(project, false);
        } catch (IOException error) {
            throw new ExecutionException("Could not resolve the MOS executable.", error);
        }
        GeneralCommandLine commandLine = new GeneralCommandLine(executable.toString(), command)
                .withWorkDirectory(basePath);
        ProcessOutput output = new CapturingProcessHandler(commandLine).runProcess();
        if (output.getExitCode() != 0) {
            String message = output.getStderr().isBlank() ? output.getStdout() : output.getStderr();
            throw new ExecutionException("MOS " + command + " failed:\n" + message.trim());
        }
    }
}
