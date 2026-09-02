package sh.datatra.mos.intellij.test;

import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil;
import com.intellij.execution.testframework.ui.BaseTestsOutputConsoleView;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.toolchain.MosBinaryManager;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Runs {@code mos test} and streams its output into an SM test runner console. Each {@code .test}
 * block becomes a node in the test tree; a property file is unnecessary because MOS reports progress
 * as plain text lines.
 *
 * <p>All event wiring is delegated to {@link SMTestRunnerConnectionUtil}, which builds the converter
 * via {@link SMCustomMessagesParsing} on {@link MosTestConsoleProperties} and owns the full
 * start/process/terminate lifecycle. This keeps a single converter and a single process adapter, so
 * a failing run is reported as failed test nodes rather than a generic framework crash.</p>
 */
final class MosTestRunProfileState implements com.intellij.execution.configurations.RunProfileState {
    private final MosTestRunConfiguration configuration;
    private final ExecutionEnvironment environment;

    MosTestRunProfileState(
            @NotNull MosTestRunConfiguration configuration,
            @NotNull ExecutionEnvironment environment
    ) {
        this.configuration = configuration;
        this.environment = environment;
    }

    @Override
    public @NotNull com.intellij.execution.ExecutionResult execute(
            @NotNull Executor executor,
            @NotNull ProgramRunner<?> runner
    ) throws ExecutionException {
        GeneralCommandLine commandLine = buildCommandLine();
        OSProcessHandler processHandler = new OSProcessHandler(commandLine);

        MosTestConsoleProperties properties =
                new MosTestConsoleProperties(configuration, executor);
        BaseTestsOutputConsoleView console =
                SMTestRunnerConnectionUtil.createAndAttachConsole(
                        MosTestConsoleProperties.TEST_FRAMEWORK_NAME,
                        processHandler,
                        properties
                );
        processHandler.startNotify();
        return new DefaultExecutionResult(console, processHandler, console.createConsoleActions());
    }

    private @NotNull GeneralCommandLine buildCommandLine() throws ExecutionException {
        String basePath = environment.getProject().getBasePath();
        if (basePath == null) {
            throw new ExecutionException("Open a project containing mos.toml first.");
        }
        Path executable;
        try {
            executable = MosBinaryManager.getInstance().getExecutable(environment.getProject(), false);
        } catch (IOException error) {
            throw new ExecutionException("Could not resolve the MOS executable.", error);
        }
        GeneralCommandLine commandLine = new GeneralCommandLine(executable.toString(), "test")
                .withWorkDirectory(basePath);
        String filter = configuration.getOptions().getFilter();
        if (!filter.isBlank()) {
            commandLine.addParameter("--filter");
            commandLine.addParameter(filter);
        }
        return commandLine;
    }
}