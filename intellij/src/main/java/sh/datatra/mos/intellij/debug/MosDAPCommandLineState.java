package sh.datatra.mos.intellij.debug;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.RunConfigurationOptions;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.redhat.devtools.lsp4ij.dap.configurations.DAPCommandLineState;
import com.redhat.devtools.lsp4ij.dap.descriptors.DebugAdapterDescriptor;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.lsp.MosProjectRuntime;
import sh.datatra.mos.intellij.toolchain.MosCommandRunner;

final class MosDAPCommandLineState extends DAPCommandLineState {
    private final MosRunConfigurationOptions options;
    private final ExecutionEnvironment environment;

    MosDAPCommandLineState(
            @NotNull DebugAdapterDescriptor descriptor,
            @NotNull MosRunConfigurationOptions options,
            @NotNull ExecutionEnvironment environment
    ) {
        super(descriptor, options, environment);
        this.options = options;
        this.environment = environment;
    }

    @Override
    protected @NotNull ProcessHandler startProcess() throws ExecutionException {
        if (!options.isTestConfiguration() && options.getVicePath().isBlank()) {
            throw new ExecutionException(
                    "Configure a VICE executable in this run configuration or in MOS settings."
            );
        }
        MosProjectRuntime.getInstance(environment.getProject()).ensureStarted();
        if (!options.isTestConfiguration()) {
            build();
        }
        return super.startProcess();
    }

    /**
     * Runs `mos build` before launching the debug session. The build is performed on a background
     * thread (via {@link Task.WithResult}) rather than the EDT, because {@link
     * MosCommandRunner#runBlocking} blocks on the process and IntelliJ forbids blocking process
     * reads on the EDT (see OSProcessHandler.checkEdtAndReadAction).
     */
    private void build() throws ExecutionException {
        Project project = environment.getProject();
        if (!ApplicationManager.getApplication().isDispatchThread()) {
            MosCommandRunner.runBlocking(project, "build");
            return;
        }
        Task.WithResult<Void, ExecutionException> task = new Task.WithResult<>(
                project,
                "Building MOS project",
                true
        ) {
            @Override
            protected Void compute(@NotNull ProgressIndicator indicator) throws ExecutionException {
                MosCommandRunner.runBlocking(project, "build");
                return null;
            }
        };
        // ProgressManager.run blocks the calling (EDT) thread while `compute` runs on a background
        // thread, rethrowing any ExecutionException from the build.
        ProgressManager.getInstance().run(task);
    }
}
