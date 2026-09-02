package sh.datatra.mos.intellij.debug;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.RunConfigurationOptions;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
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
            MosCommandRunner.runBlocking(environment.getProject(), "build");
        }
        return super.startProcess();
    }
}
