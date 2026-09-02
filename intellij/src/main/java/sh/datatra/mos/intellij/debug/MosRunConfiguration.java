package sh.datatra.mos.intellij.debug;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.redhat.devtools.lsp4ij.dap.DebugAdapterManager;
import com.redhat.devtools.lsp4ij.dap.configurations.DAPRunConfigurationBase;
import com.redhat.devtools.lsp4ij.dap.definitions.DebugAdapterServerDefinition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.lsp.MosProjectRuntime;

public final class MosRunConfiguration extends DAPRunConfigurationBase<MosRunConfigurationOptions> {
    MosRunConfiguration(
            @NotNull Project project,
            @Nullable ConfigurationFactory factory,
            @Nullable String name
    ) {
        super(project, factory, name);
    }

    @Override
    protected @Nullable DebugAdapterServerDefinition getDebugAdapterServer() {
        return DebugAdapterManager.getInstance().getDebugAdapterServerById(MosProjectRuntime.SERVER_ID);
    }

    @Override
    public @NotNull SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
        return new MosRunSettingsEditor(getProject());
    }

    @Override
    public @Nullable RunProfileState getState(
            @NotNull Executor executor,
            @NotNull ExecutionEnvironment environment
    ) {
        MosDebugAdapterDescriptor descriptor = new MosDebugAdapterDescriptor(
                getOptions(),
                environment,
                getDebugAdapterServer()
        );
        return new MosDAPCommandLineState(descriptor, getOptions(), environment);
    }

    @Override
    public @NotNull MosRunConfigurationOptions getOptions() {
        return (MosRunConfigurationOptions) super.getOptions();
    }
}
