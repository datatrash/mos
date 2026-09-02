package sh.datatra.mos.intellij.test;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Run configuration for the MOS test runner. The runner streams {@code mos test} output into an
 * SM test tree with one node per {@code .test} block, letting each test be re-run individually from
 * the test runner pane.
 */
public final class MosTestRunConfiguration extends RunConfigurationBase<MosTestConfigurationOptions> {
    public MosTestRunConfiguration(
            @NotNull Project project,
            @NotNull ConfigurationFactory factory,
            @Nullable String name
    ) {
        super(project, factory, name);
    }

    @Override
    public @NotNull MosTestConfigurationOptions getOptions() {
        return (MosTestConfigurationOptions) super.getOptions();
    }

    @Override
    public @NotNull SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
        return new MosTestSettingsEditor(getProject());
    }

    @Override
    public @Nullable RunProfileState getState(
            @NotNull Executor executor,
            @NotNull ExecutionEnvironment environment
    ) {
        return new MosTestRunProfileState(this, environment);
    }
}