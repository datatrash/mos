package sh.datatra.mos.intellij.test;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.components.BaseState;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public final class MosTestConfigurationFactory extends ConfigurationFactory {
    protected MosTestConfigurationFactory(@NotNull ConfigurationType type) {
        super(type);
    }

    @Override
    public @NotNull RunConfiguration createTemplateConfiguration(@NotNull Project project) {
        return new MosTestRunConfiguration(project, this, "MOS Tests");
    }

    @Override
    public @NotNull String getId() {
        return MosTestConfigurationType.ID + ".factory";
    }

    @Override
    public @Nullable Class<? extends BaseState> getOptionsClass() {
        return MosTestConfigurationOptions.class;
    }
}