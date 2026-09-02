package sh.datatra.mos.intellij.test;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.execution.configurations.ConfigurationTypeUtil;
import com.intellij.openapi.util.NotNullLazyValue;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.lang.MosIcons;

public final class MosTestConfigurationType extends ConfigurationTypeBase {
    public static final String ID = "Mos6502TestConfiguration";

    public MosTestConfigurationType() {
        super(
                ID,
                "MOS Tests",
                "Run tests in a MOS 6502 project",
                NotNullLazyValue.createValue(() -> MosIcons.FILE)
        );
        addFactory(new MosTestConfigurationFactory(this));
    }

    public static @NotNull MosTestConfigurationType getInstance() {
        return ConfigurationTypeUtil.findConfigurationType(MosTestConfigurationType.class);
    }

    public static @NotNull ConfigurationFactory findFactory() {
        return getInstance().getConfigurationFactories()[0];
    }
}