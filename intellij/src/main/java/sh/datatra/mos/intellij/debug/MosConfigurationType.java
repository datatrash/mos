package sh.datatra.mos.intellij.debug;

import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.execution.configurations.ConfigurationTypeUtil;
import com.intellij.openapi.util.NotNullLazyValue;
import sh.datatra.mos.intellij.lang.MosIcons;

public final class MosConfigurationType extends ConfigurationTypeBase {
    public static final String ID = "Mos6502Configuration";

    public MosConfigurationType() {
        super(
                ID,
                "MOS 6502",
                "Run or debug a MOS application or test",
                NotNullLazyValue.createValue(() -> MosIcons.FILE)
        );
        addFactory(new MosConfigurationFactory(this));
    }

    public static MosConfigurationType getInstance() {
        return ConfigurationTypeUtil.findConfigurationType(MosConfigurationType.class);
    }
}
