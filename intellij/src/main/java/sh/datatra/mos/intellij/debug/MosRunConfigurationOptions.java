package sh.datatra.mos.intellij.debug;

import com.intellij.openapi.components.StoredProperty;
import com.redhat.devtools.lsp4ij.dap.configurations.DAPRunConfigurationOptionsBase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public final class MosRunConfigurationOptions extends DAPRunConfigurationOptionsBase {
    private final StoredProperty<String> vicePath = string("").provideDelegate(this, "vicePath");
    private final StoredProperty<String> testCaseName = string("").provideDelegate(this, "testCaseName");

    public @NotNull String getVicePath() {
        return notNullize(vicePath.getValue(this));
    }

    public void setVicePath(String value) {
        vicePath.setValue(this, notNullize(value));
    }

    public @NotNull String getTestCaseName() {
        return notNullize(testCaseName.getValue(this));
    }

    public void setTestCaseName(String value) {
        testCaseName.setValue(this, notNullize(value));
    }

    public boolean isTestConfiguration() {
        return !getTestCaseName().isBlank();
    }

    /**
     * IntelliJ's string state properties normalize blank values to {@code null} when they are stored,
     * so reading one back can return {@code null} even though it was written as an empty string.
     */
    private static @NotNull String notNullize(@Nullable String value) {
        return value == null ? "" : value;
    }
}
