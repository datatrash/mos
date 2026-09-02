package sh.datatra.mos.intellij.test;

import com.intellij.execution.configurations.RunConfigurationOptions;
import com.intellij.openapi.components.StoredProperty;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Persisted settings for a MOS test run configuration. The scope decides which tests MOS runs:
 * when empty, the whole project's tests run; when set, only tests whose name matches the filter are
 * run. The SingleFile option carries the configured executable's absolute path if the user gave MOS
 * one explicitly (kept separately from {@link sh.datatra.mos.intellij.settings.MosSettings} so the
 * test runner behaves like the rest of the toolchain actions).
 */
public final class MosTestConfigurationOptions extends RunConfigurationOptions {
    private final StoredProperty<String> filter = string("").provideDelegate(this, "filter");

    public @NotNull String getFilter() {
        return notNullize(filter.getValue(this));
    }

    public void setFilter(@Nullable String value) {
        filter.setValue(this, notNullize(value));
    }

    private static @NotNull String notNullize(@Nullable String value) {
        return value == null ? "" : value;
    }
}