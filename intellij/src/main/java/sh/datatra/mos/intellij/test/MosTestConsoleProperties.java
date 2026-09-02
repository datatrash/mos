package sh.datatra.mos.intellij.test;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.SMCustomMessagesParsing;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties;
import org.jetbrains.annotations.NotNull;

public final class MosTestConsoleProperties extends SMTRunnerConsoleProperties implements SMCustomMessagesParsing {
    public static final String TEST_FRAMEWORK_NAME = "MOSTestFramework";

    public MosTestConsoleProperties(
            @NotNull RunConfiguration configuration,
            @NotNull Executor executor
    ) {
        super(configuration, TEST_FRAMEWORK_NAME, executor);
        setPreservePresentableName(true);
        setPrintTestingStartedTime(false);
    }

    @Override
    public @NotNull OutputToGeneralTestEventsConverter createTestEventsConverter(
            @NotNull String testFrameworkName,
            @NotNull TestConsoleProperties consoleProperties
    ) {
        return new MosOutputConverter(testFrameworkName, consoleProperties);
    }
}