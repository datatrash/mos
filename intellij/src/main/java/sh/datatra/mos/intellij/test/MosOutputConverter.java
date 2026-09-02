package sh.datatra.mos.intellij.test;

import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.execution.testframework.sm.runner.events.TestFailedEvent;
import com.intellij.execution.testframework.sm.runner.events.TestFinishedEvent;
import com.intellij.execution.testframework.sm.runner.events.TestStartedEvent;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Turns {@code mos test} console output into SM test events so each {@code .test} block appears as
 * its own node in the test runner tree with a pass/fail status.
 *
 * <p>MOS reports progress on stdout as a single line per test:
 * <pre>{@code test 'name' ... ok (N cycles)}</pre>
 * or {@code test 'name' ... failed (N cycles)}. Finalization of the run ({@link
 * OutputToGeneralTestEventsConverter#finishTesting()}) happens in
 * {@link #processTerminated} once the process has exited.</p>
 */
final class MosOutputConverter extends OutputToGeneralTestEventsConverter {
    private static final Pattern TEST_LINE = Pattern.compile(
            "^test '([^']+)' \\.\\.\\.\\s*(ok|failed)"
    );

    MosOutputConverter(@NotNull String testFrameworkName, @NotNull TestConsoleProperties properties) {
        super(testFrameworkName, properties);
    }

    @Override
    protected void processConsistentText(@NotNull String text, @NotNull Key<?> outputType) {
        String plain = stripAnsi(text);
        Matcher matcher = TEST_LINE.matcher(plain);
        if (matcher.find()) {
            String name = matcher.group(1);
            boolean failed = "failed".equals(matcher.group(2));
            TestStartedEvent started = new TestStartedEvent(name, null);
            getProcessor().onTestStarted(started);
            if (failed) {
                getProcessor().onTestFailure(new TestFailedEvent(name, "Test failed", null, false, null, null));
            }
            getProcessor().onTestFinished(new TestFinishedEvent(name, null));
            return;
        }
        fireOnUncapturedOutput(text, outputType);
    }

    private static @NotNull String stripAnsi(@NotNull String text) {
        return text.replaceAll("\\u001B\\[[;\\d]*m", "");
    }
}