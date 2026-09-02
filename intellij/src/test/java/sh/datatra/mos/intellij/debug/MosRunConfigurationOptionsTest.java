package sh.datatra.mos.intellij.debug;

import com.intellij.testFramework.LightPlatformTestCase;

/**
 * IntelliJ's string state properties normalize blank values to {@code null} on write, so a
 * "Debug MOS" application configuration (which stores an empty test case name) used to make the
 * {@code @NotNull} getters fail with
 * "@NotNull method ... getTestCaseName must not return null".
 */
public final class MosRunConfigurationOptionsTest extends LightPlatformTestCase {
    public void testDefaultsAreEmptyRatherThanNull() {
        MosRunConfigurationOptions options = new MosRunConfigurationOptions();

        assertEquals("", options.getTestCaseName());
        assertEquals("", options.getVicePath());
        assertFalse(options.isTestConfiguration());
    }

    public void testApplicationConfigurationStoresBlankTestCaseName() {
        MosRunConfigurationOptions options = new MosRunConfigurationOptions();

        // This is exactly what the "Debug MOS" action does for an application (non-test) run.
        options.setTestCaseName("");
        options.setVicePath("");

        assertEquals("", options.getTestCaseName());
        assertEquals("", options.getVicePath());
        assertFalse(options.isTestConfiguration());
    }

    public void testNullValuesAreToleratedAndReadBackAsEmpty() {
        MosRunConfigurationOptions options = new MosRunConfigurationOptions();

        options.setTestCaseName(null);
        options.setVicePath(null);

        assertEquals("", options.getTestCaseName());
        assertEquals("", options.getVicePath());
        assertFalse(options.isTestConfiguration());
    }

    public void testTestConfigurationIsDetectedWhenNamed() {
        MosRunConfigurationOptions options = new MosRunConfigurationOptions();

        options.setTestCaseName("some_test");

        assertEquals("some_test", options.getTestCaseName());
        assertTrue(options.isTestConfiguration());
    }
}
