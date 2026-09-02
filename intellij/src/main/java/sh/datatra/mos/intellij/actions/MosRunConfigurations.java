package sh.datatra.mos.intellij.actions;

import com.intellij.execution.ProgramRunnerUtil;
import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.debug.MosConfigurationFactory;
import sh.datatra.mos.intellij.debug.MosConfigurationType;
import sh.datatra.mos.intellij.debug.MosRunConfiguration;
import sh.datatra.mos.intellij.settings.MosSettings;
import sh.datatra.mos.intellij.test.MosTestConfigurationFactory;
import sh.datatra.mos.intellij.test.MosTestConfigurationType;
import sh.datatra.mos.intellij.test.MosTestRunConfiguration;

public final class MosRunConfigurations {
    private MosRunConfigurations() {
    }

    public static void execute(@NotNull Project project, @Nullable String testCase, boolean debug) {
        ApplicationManager.getApplication().invokeLater(() -> {
            String vicePath = MosSettings.getInstance().getState().vicePath;
            if (testCase == null && vicePath.isBlank()) {
                vicePath = askForVicePath(project);
                if (vicePath == null) {
                    return;
                }
            }
            MosConfigurationFactory factory =
                    (MosConfigurationFactory) MosConfigurationType.getInstance().getConfigurationFactories()[0];
            String name = testCase == null
                    ? "MOS Application"
                    : (debug ? "Debug " : "Run ") + testCase;
            RunnerAndConfigurationSettings settings =
                    RunManager.getInstance(project).createConfiguration(name, factory);
            MosRunConfiguration configuration = (MosRunConfiguration) settings.getConfiguration();
            configuration.getOptions().setTestCaseName(testCase == null ? "" : testCase);
            configuration.getOptions().setVicePath(vicePath);
            RunManager.getInstance(project).setTemporaryConfiguration(settings);
            ProgramRunnerUtil.executeConfiguration(
                    settings,
                    debug
                            ? DefaultDebugExecutor.getDebugExecutorInstance()
                            : DefaultRunExecutor.getRunExecutorInstance()
            );
        });
    }

    /**
     * Runs the whole test suite in the SM test runner when {@code testName} is null, or a single
     * test when it names one. Unlike {@link #execute}, this does not need VICE because MOS executes
     * {@code .test} blocks on its own embedded emulator.
     */
    public static void runTests(@NotNull Project project, @Nullable String testName) {
        ApplicationManager.getApplication().invokeLater(() -> {
            MosTestConfigurationFactory factory =
                    (MosTestConfigurationFactory) MosTestConfigurationType.getInstance().getConfigurationFactories()[0];
            String name = testName == null ? "MOS Tests" : "Run " + testName;
            RunnerAndConfigurationSettings settings =
                    RunManager.getInstance(project).createConfiguration(name, factory);
            MosTestRunConfiguration configuration = (MosTestRunConfiguration) settings.getConfiguration();
            configuration.getOptions().setFilter(testName == null ? "" : testName);
            RunManager.getInstance(project).setTemporaryConfiguration(settings);
            ProgramRunnerUtil.executeConfiguration(
                    settings,
                    DefaultRunExecutor.getRunExecutorInstance()
            );
        });
    }

    /**
     * Running an application needs the VICE emulator, which MOS cannot supply. Ask for it once and
     * remember it, rather than failing the run with an error the user has to go and fix by hand.
     */
    private static @Nullable String askForVicePath(@NotNull Project project) {
        VirtualFile chosen = FileChooser.chooseFile(
                FileChooserDescriptorFactory.createSingleFileOrExecutableAppDescriptor()
                        .withTitle("Select the VICE Executable")
                        .withDescription("MOS runs applications in VICE, for example x64sc.exe."),
                project,
                null
        );
        if (chosen == null) {
            return null;
        }
        String path = chosen.getPath();
        MosSettings.getInstance().getState().vicePath = path;
        return path;
    }
}
