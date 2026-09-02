package sh.datatra.mos.intellij.debug;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.RunConfigurationOptions;
import com.intellij.execution.process.NopProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.XDebugSession;
import com.redhat.devtools.lsp4ij.dap.DebugMode;
import com.redhat.devtools.lsp4ij.dap.breakpoints.DAPBreakpointHandlerBase;
import com.redhat.devtools.lsp4ij.dap.definitions.DebugAdapterServerDefinition;
import com.redhat.devtools.lsp4ij.dap.descriptors.DebugAdapterDescriptor;
import com.redhat.devtools.lsp4ij.dap.descriptors.ServerReadyConfig;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.lang.MosFileType;
import sh.datatra.mos.intellij.lsp.MosProjectRuntime;

import java.util.LinkedHashMap;
import java.util.Map;

final class MosDebugAdapterDescriptor extends DebugAdapterDescriptor {
    private static final String HOST = "127.0.0.1";

    MosDebugAdapterDescriptor(
            @NotNull RunConfigurationOptions options,
            @NotNull ExecutionEnvironment environment,
            @Nullable DebugAdapterServerDefinition serverDefinition
    ) {
        super(options, environment, serverDefinition);
    }

    @Override
    public ProcessHandler startServer() {
        return new NopProcessHandler();
    }

    @Override
    public @NotNull ServerReadyConfig getServerReadyConfig(@NotNull DebugMode debugMode) {
        try {
            int port = MosProjectRuntime.getInstance(environment.getProject()).getDebugAdapterPort();
            return new ServerReadyConfig(HOST, port);
        } catch (ExecutionException error) {
            throw new IllegalStateException(error.getMessage(), error);
        }
    }

    @Override
    public @NotNull Map<String, Object> getDapParameters() {
        MosRunConfigurationOptions mosOptions = (MosRunConfigurationOptions) options;
        Map<String, Object> parameters = new LinkedHashMap<>();
        parameters.put("type", "mos");
        parameters.put("request", "launch");
        parameters.put("workspace", requireProjectPath());
        if (mosOptions.isTestConfiguration()) {
            parameters.put("testRunner", Map.of("testCaseName", mosOptions.getTestCaseName()));
        } else {
            parameters.put("vicePath", mosOptions.getVicePath());
        }
        return parameters;
    }

    @Override
    public @NotNull DebugMode getDebugMode() {
        return DebugMode.LAUNCH;
    }

    @Override
    public @Nullable FileType getFileType() {
        return MosFileType.INSTANCE;
    }

    @Override
    public @NotNull DAPBreakpointHandlerBase<?> createBreakpointHandler(
            @NotNull XDebugSession session,
            Project project
    ) {
        return new MosBreakpointHandler(session, this, project);
    }

    private String requireProjectPath() {
        String path = environment.getProject().getBasePath();
        if (path == null) {
            throw new IllegalStateException("Open a MOS project before starting the debugger.");
        }
        return path;
    }
}
