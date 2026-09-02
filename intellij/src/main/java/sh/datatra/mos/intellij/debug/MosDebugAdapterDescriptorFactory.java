package sh.datatra.mos.intellij.debug;

import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunConfigurationOptions;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.xdebugger.breakpoints.XBreakpointType;
import com.redhat.devtools.lsp4ij.dap.descriptors.DebugAdapterDescriptor;
import com.redhat.devtools.lsp4ij.dap.descriptors.DebugAdapterDescriptorFactory;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.MosProject;
import sh.datatra.mos.intellij.lang.MosFileType;
import sh.datatra.mos.intellij.settings.MosSettings;

public final class MosDebugAdapterDescriptorFactory extends DebugAdapterDescriptorFactory {
    @Override
    public DebugAdapterDescriptor createDebugAdapterDescriptor(
            @NotNull RunConfigurationOptions options,
            @NotNull ExecutionEnvironment environment
    ) {
        return new MosDebugAdapterDescriptor(options, environment, getServerDefinition());
    }

    @Override
    public boolean supportsBreakpointType(@NotNull XBreakpointType breakpointType) {
        return breakpointType.getClass() == MosBreakpointType.class;
    }

    @Override
    public boolean isDebuggableFile(@NotNull VirtualFile file, @NotNull Project project) {
        return file.getFileType() == MosFileType.INSTANCE && MosProject.isMosProject(project);
    }

    @Override
    public boolean prepareConfiguration(
            @NotNull RunConfiguration configuration,
            @NotNull VirtualFile file,
            @NotNull Project project
    ) {
        if (!(configuration instanceof MosRunConfiguration mosConfiguration)) {
            return false;
        }
        mosConfiguration.setName("MOS Application");
        mosConfiguration.getOptions().setVicePath(MosSettings.getInstance().getState().vicePath);
        return true;
    }
}
