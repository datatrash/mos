package sh.datatra.mos.intellij.debug;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import com.redhat.devtools.lsp4ij.dap.breakpoints.DAPBreakpointTypeBase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.lang.MosFileType;

public final class MosBreakpointType extends DAPBreakpointTypeBase<XBreakpointProperties<?>> {
    public MosBreakpointType() {
        super("mos-breakpoint", "MOS Breakpoint");
    }

    @Override
    public @Nullable XBreakpointProperties<?> createBreakpointProperties(
            @NotNull VirtualFile file,
            int line
    ) {
        return null;
    }

    @Override
    public boolean canPutAt(@NotNull VirtualFile file, int line, @NotNull Project project) {
        return file.getFileType() == MosFileType.INSTANCE;
    }
}
