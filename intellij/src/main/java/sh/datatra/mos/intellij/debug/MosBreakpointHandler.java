package sh.datatra.mos.intellij.debug;

import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.redhat.devtools.lsp4ij.dap.breakpoints.DAPBreakpointHandlerBase;
import org.jetbrains.annotations.NotNull;

final class MosBreakpointHandler
        extends DAPBreakpointHandlerBase<XLineBreakpoint<XBreakpointProperties<?>>> {
    MosBreakpointHandler(
            @NotNull XDebugSession session,
            @NotNull MosDebugAdapterDescriptor descriptor,
            @NotNull Project project
    ) {
        super(MosBreakpointType.class, session, descriptor, project);
    }
}
