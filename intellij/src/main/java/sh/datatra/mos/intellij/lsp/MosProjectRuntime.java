package sh.datatra.mos.intellij.lsp;

import com.intellij.execution.ExecutionException;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.util.net.NetUtils;
import com.redhat.devtools.lsp4ij.LanguageServerManager;
import sh.datatra.mos.intellij.toolchain.MosBinaryManager;

import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@Service(Service.Level.PROJECT)
public final class MosProjectRuntime {
    public static final String SERVER_ID = "mos";

    private final Project project;
    private volatile int debugAdapterPort;

    public MosProjectRuntime(Project project) {
        this.project = project;
    }

    public static MosProjectRuntime getInstance(Project project) {
        return project.getService(MosProjectRuntime.class);
    }

    public synchronized MosServerCommand createServerCommand() throws IOException, ExecutionException {
        Path executable = MosBinaryManager.getInstance().getExecutable(project, false);
        debugAdapterPort = NetUtils.findAvailableSocketPort();
        String workingDirectory = project.getBasePath();
        if (workingDirectory == null) {
            throw new IOException("Open a MOS project before starting the language server.");
        }
        return new MosServerCommand(executable, workingDirectory, debugAdapterPort);
    }

    public void ensureStarted() throws ExecutionException {
        LanguageServerManager manager = LanguageServerManager.getInstance(project);
        LanguageServerManager.StartOptions options = new LanguageServerManager.StartOptions()
                .setForceStart(true)
                .setForceRestart(false);
        manager.start(SERVER_ID, options);
        try {
            if (manager.getLanguageServer(SERVER_ID).get(30, TimeUnit.SECONDS) == null) {
                throw new ExecutionException("MOS language server did not start.");
            }
        } catch (InterruptedException error) {
            Thread.currentThread().interrupt();
            throw new ExecutionException("Interrupted while starting the MOS language server.", error);
        } catch (java.util.concurrent.ExecutionException | TimeoutException error) {
            throw new ExecutionException("Could not start the MOS language server.", error);
        }
    }

    public int getDebugAdapterPort() throws ExecutionException {
        int port = debugAdapterPort;
        if (port < 1) {
            throw new ExecutionException("MOS language server did not provide a debug adapter port.");
        }
        return port;
    }

    public record MosServerCommand(Path executable, String workingDirectory, int debugAdapterPort) {
    }
}
