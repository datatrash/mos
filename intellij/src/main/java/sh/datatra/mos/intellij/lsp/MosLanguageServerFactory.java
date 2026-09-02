package sh.datatra.mos.intellij.lsp;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.redhat.devtools.lsp4ij.LanguageServerFactory;
import com.redhat.devtools.lsp4ij.client.features.LSPClientFeatures;
import com.redhat.devtools.lsp4ij.client.features.LSPCodeLensFeature;
import com.redhat.devtools.lsp4ij.server.OSProcessStreamConnectionProvider;
import com.redhat.devtools.lsp4ij.server.StreamConnectionProvider;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.MosProject;

import java.io.IOException;

public final class MosLanguageServerFactory implements LanguageServerFactory {
    @Override
    public @NotNull StreamConnectionProvider createConnectionProvider(@NotNull Project project) {
        try {
            MosProjectRuntime.MosServerCommand server = MosProjectRuntime.getInstance(project).createServerCommand();
            GeneralCommandLine commandLine = new GeneralCommandLine(server.executable().toString())
                    .withParameters(
                            "lsp",
                            "--debug-adapter-port",
                            Integer.toString(server.debugAdapterPort())
                    )
                    .withWorkDirectory(server.workingDirectory());
            return new OSProcessStreamConnectionProvider(commandLine);
        } catch (IOException | ExecutionException error) {
            throw new IllegalStateException("Could not start the MOS language server.", error);
        }
    }

    @Override
    public @NotNull LSPClientFeatures createClientFeatures() {
        return new LSPClientFeatures() {
            @Override
            public boolean isEnabled(@NotNull VirtualFile file) {
                return MosProject.isMosProject(getProject());
            }

            {
                // Render test run/debug as gutter markers (MosTestLineMarkerContributor) instead of
                // inline CodeLens. The server keeps advertising/emitting CodeLens for the VSCode
                // client; this only stops the IntelliJ client from rendering them.
                setCodeLensFeature(new LSPCodeLensFeature() {
                    @Override
                    public boolean isEnabled(@NotNull PsiFile file) {
                        return false;
                    }
                });
            }
        };
    }
}
