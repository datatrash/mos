package sh.datatra.mos.intellij;

import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * A directory is only treated as a MOS project when it holds a MOS project file. Everything the
 * plugin does on its own - starting the language server, downloading the toolchain, offering
 * CodeLens actions - is gated on this, so that opening an unrelated assembly file does nothing.
 */
public final class MosProject {
    public static final String CONFIG_FILE_NAME = "mos.toml";

    private MosProject() {
    }

    public static boolean isMosProject(@Nullable Project project) {
        return findConfigFile(project) != null;
    }

    /** The project's {@code mos.toml}, or {@code null} when it has none. */
    public static @Nullable Path findConfigFile(@Nullable Project project) {
        if (project == null || project.isDefault()) {
            return null;
        }
        String basePath = project.getBasePath();
        if (basePath == null) {
            return null;
        }
        return findConfigFile(Path.of(basePath));
    }

    public static @Nullable Path findConfigFile(@NotNull Path projectDirectory) {
        Path config = projectDirectory.resolve(CONFIG_FILE_NAME);
        return Files.isRegularFile(config) ? config : null;
    }
}
