package sh.datatra.mos.intellij;

import com.intellij.testFramework.LightPlatformTestCase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * The plugin must stay inactive unless the project root holds a {@code mos.toml}, so that opening
 * an unrelated assembly file never starts a language server or downloads the toolchain.
 */
public final class MosProjectTest extends LightPlatformTestCase {
    public void testDirectoryWithoutConfigFileIsNotAMosProject() throws IOException {
        Path directory = Files.createTempDirectory("mos-no-config");
        try {
            assertNull(MosProject.findConfigFile(directory));
        } finally {
            Files.deleteIfExists(directory);
        }
    }

    public void testDirectoryWithConfigFileIsAMosProject() throws IOException {
        Path directory = Files.createTempDirectory("mos-config");
        Path config = directory.resolve(MosProject.CONFIG_FILE_NAME);
        Files.writeString(config, "[build]\nentry = \"main.asm\"\n");
        try {
            assertEquals(config, MosProject.findConfigFile(directory));
        } finally {
            Files.deleteIfExists(config);
            Files.deleteIfExists(directory);
        }
    }

    public void testConfigDirectoryDoesNotCount() throws IOException {
        Path directory = Files.createTempDirectory("mos-config-dir");
        Path config = Files.createDirectory(directory.resolve(MosProject.CONFIG_FILE_NAME));
        try {
            assertNull(MosProject.findConfigFile(directory));
        } finally {
            Files.deleteIfExists(config);
            Files.deleteIfExists(directory);
        }
    }

    public void testNullAndDefaultProjectsAreNotMosProjects() {
        assertFalse(MosProject.isMosProject(null));
    }

    /** The test fixture project has no mos.toml, so nothing should activate for it. */
    public void testProjectWithoutConfigFileIsNotAMosProject() {
        assertFalse(MosProject.isMosProject(getProject()));
    }
}
