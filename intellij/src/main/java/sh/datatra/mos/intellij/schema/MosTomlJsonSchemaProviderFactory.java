package sh.datatra.mos.intellij.schema;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.jetbrains.jsonSchema.extension.JsonSchemaFileProvider;
import com.jetbrains.jsonSchema.extension.JsonSchemaProviderFactory;
import com.jetbrains.jsonSchema.extension.SchemaType;
import com.jetbrains.jsonSchema.impl.JsonSchemaVersion;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Associates every {@code mos.toml} file with the bundled MOS JSON Schema, giving
 * schema-driven completion, validation and documentation for free.
 */
public final class MosTomlJsonSchemaProviderFactory implements JsonSchemaProviderFactory {

    @Override
    public @NotNull List<JsonSchemaFileProvider> getProviders(@NotNull Project project) {
        return List.of(new MosTomlSchema());
    }

    private static final class MosTomlSchema implements JsonSchemaFileProvider {

        @Override
        public boolean isAvailable(@NotNull VirtualFile file) {
            return "mos.toml".equals(file.getName());
        }

        @Override
        public @Nls @NotNull String getName() {
            return "mos.toml (MOS 6502)";
        }

        @Override
        public @NotNull SchemaType getSchemaType() {
            return SchemaType.embeddedSchema;
        }

        @Override
        public @NotNull JsonSchemaVersion getSchemaVersion() {
            return JsonSchemaVersion.SCHEMA_2020_12;
        }

        @Override
        public VirtualFile getSchemaFile() {
            return JsonSchemaProviderFactory.getResourceFile(getClass(), "/mos-schema.json");
        }
    }
}
