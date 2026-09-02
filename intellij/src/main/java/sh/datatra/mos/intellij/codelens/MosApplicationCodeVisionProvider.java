package sh.datatra.mos.intellij.codelens;

import com.intellij.codeInsight.codeVision.CodeVisionAnchorKind;
import com.intellij.codeInsight.codeVision.CodeVisionEntry;
import com.intellij.codeInsight.codeVision.CodeVisionProvider;
import com.intellij.codeInsight.codeVision.CodeVisionRelativeOrdering;
import com.intellij.codeInsight.codeVision.CodeVisionState;
import com.intellij.codeInsight.codeVision.ui.model.ClickableTextCodeVisionEntry;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import kotlin.Pair;
import kotlin.Unit;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.MosProject;
import sh.datatra.mos.intellij.actions.MosRunConfigurations;
import sh.datatra.mos.intellij.lang.MosFileType;
import sh.datatra.mos.intellij.settings.MosSettings;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

public abstract class MosApplicationCodeVisionProvider implements CodeVisionProvider<Void> {
    private static final Logger LOG = Logger.getInstance(MosApplicationCodeVisionProvider.class);
    private final String id;
    private final String text;
    private final boolean debug;

    protected MosApplicationCodeVisionProvider(String id, String text, boolean debug) {
        this.id = id;
        this.text = text;
        this.debug = debug;
    }

    @Override
    public Void precomputeOnUiThread(@NotNull Editor editor) {
        return null;
    }

    @Override
    public @NotNull CodeVisionState computeCodeVision(@NotNull Editor editor, Void unused) {
        Project project = editor.getProject();
        VirtualFile file = FileDocumentManager.getInstance().getFile(editor.getDocument());
        if (project == null
                || file == null
                || file.getFileType() != MosFileType.INSTANCE
                || !MosSettings.getInstance().getState().applicationCodeLens
                || !isBuildEntry(file, project)) {
            return new CodeVisionState.Ready(Collections.emptyList());
        }

        Document document = editor.getDocument();
        List<Pair<TextRange, CodeVisionEntry>> entries = MosApplicationStarts
                .find(document.getCharsSequence())
                .stream()
                .map(start -> {
                    TextRange range = new TextRange(start.offset(), start.offset() + start.length());
                    return new Pair<>(range, entry(project));
                })
                .toList();
        return new CodeVisionState.Ready(entries);
    }

    private CodeVisionEntry entry(Project project) {
        return new ClickableTextCodeVisionEntry(
                text,
                id,
                (mouseEvent, editor) -> {
                    MosRunConfigurations.execute(project, null, debug);
                    return Unit.INSTANCE;
                },
                null,
                text,
                text + " application",
                Collections.emptyList()
        );
    }

    private static boolean isBuildEntry(VirtualFile file, Project project) {
        String basePath = project.getBasePath();
        Path config = MosProject.findConfigFile(project);
        if (basePath == null || config == null || !file.isInLocalFileSystem()) {
            return false;
        }
        String entry;
        try {
            entry = MosApplicationStarts.parseBuildEntry(Files.readString(config));
        } catch (IOException error) {
            LOG.warn("Could not read " + config, error);
            return false;
        }
        Path entryPath = Path.of(basePath).resolve(entry).normalize();
        return FileUtil.pathsEqual(file.getPath(), entryPath.toString());
    }

    @Override
    public @NotNull String getName() {
        return text;
    }

    @Override
    public @NotNull List<CodeVisionRelativeOrdering> getRelativeOrderings() {
        return Collections.emptyList();
    }

    @Override
    public @NotNull CodeVisionAnchorKind getDefaultAnchor() {
        return CodeVisionAnchorKind.Top;
    }

    @Override
    public @NotNull String getId() {
        return id;
    }
}
