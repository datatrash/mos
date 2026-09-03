package sh.datatra.mos.intellij.codelens;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.MosProject;
import sh.datatra.mos.intellij.lang.MosFileType;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Caches which file is the MOS application build entry and where its runnable start markers lie, so
 * the gutter line markers can decide cheaply on a per-element basis without re-reading {@code
 * mos.toml} or re-scanning the source on every highlight pass.
 */
@Service(Service.Level.PROJECT)
public final class MosApplicationMarkerIndexer {
    private static final Logger LOG = Logger.getInstance(MosApplicationMarkerIndexer.class);

    private final Project project;

    private VirtualFile cachedEntryFile;
    private long cachedEntryStamp = -1;
    private boolean cachedIsEntry;

    private VirtualFile cachedStartFile;
    private long cachedStartStamp = -1;
    private Set<Integer> cachedStartOffsets = Collections.emptySet();

    public MosApplicationMarkerIndexer(Project project) {
        this.project = project;
    }

    public static MosApplicationMarkerIndexer getInstance(Project project) {
        return project.getService(MosApplicationMarkerIndexer.class);
    }

    /** True when {@code file} is the configured build-entry source file. */
    public boolean isBuildEntryFile(@NotNull VirtualFile file) {
        if (!file.isInLocalFileSystem() || file.getFileType() != MosFileType.INSTANCE) {
            return false;
        }
        long stamp = file.getModificationStamp();
        if (file.equals(cachedEntryFile) && stamp == cachedEntryStamp) {
            return cachedIsEntry;
        }
        cachedEntryFile = file;
        cachedEntryStamp = stamp;
        cachedIsEntry = computeIsBuildEntry(file);
        return cachedIsEntry;
    }

    /** The runnable start offsets (from {@link MosApplicationStarts#find}) for {@code file}. */
    public @NotNull Set<Integer> startOffsets(@NotNull VirtualFile file) {
        Document document = FileDocumentManager.getInstance().getDocument(file);
        long stamp = document != null ? document.getModificationStamp() : file.getModificationStamp();
        if (file.equals(cachedStartFile) && stamp == cachedStartStamp) {
            return cachedStartOffsets;
        }
        cachedStartFile = file;
        cachedStartStamp = stamp;
        if (document == null) {
            cachedStartOffsets = Collections.emptySet();
            return cachedStartOffsets;
        }
        List<MosApplicationStarts.Start> starts = MosApplicationStarts.find(document.getCharsSequence());
        Set<Integer> offsets = new HashSet<>(starts.size());
        for (MosApplicationStarts.Start start : starts) {
            offsets.add(start.offset());
        }
        cachedStartOffsets = offsets;
        return cachedStartOffsets;
    }

    private boolean computeIsBuildEntry(@NotNull VirtualFile file) {
        String basePath = project.getBasePath();
        Path config = MosProject.findConfigFile(project);
        if (basePath == null || config == null) {
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
        return Path.of(file.getPath()).normalize().equals(entryPath);
    }
}