package sh.datatra.mos.intellij.toolchain;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.ide.impl.TrustedProjects;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.io.HttpRequests;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.jetbrains.annotations.NotNull;
import sh.datatra.mos.intellij.settings.MosSettings;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

@Service(Service.Level.APP)
public final class MosBinaryManager {
    private static final Logger LOG = Logger.getInstance(MosBinaryManager.class);
    private static final String RELEASE_API = "https://api.github.com/repos/datatrash/mos/releases/latest";
    private static final String USER_AGENT = "sh.datatra.mos.intellij";
    private static final long MAX_EXECUTABLE_SIZE = 128L * 1024L * 1024L;

    private boolean checkedForUpdates;

    public static MosBinaryManager getInstance() {
        return ApplicationManager.getApplication().getService(MosBinaryManager.class);
    }

    public synchronized @NotNull Path getExecutable(@NotNull Project project, boolean forceUpdate)
            throws IOException, ExecutionException {
        if (!TrustedProjects.isTrusted(project)) {
            throw new ExecutionException("Trust this project before running or downloading MOS.");
        }
        MosSettings.State settings = MosSettings.getInstance().getState();
        if (!settings.executablePath.isBlank()) {
            Path configured = expandConfiguredPath(settings.executablePath, project);
            validateExecutable(configured);
            return configured;
        }

        MosPlatform platform = MosPlatform.current();
        if (platform == null) {
            throw new IOException(
                    "MOS does not publish a binary for this operating system and architecture. "
                            + "Configure a custom MOS executable in Settings."
            );
        }

        Path current = settings.managedExecutable.isBlank() ? null : Path.of(settings.managedExecutable);
        boolean currentIsValid = current != null
                && settings.managedTarget.equals(platform.target())
                && Files.isRegularFile(current);
        if (currentIsValid
                && !forceUpdate
                && (!settings.checkForUpdates || checkedForUpdates)) {
            return current;
        }
        if (currentIsValid && !forceUpdate) {
            checkedForUpdates = true;
        }

        try {
            return installLatest(platform, currentIsValid ? current : null, forceUpdate, project);
        } catch (IOException | ExecutionException | RuntimeException error) {
            if (currentIsValid && !forceUpdate) {
                LOG.warn("Could not check for a MOS update; using " + settings.managedTag, error);
                return current;
            }
            throw error;
        }
    }

    private Path installLatest(
            MosPlatform platform,
            Path current,
            boolean forceUpdate,
            Project project
    ) throws IOException, ExecutionException {
        ProgressIndicator indicator = ProgressManager.getGlobalProgressIndicator();
        MosRelease release = fetchLatestRelease(indicator);
        MosSettings.State settings = MosSettings.getInstance().getState();
        if (current != null && normalizeTag(settings.managedTag).equals(normalizeTag(release.tag()))) {
            return current;
        }
        if (current != null && !forceUpdate && !confirmUpdate(settings.managedTag, release.tag())) {
            return current;
        }

        MosRelease.Asset asset = release.selectAsset(platform);
        Path installRoot = Path.of(PathManager.getSystemPath(), "mos", "toolchains");
        Path finalDirectory = installRoot
                .resolve(safePathSegment(release.tag()))
                .resolve(platform.target());
        Path finalExecutable = finalDirectory.resolve(platform.executableName());
        if (Files.isRegularFile(finalExecutable)) {
            validateExecutable(finalExecutable);
            activate(finalExecutable, release.tag(), platform.target());
            return finalExecutable;
        }

        Files.createDirectories(installRoot);
        Path staging = installRoot.resolve(".staging-" + UUID.randomUUID());
        Path archive = staging.resolve(asset.name());
        Path payload = staging.resolve("payload");
        Files.createDirectories(staging);
        try {
            download(asset, archive, indicator);
            Files.createDirectories(payload);
            Path payloadExecutable = payload.resolve(platform.executableName());
            extractExecutable(archive, payloadExecutable, platform);
            if (!SystemInfo.isWindows) {
                payloadExecutable.toFile().setExecutable(true, true);
            }
            validateExecutable(payloadExecutable);
            Files.createDirectories(finalDirectory.getParent());
            publish(payload, finalDirectory, finalExecutable);
            validateExecutable(finalExecutable);
            activate(finalExecutable, release.tag(), platform.target());
            return finalExecutable;
        } finally {
            if (!FileUtil.delete(staging.toFile())) {
                LOG.warn("Could not remove MOS staging directory " + staging);
            }
        }
    }

    private static MosRelease fetchLatestRelease(ProgressIndicator indicator) throws IOException {
        String response = HttpRequests.request(RELEASE_API)
                .userAgent(USER_AGENT)
                .readString(indicator);
        JsonObject root = JsonParser.parseString(response).getAsJsonObject();
        if (!root.has("tag_name") || !root.get("tag_name").isJsonPrimitive()) {
            throw new IOException("GitHub returned release metadata without a tag.");
        }
        JsonArray jsonAssets = root.getAsJsonArray("assets");
        if (jsonAssets == null) {
            throw new IOException("GitHub returned release metadata without assets.");
        }
        List<MosRelease.Asset> assets = new ArrayList<>(jsonAssets.size());
        for (JsonElement element : jsonAssets) {
            JsonObject asset = element.getAsJsonObject();
            String name = requiredString(asset, "name");
            String url = requiredString(asset, "browser_download_url");
            String digest = optionalString(asset, "digest");
            long size = asset.has("size") ? asset.get("size").getAsLong() : -1;
            assets.add(new MosRelease.Asset(name, url, digest, size));
        }
        return new MosRelease(root.get("tag_name").getAsString(), List.copyOf(assets));
    }

    private static void download(
            MosRelease.Asset asset,
            Path destination,
            ProgressIndicator indicator
    ) throws IOException {
        URI uri = URI.create(asset.downloadUrl());
        if (!"https".equalsIgnoreCase(uri.getScheme())) {
            throw new IOException("Refusing an insecure MOS download URL: " + uri.getScheme());
        }
        if (indicator != null) {
            indicator.setText("Downloading MOS " + asset.name());
        }
        HttpRequests.request(asset.downloadUrl())
                .userAgent(USER_AGENT)
                .connect(request -> {
                    try (InputStream input = request.getInputStream();
                         OutputStream output = Files.newOutputStream(destination, StandardOpenOption.CREATE_NEW)) {
                        byte[] buffer = new byte[64 * 1024];
                        long downloaded = 0;
                        int read;
                        while ((read = input.read(buffer)) >= 0) {
                            if (indicator != null) {
                                indicator.checkCanceled();
                            }
                            output.write(buffer, 0, read);
                            downloaded += read;
                            if (indicator != null && asset.size() > 0) {
                                indicator.setFraction(Math.min(1.0, (double) downloaded / asset.size()));
                            }
                        }
                    }
                    return null;
                });
        verifyDigest(destination, asset.digest());
    }

    private static void verifyDigest(Path file, String digest) throws IOException {
        if (digest == null || digest.isBlank()) {
            return;
        }
        if (!digest.matches("(?i)^sha256:[0-9a-f]{64}$")) {
            throw new IOException("Unsupported release digest: " + digest);
        }
        try (InputStream input = Files.newInputStream(file)) {
            MessageDigest sha256 = MessageDigest.getInstance("SHA-256");
            byte[] buffer = new byte[64 * 1024];
            int read;
            while ((read = input.read(buffer)) >= 0) {
                sha256.update(buffer, 0, read);
            }
            String actual = HexFormat.of().formatHex(sha256.digest());
            String expected = digest.substring("sha256:".length());
            if (!actual.equalsIgnoreCase(expected)) {
                Files.deleteIfExists(file);
                throw new IOException("Checksum validation failed for " + file.getFileName() + ".");
            }
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is unavailable.", impossible);
        }
    }

    private static void extractExecutable(
            Path archive,
            Path destination,
            MosPlatform platform
    ) throws IOException {
        try (InputStream fileInput = new BufferedInputStream(Files.newInputStream(archive));
             InputStream archiveInput = platform.archiveExtension().equals("zip")
                     ? new ZipArchiveInputStream(fileInput)
                     : new TarArchiveInputStream(new GzipCompressorInputStream(fileInput))) {
            ArchiveEntry entry;
            while ((entry = nextEntry(archiveInput)) != null) {
                String normalized = entry.getName().replace('\\', '/');
                if (!isSafeArchivePath(normalized)) {
                    throw new IOException("Unsafe path in MOS archive: " + entry.getName());
                }
                if (!Path.of(normalized).getFileName().toString().equals(platform.executableName())) {
                    continue;
                }
                if (entry.isDirectory()
                        || entry instanceof TarArchiveEntry tar && (tar.isSymbolicLink() || tar.isLink())
                        || entry instanceof ZipArchiveEntry zip && zip.isUnixSymlink()) {
                    throw new IOException("The MOS executable in the archive is not a regular file.");
                }
                if (entry.getSize() > MAX_EXECUTABLE_SIZE) {
                    throw new IOException("The MOS executable exceeds the safe extraction limit.");
                }
                copyBounded(archiveInput, destination);
                return;
            }
        }
        throw new IOException("The MOS archive does not contain " + platform.executableName() + ".");
    }

    private static ArchiveEntry nextEntry(InputStream archive) throws IOException {
        if (archive instanceof ZipArchiveInputStream zip) {
            return zip.getNextEntry();
        }
        return ((TarArchiveInputStream) archive).getNextEntry();
    }

    private static void copyBounded(InputStream input, Path destination) throws IOException {
        try (OutputStream output = Files.newOutputStream(destination, StandardOpenOption.CREATE_NEW)) {
            byte[] buffer = new byte[64 * 1024];
            long total = 0;
            int read;
            while ((read = input.read(buffer)) >= 0) {
                total += read;
                if (total > MAX_EXECUTABLE_SIZE) {
                    throw new IOException("The MOS executable exceeds the safe extraction limit.");
                }
                output.write(buffer, 0, read);
            }
        }
    }

    private static void publish(Path payload, Path finalDirectory, Path finalExecutable) throws IOException {
        try {
            Files.move(payload, finalDirectory, StandardCopyOption.ATOMIC_MOVE);
        } catch (AtomicMoveNotSupportedException unsupported) {
            try {
                Files.move(payload, finalDirectory);
            } catch (FileAlreadyExistsException exists) {
                if (!Files.isRegularFile(finalExecutable)) {
                    throw exists;
                }
            }
        } catch (FileAlreadyExistsException exists) {
            if (!Files.isRegularFile(finalExecutable)) {
                throw exists;
            }
        }
    }

    private static void validateExecutable(Path executable) throws IOException, ExecutionException {
        if (!Files.isRegularFile(executable)) {
            throw new IOException("MOS executable does not exist: " + executable);
        }
        GeneralCommandLine commandLine = new GeneralCommandLine(executable.toString(), "version")
                .withWorkDirectory(executable.getParent().toString());
        ProcessOutput output = new CapturingProcessHandler(commandLine).runProcess(
                (int) Duration.ofSeconds(10).toMillis(),
                true
        );
        if (output.isTimeout()) {
            throw new ExecutionException("Timed out while validating " + executable + ".");
        }
        if (output.getExitCode() != 0) {
            throw new ExecutionException(
                    "Could not execute " + executable + ": " + output.getStderr().trim()
            );
        }
    }

    private static Path expandConfiguredPath(String value, Project project) {
        String expanded = value.trim();
        String home = System.getProperty("user.home");
        if (expanded.equals("~")) {
            expanded = home;
        } else if (expanded.startsWith("~/") || expanded.startsWith("~\\")) {
            expanded = Path.of(home, expanded.substring(2)).toString();
        }
        String projectPath = project.getBasePath();
        if (projectPath != null) {
            expanded = expanded
                    .replace("${workspaceFolder}", projectPath)
                    .replace("${PROJECT_DIR}", projectPath);
        }
        Path path = Path.of(expanded);
        if (!path.isAbsolute() && projectPath != null) {
            path = Path.of(projectPath).resolve(path);
        }
        return path.toAbsolutePath().normalize();
    }

    private static boolean confirmUpdate(String current, String latest) {
        AtomicBoolean accepted = new AtomicBoolean();
        Runnable prompt = () -> accepted.set(Messages.showYesNoDialog(
                "MOS " + latest + " is available. You currently have " + current + " installed.",
                "Update MOS",
                "Update",
                "Not Now",
                Messages.getQuestionIcon()
        ) == Messages.YES);
        if (ApplicationManager.getApplication().isDispatchThread()) {
            prompt.run();
        } else {
            ApplicationManager.getApplication().invokeAndWait(prompt);
        }
        return accepted.get();
    }

    private static void activate(Path executable, String tag, String target) {
        MosSettings.State settings = MosSettings.getInstance().getState();
        settings.managedExecutable = executable.toString();
        settings.managedTag = tag;
        settings.managedTarget = target;
    }

    private static String requiredString(JsonObject object, String property) throws IOException {
        String value = optionalString(object, property);
        if (value == null) {
            throw new IOException("GitHub returned malformed release asset metadata.");
        }
        return value;
    }

    private static String optionalString(JsonObject object, String property) {
        JsonElement value = object.get(property);
        return value == null || value.isJsonNull() || !value.isJsonPrimitive()
                ? null
                : value.getAsString();
    }

    private static String safePathSegment(String value) {
        return value.replaceAll("[^A-Za-z0-9._-]", "_");
    }

    private static String normalizeTag(String value) {
        return value.trim().replaceFirst("(?i)^v", "");
    }

    private static boolean isSafeArchivePath(String value) {
        if (value.startsWith("/") || value.matches("^[A-Za-z]:/.*")) {
            return false;
        }
        int depth = 0;
        for (String segment : value.split("/")) {
            if (segment.isEmpty() || segment.equals(".")) {
                continue;
            }
            if (segment.equals("..")) {
                if (depth == 0) {
                    return false;
                }
                depth--;
            } else {
                depth++;
            }
        }
        return true;
    }
}
