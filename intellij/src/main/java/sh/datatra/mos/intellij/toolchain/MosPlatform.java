package sh.datatra.mos.intellij.toolchain;

import com.intellij.openapi.util.SystemInfo;
import org.jetbrains.annotations.Nullable;

record MosPlatform(String target, String archiveExtension, String executableName) {
    static @Nullable MosPlatform current() {
        String architecture = System.getProperty("os.arch", "").toLowerCase();
        boolean arm64 = architecture.equals("aarch64") || architecture.equals("arm64");
        boolean x64 = architecture.equals("amd64") || architecture.equals("x86_64");
        if (SystemInfo.isWindows && x64) {
            return new MosPlatform("x86_64-pc-windows-msvc", "zip", "mos.exe");
        }
        if (SystemInfo.isLinux && (x64 || arm64)) {
            return new MosPlatform(
                    arm64 ? "aarch64-unknown-linux-musl" : "x86_64-unknown-linux-musl",
                    "tar.gz",
                    "mos"
            );
        }
        if (SystemInfo.isMac && (x64 || arm64)) {
            return new MosPlatform(
                    arm64 ? "aarch64-apple-darwin" : "x86_64-apple-darwin",
                    "tar.gz",
                    "mos"
            );
        }
        return null;
    }
}
