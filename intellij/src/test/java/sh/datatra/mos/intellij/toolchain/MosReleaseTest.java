package sh.datatra.mos.intellij.toolchain;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

final class MosReleaseTest {
    @Test
    void selectsVersionedAndStableReleaseAssetNames() throws Exception {
        MosPlatform platform = new MosPlatform(
                "x86_64-pc-windows-msvc",
                "zip",
                "mos.exe"
        );
        MosRelease versioned = new MosRelease(
                "0.8.3",
                List.of(new MosRelease.Asset(
                        "mos-0.8.3-x86_64-pc-windows-msvc.zip",
                        "https://example.invalid/mos.zip",
                        null,
                        1
                ))
        );
        MosRelease stable = new MosRelease(
                "v0.8.3",
                List.of(new MosRelease.Asset(
                        "mos-x86_64-pc-windows-msvc.zip",
                        "https://example.invalid/mos.zip",
                        null,
                        1
                ))
        );

        assertEquals(versioned.assets().get(0), versioned.selectAsset(platform));
        assertEquals(stable.assets().get(0), stable.selectAsset(platform));
    }
}
