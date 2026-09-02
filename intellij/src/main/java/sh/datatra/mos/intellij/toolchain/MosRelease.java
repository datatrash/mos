package sh.datatra.mos.intellij.toolchain;

import java.io.IOException;
import java.util.List;

record MosRelease(String tag, List<Asset> assets) {
    record Asset(String name, String downloadUrl, String digest, long size) {
    }

    Asset selectAsset(MosPlatform platform) throws IOException {
        String version = tag.startsWith("v") ? tag.substring(1) : tag;
        List<String> candidates = List.of(
                "mos-" + version + "-" + platform.target() + "." + platform.archiveExtension(),
                "mos-" + platform.target() + "." + platform.archiveExtension()
        );
        return assets.stream()
                .filter(asset -> candidates.contains(asset.name()))
                .findFirst()
                .orElseThrow(() -> new IOException(
                        "MOS " + tag + " does not provide a " + platform.target() + " archive."
                ));
    }
}
