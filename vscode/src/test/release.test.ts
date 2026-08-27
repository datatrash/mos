import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {
  isSafeArchivePath,
  platformPackage,
  releaseUpdateAvailable,
  selectReleaseAsset,
  type MosRelease
} from "../release.js";

void describe("platformPackage", () => {
  void it("maps supported hosts to their published cargo-dist targets", () => {
    const cases = [
      ["win32", "x64", "zip", "mos.exe", "x86_64-pc-windows-msvc"],
      ["linux", "x64", "tar.gz", "mos", "x86_64-unknown-linux-musl"],
      ["linux", "arm64", "tar.gz", "mos", "aarch64-unknown-linux-musl"],
      ["darwin", "x64", "tar.gz", "mos", "x86_64-apple-darwin"],
      ["darwin", "arm64", "tar.gz", "mos", "aarch64-apple-darwin"]
    ] as const;

    for (const [host, architecture, archiveExtension, executableName, target] of cases) {
      assert.deepEqual(platformPackage(host, architecture), {
        archiveExtension,
        executableName,
        target
      });
    }
  });

  void it("rejects architectures without published binaries", () => {
    assert.equal(platformPackage("win32", "arm64"), undefined);
    assert.equal(platformPackage("linux", "ia32"), undefined);
  });
});

void describe("selectReleaseAsset", () => {
  const platform = platformPackage("linux", "x64");
  assert.ok(platform);

  void it("supports versioned release archive names", () => {
    const release: MosRelease = {
      assets: [
        {
          downloadUrl: "https://example.invalid/mos.tar.gz",
          name: "mos-0.8.2-x86_64-unknown-linux-musl.tar.gz"
        }
      ],
      tag: "0.8.2"
    };
    assert.equal(selectReleaseAsset(release, platform).name, release.assets[0]?.name);
  });

  void it("supports stable unversioned archive names", () => {
    const release: MosRelease = {
      assets: [
        {
          downloadUrl: "https://example.invalid/mos.tar.gz",
          name: "mos-x86_64-unknown-linux-musl.tar.gz"
        }
      ],
      tag: "v0.9.0"
    };
    assert.equal(selectReleaseAsset(release, platform).name, release.assets[0]?.name);
  });

  void it("selects every platform archive published for MOS 0.8.3", () => {
    const platforms = [
      platformPackage("win32", "x64"),
      platformPackage("linux", "x64"),
      platformPackage("linux", "arm64"),
      platformPackage("darwin", "x64"),
      platformPackage("darwin", "arm64")
    ];
    assert.ok(platforms.every((candidate) => candidate !== undefined));
    const release: MosRelease = {
      assets: platforms.map((candidate) => ({
        downloadUrl: "https://example.invalid/mos",
        name: `mos-${candidate.target}.${candidate.archiveExtension}`
      })),
      tag: "0.8.3"
    };

    for (const candidate of platforms) {
      assert.equal(
        selectReleaseAsset(release, candidate).name,
        `mos-${candidate.target}.${candidate.archiveExtension}`
      );
    }
  });
});

void describe("releaseUpdateAvailable", () => {
  void it("recognizes a newer release tag", () => {
    assert.equal(releaseUpdateAvailable("0.8.2", "0.9.0"), true);
  });

  void it("treats an optional v prefix as the same release", () => {
    assert.equal(releaseUpdateAvailable("0.8.2", "v0.8.2"), false);
  });
});

void describe("isSafeArchivePath", () => {
  void it("accepts paths within the archive root", () => {
    assert.equal(isSafeArchivePath("mos-0.8.2/mos"), true);
  });

  void it("rejects path traversal and absolute paths", () => {
    assert.equal(isSafeArchivePath("../mos"), false);
    assert.equal(isSafeArchivePath("/tmp/mos"), false);
    assert.equal(isSafeArchivePath("C:\\temp\\mos.exe"), false);
  });
});
