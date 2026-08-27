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
  void it("selects the published Windows x64 archive", () => {
    assert.deepEqual(platformPackage("win32", "x64"), {
      archiveExtension: "zip",
      executableName: "mos.exe",
      target: "x86_64-pc-windows-msvc"
    });
  });

  void it("rejects architectures without published binaries", () => {
    assert.equal(platformPackage("linux", "arm64"), undefined);
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
