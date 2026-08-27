import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {parseRelease, parseSha256Digest} from "../github.js";

void describe("parseRelease", () => {
  void it("parses GitHub release metadata and its supplied digest", () => {
    const release = parseRelease({
      assets: [
        {
          browser_download_url: "https://github.com/example/mos.zip",
          digest: `sha256:${"a".repeat(64)}`,
          name: "mos.zip",
          size: 42
        }
      ],
      tag_name: "1.0.0"
    });

    assert.equal(release.tag, "1.0.0");
    assert.equal(release.assets[0]?.digest, `sha256:${"a".repeat(64)}`);
    assert.equal(release.assets[0].size, 42);
  });

  void it("accepts legacy release assets without digest metadata", () => {
    const release = parseRelease({
      assets: [
        {
          browser_download_url: "https://github.com/example/mos.zip",
          digest: null,
          name: "mos-0.8.2-x86_64-pc-windows-msvc.zip"
        }
      ],
      tag_name: "0.8.2"
    });

    assert.equal(release.assets[0]?.digest, undefined);
  });

  void it("rejects malformed GitHub responses", () => {
    assert.throws(() => parseRelease({assets: []}), /without a tag/);
    assert.throws(
      () => parseRelease({assets: [{}], tag_name: "1.0.0"}),
      /malformed release asset/
    );
  });
});

void describe("parseSha256Digest", () => {
  void it("allows releases without checksum metadata", () => {
    assert.equal(parseSha256Digest(undefined), undefined);
  });

  void it("uses valid GitHub SHA-256 metadata", () => {
    const hash = "a".repeat(64);
    assert.equal(parseSha256Digest(`sha256:${hash}`), hash);
  });

  void it("rejects malformed digest metadata instead of ignoring it", () => {
    assert.throws(
      () => parseSha256Digest("sha256:not-a-hash"),
      /Unsupported release digest/
    );
  });
});
