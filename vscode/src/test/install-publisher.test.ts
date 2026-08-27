import assert from "node:assert/strict";
import {promises as fs} from "node:fs";
import {tmpdir} from "node:os";
import * as path from "node:path";
import {describe, it} from "node:test";
import {publishInstallDirectory} from "../install-publisher.js";

void describe("publishInstallDirectory", () => {
  void it("replaces an incomplete destination directory", async () => {
    const root = await fs.mkdtemp(path.join(tmpdir(), "mos-vscode-publish-"));
    try {
      const payload = path.join(root, "payload");
      const destination = path.join(root, "toolchain");
      const executable = path.join(destination, "mos.exe");
      await fs.mkdir(payload);
      await fs.writeFile(path.join(payload, "mos.exe"), "new MOS");
      await fs.mkdir(destination);
      await fs.writeFile(path.join(destination, "partial-download"), "stale");

      await publishInstallDirectory(payload, destination, executable);

      assert.equal(await fs.readFile(executable, "utf8"), "new MOS");
      await assert.rejects(fs.stat(path.join(destination, "partial-download")), {
        code: "ENOENT"
      });
    } finally {
      await fs.rm(root, {force: true, recursive: true});
    }
  });

  void it("preserves a concurrently published install", async () => {
    const root = await fs.mkdtemp(path.join(tmpdir(), "mos-vscode-publish-"));
    try {
      const payload = path.join(root, "payload");
      const destination = path.join(root, "toolchain");
      const executable = path.join(destination, "mos.exe");
      await fs.mkdir(payload);
      await fs.writeFile(path.join(payload, "mos.exe"), "new MOS");
      await fs.mkdir(destination);
      await fs.writeFile(executable, "concurrent MOS");

      await publishInstallDirectory(payload, destination, executable);

      assert.equal(await fs.readFile(executable, "utf8"), "concurrent MOS");
    } finally {
      await fs.rm(root, {force: true, recursive: true});
    }
  });
});
