import assert from "node:assert/strict";
import {createWriteStream, promises as fs} from "node:fs";
import {tmpdir} from "node:os";
import * as path from "node:path";
import {finished} from "node:stream/promises";
import {describe, it} from "node:test";
import * as tar from "tar";
import yazl from "yazl";
import {extractMosArchive} from "../archive.js";
import {platformPackage} from "../release.js";

void describe("extractMosArchive", () => {
  void it("extracts the MOS executable from a nested ZIP archive", async () => {
    const root = await fs.mkdtemp(path.join(tmpdir(), "mos-vscode-zip-"));
    try {
      const archive = path.join(root, "mos.zip");
      await createZip(archive, 0o100755);
      const destination = path.join(root, "output");
      const platform = platformPackage("win32", "x64");
      assert.ok(platform);

      const executable = await extractMosArchive(
        archive,
        destination,
        platform
      );

      assert.equal(path.basename(executable), "mos.exe");
      assert.equal(await fs.readFile(executable, "utf8"), "fake MOS executable");
    } finally {
      await fs.rm(root, {force: true, recursive: true});
    }
  });

  void it("rejects a ZIP symlink masquerading as MOS", async () => {
    const root = await fs.mkdtemp(path.join(tmpdir(), "mos-vscode-link-"));
    try {
      const archive = path.join(root, "mos.zip");
      await createZip(archive, 0o120777);
      const platform = platformPackage("win32", "x64");
      assert.ok(platform);

      await assert.rejects(
        extractMosArchive(archive, path.join(root, "output"), platform),
        /symbolic link|regular file/
      );
    } finally {
      await fs.rm(root, {force: true, recursive: true});
    }
  });

  void it("extracts only the executable from a tar.gz archive", async () => {
    const root = await fs.mkdtemp(path.join(tmpdir(), "mos-vscode-tar-"));
    try {
      const input = path.join(root, "input");
      await fs.mkdir(path.join(input, "release"), {recursive: true});
      await fs.writeFile(path.join(input, "release", "mos"), "fake MOS");
      await fs.writeFile(path.join(input, "release", "README.md"), "ignored");
      const archive = path.join(root, "mos.tar.gz");
      await tar.c(
        {cwd: input, file: archive, gzip: true},
        ["release/mos", "release/README.md"]
      );
      const platform = platformPackage("linux", "x64");
      assert.ok(platform);
      const destination = path.join(root, "output");

      const executable = await extractMosArchive(
        archive,
        destination,
        platform
      );

      assert.equal(await fs.readFile(executable, "utf8"), "fake MOS");
      await assert.rejects(fs.stat(path.join(destination, "release", "README.md")));
    } finally {
      await fs.rm(root, {force: true, recursive: true});
    }
  });
});

async function createZip(filename: string, mode: number): Promise<void> {
  const archive = new yazl.ZipFile();
  archive.addBuffer(
    Buffer.from("fake MOS executable"),
    "mos-release/mos.exe",
    {mode}
  );
  archive.end();
  await finished(archive.outputStream.pipe(createWriteStream(filename)));
}
