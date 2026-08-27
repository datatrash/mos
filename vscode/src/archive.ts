import {createWriteStream, promises as fs} from "node:fs";
import * as path from "node:path";
import type {Readable} from "node:stream";
import {pipeline} from "node:stream/promises";
import * as tar from "tar";
import yauzl, {type Entry, type ZipFile} from "yauzl";
import {isSafeArchivePath, type PlatformPackage} from "./release.js";

const MAX_EXECUTABLE_SIZE = 128 * 1024 * 1024;

export async function extractMosArchive(
  archivePath: string,
  destination: string,
  platform: PlatformPackage
): Promise<string> {
  await fs.mkdir(destination, {recursive: true});
  if (platform.archiveExtension === "zip") {
    await extractZipArchive(
      archivePath,
      destination,
      platform.executableName
    );
  } else {
    await tar.x({
      cwd: destination,
      file: archivePath,
      filter: (entryPath, entry) => {
        if (!isSafeArchivePath(entryPath)) {
          throw new Error(`Unsafe path in MOS archive: ${entryPath}`);
        }
        const isExecutable =
          path.posix.basename(entryPath.replaceAll("\\", "/")) ===
          platform.executableName;
        const isFile = "type" in entry ? entry.type === "File" : entry.isFile();
        if (isExecutable && !isFile) {
          throw new Error("The MOS executable in the archive is not a file.");
        }
        if (isExecutable && entry.size > MAX_EXECUTABLE_SIZE) {
          throw new Error("The MOS executable exceeds the safe extraction limit.");
        }
        return isExecutable;
      },
      preservePaths: false,
      strict: true
    });
  }

  const executable = await findFile(destination, platform.executableName);
  if (executable === undefined) {
    throw new Error(`The MOS archive does not contain ${platform.executableName}.`);
  }
  const root = await fs.realpath(destination);
  const realExecutable = await fs.realpath(executable);
  const relative = path.relative(root, realExecutable);
  if (relative.startsWith("..") || path.isAbsolute(relative)) {
    throw new Error("The MOS archive contains an executable outside its root.");
  }
  if (platform.executableName === "mos") {
    await fs.chmod(realExecutable, 0o755);
  }
  return realExecutable;
}

function extractZipArchive(
  archivePath: string,
  destination: string,
  executableName: string
): Promise<void> {
  return new Promise((resolve, reject) => {
    yauzl.open(
      archivePath,
      {
        decodeStrings: true,
        lazyEntries: true,
        validateEntrySizes: true
      },
      (openError, zipFile) => {
        if (openError !== null) {
          reject(openError);
          return;
        }
        processZipEntries(zipFile, destination, executableName).then(
          resolve,
          reject
        );
      }
    );
  });
}

function processZipEntries(
  zipFile: ZipFile,
  destination: string,
  executableName: string
): Promise<void> {
  return new Promise((resolve, reject) => {
    let settled = false;
    const fail = (error: unknown): void => {
      if (settled) {
        return;
      }
      settled = true;
      zipFile.close();
      reject(error instanceof Error ? error : new Error(String(error)));
    };

    zipFile.once("error", fail);
    zipFile.once("end", () => {
      if (!settled) {
        settled = true;
        resolve();
      }
    });
    zipFile.on("entry", (entry: Entry) => {
      void extractZipEntry(zipFile, entry, destination, executableName).catch(
        fail
      );
    });
    zipFile.readEntry();
  });
}

async function extractZipEntry(
  zipFile: ZipFile,
  entry: Entry,
  destination: string,
  executableName: string
): Promise<void> {
  if (!isSafeArchivePath(entry.fileName)) {
    throw new Error(`Unsafe path in MOS archive: ${entry.fileName}`);
  }

  const mode = (entry.externalFileAttributes >>> 16) & 0xffff;
  const fileType = mode & 0o170000;
  if (fileType === 0o120000) {
    throw new Error(`Refusing a symbolic link in MOS archive: ${entry.fileName}`);
  }

  const normalized = entry.fileName.replaceAll("\\", "/");
  const isExecutable = path.posix.basename(normalized) === executableName;
  if (!isExecutable) {
    zipFile.readEntry();
    return;
  }
  if (normalized.endsWith("/") || (fileType !== 0 && fileType !== 0o100000)) {
    throw new Error("The MOS executable in the archive is not a regular file.");
  }
  if (entry.uncompressedSize > MAX_EXECUTABLE_SIZE) {
    throw new Error("The MOS executable exceeds the safe extraction limit.");
  }

  const output = path.join(destination, executableName);
  const readStream = await openZipEntry(zipFile, entry);
  await pipeline(readStream, createWriteStream(output, {flags: "wx"}));
  zipFile.readEntry();
}

function openZipEntry(
  zipFile: ZipFile,
  entry: Entry
): Promise<Readable> {
  return new Promise((resolve, reject) => {
    zipFile.openReadStream(entry, (error, stream) => {
      if (error !== null) {
        reject(error);
      } else {
        resolve(stream);
      }
    });
  });
}

async function findFile(
  directory: string,
  filename: string
): Promise<string | undefined> {
  for (const entry of await fs.readdir(directory, {withFileTypes: true})) {
    const entryPath = path.join(directory, entry.name);
    if (entry.isFile() && entry.name === filename) {
      return entryPath;
    }
    if (entry.isDirectory()) {
      const found = await findFile(entryPath, filename);
      if (found !== undefined) {
        return found;
      }
    }
  }
  return undefined;
}
