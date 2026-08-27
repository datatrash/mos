import {promises as fs} from "node:fs";

export async function publishInstallDirectory(
  payloadDirectory: string,
  finalDirectory: string,
  finalExecutable: string
): Promise<void> {
  try {
    await fs.rename(payloadDirectory, finalDirectory);
    return;
  } catch {
    if (await isFile(finalExecutable)) {
      return;
    }
    await fs.rm(finalDirectory, {force: true, recursive: true});
  }

  try {
    await fs.rename(payloadDirectory, finalDirectory);
  } catch (error) {
    if (!(await isFile(finalExecutable))) {
      throw error;
    }
  }
}

async function isFile(filename: string): Promise<boolean> {
  try {
    return (await fs.stat(filename)).isFile();
  } catch (error) {
    if (
      typeof error === "object" &&
      error !== null &&
      "code" in error &&
      error.code === "ENOENT"
    ) {
      return false;
    }
    throw error;
  }
}
