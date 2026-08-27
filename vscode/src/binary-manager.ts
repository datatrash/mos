import {randomUUID} from "node:crypto";
import {execFile} from "node:child_process";
import {promises as fs} from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as vscode from "vscode";
import {extractMosArchive} from "./archive.js";
import {
  downloadAsset,
  fetchLatestRelease,
  type NetworkOptions
} from "./github.js";
import {publishInstallDirectory} from "./install-publisher.js";
import type {MosLogger} from "./logging.js";
import {
  platformPackage,
  releaseUpdateAvailable,
  selectReleaseAsset,
  type PlatformPackage
} from "./release.js";
import {selectExecutablePath} from "./settings.js";
import {managedStoragePath} from "./storage.js";

const ACTIVE_INSTALL_KEY = "mos.activeManagedInstall";

interface ManagedInstall {
  readonly executable: string;
  readonly tag: string;
  readonly target: string;
}

export class BinaryManager {
  private checkedForUpdates = false;
  private inFlight: Promise<string> | undefined;

  constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly logger: MosLogger
  ) {}

  getExecutable(forceUpdate = false): Promise<string> {
    this.inFlight ??= this.resolveExecutable(forceUpdate).finally(() => {
      this.inFlight = undefined;
    });
    return this.inFlight;
  }

  private async resolveExecutable(forceUpdate: boolean): Promise<string> {
    if (!vscode.workspace.isTrusted) {
      throw new Error("Trust this workspace before running or downloading MOS.");
    }

    const configured = this.configuredExecutable();
    if (configured !== undefined) {
      await validateExecutable(configured);
      this.logger.info(`Using configured MOS executable: ${configured}`);
      return configured;
    }

    const current = this.context.globalState.get<ManagedInstall>(ACTIVE_INSTALL_KEY);
    const currentIsValid =
      current?.target === this.currentPlatform().target &&
      (await isFile(current.executable));
    const checkForUpdates = vscode.workspace
      .getConfiguration("mos")
      .get<boolean>("checkForUpdates", true);
    if (
      currentIsValid &&
      !forceUpdate &&
      (!checkForUpdates || this.checkedForUpdates)
    ) {
      return current.executable;
    }

    if (currentIsValid && !forceUpdate) {
      this.checkedForUpdates = true;
    }
    try {
      return await this.installLatest(
        currentIsValid ? current : undefined,
        currentIsValid && !forceUpdate
      );
    } catch (error) {
      if (currentIsValid && !forceUpdate) {
        this.logger.warn(
          `Could not check for a MOS update; continuing with ${current.tag}.`
        );
        return current.executable;
      }
      throw error;
    }
  }

  private async installLatest(
    current: ManagedInstall | undefined,
    promptBeforeUpdate: boolean
  ): Promise<string> {
    const platform = this.currentPlatform();
    const release = await vscode.window.withProgress(
      {
        cancellable: true,
        location: vscode.ProgressLocation.Notification,
        title: "Checking for the latest MOS release"
      },
      async (_progress, token) => fetchLatestRelease(token, networkOptions())
    );

    if (
      current !== undefined &&
      !releaseUpdateAvailable(current.tag, release.tag) &&
      current.target === platform.target
    ) {
      return current.executable;
    }

    if (current !== undefined && promptBeforeUpdate) {
      const selected = await vscode.window.showInformationMessage(
        `MOS ${release.tag} is available. You currently have ${current.tag} installed.`,
        "Update",
        "Not Now"
      );
      if (selected !== "Update") {
        this.logger.info(`MOS ${release.tag} update was postponed.`);
        return current.executable;
      }
    }

    const asset = selectReleaseAsset(release, platform);
    const installRoot = path.join(
      managedStoragePath(this.context.globalStorageUri),
      "toolchains"
    );
    const finalDirectory = path.join(
      installRoot,
      safePathSegment(release.tag),
      platform.target
    );
    const finalExecutable = path.join(finalDirectory, platform.executableName);
    if (await isFile(finalExecutable)) {
      await validateExecutable(finalExecutable);
      await this.setActiveInstall(finalExecutable, release.tag, platform.target);
      return finalExecutable;
    }

    await fs.mkdir(installRoot, {recursive: true});
    const stagingDirectory = path.join(installRoot, `.staging-${randomUUID()}`);
    const archivePath = path.join(stagingDirectory, asset.name);
    const extractedDirectory = path.join(stagingDirectory, "extracted");
    const payloadDirectory = path.join(stagingDirectory, "payload");
    await fs.mkdir(stagingDirectory);

    try {
      await vscode.window.withProgress(
        {
          cancellable: true,
          location: vscode.ProgressLocation.Notification,
          title: `Downloading MOS ${release.tag}`
        },
        async (progress, token) => {
          await downloadAsset(
            asset,
            archivePath,
            progress,
            token,
            networkOptions()
          );
        }
      );
      const extractedExecutable = await extractMosArchive(
        archivePath,
        extractedDirectory,
        platform
      );
      await fs.mkdir(payloadDirectory);
      const payloadExecutable = path.join(
        payloadDirectory,
        platform.executableName
      );
      await fs.copyFile(extractedExecutable, payloadExecutable);
      if (platform.executableName === "mos") {
        await fs.chmod(payloadExecutable, 0o755);
      }
      await validateExecutable(payloadExecutable);
      await fs.mkdir(path.dirname(finalDirectory), {recursive: true});
      await publishInstallDirectory(
        payloadDirectory,
        finalDirectory,
        finalExecutable
      );
      await validateExecutable(finalExecutable);
      await this.setActiveInstall(
        finalExecutable,
        release.tag,
        platform.target
      );
      this.logger.info(`Installed MOS ${release.tag} at ${finalExecutable}`);
      return finalExecutable;
    } finally {
      await fs.rm(stagingDirectory, {force: true, recursive: true});
    }
  }

  private async setActiveInstall(
    executable: string,
    tag: string,
    target: string
  ): Promise<void> {
    await this.context.globalState.update(ACTIVE_INSTALL_KEY, {
      executable,
      tag,
      target
    } satisfies ManagedInstall);
  }

  private configuredExecutable(): string | undefined {
    const configuration = vscode.workspace.getConfiguration("mos");
    const selected = selectExecutablePath(
      configuration.get<string>("executablePath"),
      configuration.get<string>("path")
    );
    if (selected === undefined) {
      return undefined;
    }
    if (selected.usesLegacySetting) {
      this.logger.warn(
        "The mos.path setting is deprecated; use mos.executablePath instead."
      );
    }
    const value = selected.path;

    const folder =
      vscode.window.activeTextEditor === undefined
        ? vscode.workspace.workspaceFolders?.[0]
        : vscode.workspace.getWorkspaceFolder(
            vscode.window.activeTextEditor.document.uri
          );
    let expanded = value;
    if (value === "~") {
      expanded = os.homedir();
    } else if (/^~[\\/]/.test(value)) {
      expanded = path.join(os.homedir(), value.slice(2));
    }
    if (folder !== undefined) {
      expanded = expanded.replaceAll("${workspaceFolder}", folder.uri.fsPath);
      if (!path.isAbsolute(expanded)) {
        expanded = path.join(folder.uri.fsPath, expanded);
      }
    }
    return path.resolve(expanded);
  }

  private currentPlatform(): PlatformPackage {
    const platform = platformPackage(process.platform, process.arch);
    if (platform === undefined) {
      throw new Error(
        `MOS does not publish a binary for ${process.platform}/${process.arch}. Configure mos.executablePath to use a custom build.`
      );
    }
    return platform;
  }
}

async function validateExecutable(executable: string): Promise<void> {
  if (!(await isFile(executable))) {
    throw new Error(`MOS executable does not exist: ${executable}`);
  }
  await new Promise<void>((resolve, reject) => {
    execFile(
      executable,
      ["version"],
      {timeout: 10_000, windowsHide: true},
      (error) => {
        if (error === null) {
          resolve();
        } else {
          reject(new Error(`Could not execute ${executable}: ${error.message}`));
        }
      }
    );
  });
}

async function isFile(filename: string): Promise<boolean> {
  try {
    return (await fs.stat(filename)).isFile();
  } catch (error) {
    if (isNotFound(error)) {
      return false;
    }
    throw error;
  }
}

function isNotFound(error: unknown): boolean {
  return (
    typeof error === "object" &&
    error !== null &&
    "code" in error &&
    error.code === "ENOENT"
  );
}

function safePathSegment(value: string): string {
  return value.replaceAll(/[^A-Za-z0-9._-]/g, "_");
}

function networkOptions(): NetworkOptions {
  const configuration = vscode.workspace.getConfiguration("http");
  const configuredProxy = configuration.get<string>("proxy", "").trim();
  return {
    proxyUrl:
      configuredProxy === ""
        ? process.env.HTTPS_PROXY ?? process.env.HTTP_PROXY
        : configuredProxy,
    strictSsl: configuration.get<boolean>("proxyStrictSSL", true)
  };
}
