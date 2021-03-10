import * as vscode from "vscode";
import * as path from "path";
import {createReadStream, createWriteStream, promises as fs, renameSync} from "fs";
import {download, fetchRelease} from "./net";
import * as os from "os";
import {x as tar_extract} from "tar";
import {Parse} from "unzipper";
import rimraf = require("rimraf");
import {execFile} from "child_process";
import {promisify} from "util";

export async function getMosBinary(ctx: vscode.ExtensionContext): Promise<string | undefined> {
    const mos_version = await fs.readFile(path.join(vscode.extensions.getExtension("datatrash.mos")!!.extensionPath, "MOS_VERSION"), "utf8");

    let cfg = vscode.workspace.getConfiguration("mos");
    const explicitPath = cfg.get<string>("path");
    if (explicitPath) {
        if (explicitPath.startsWith("~/")) {
            return os.homedir() + explicitPath.slice("~".length);
        }

        if (!path.isAbsolute(explicitPath) && vscode.workspace.workspaceFolders !== undefined) {
            // Relative path, so make it absolute from the workspace folder
            let workspacePath = vscode.workspace.workspaceFolders[0].uri.fsPath;
            return path.join(workspacePath, explicitPath);
        } else {
            return explicitPath;
        }
    }

    const platforms: { [key: string]: string } = {
        "ia32 win32": "x86_64-pc-windows-msvc",
        "x64 win32": "x86_64-pc-windows-msvc",
        "x64 linux": "x86_64-unknown-linux-musl",
        "x64 darwin": "x86_64-apple-darwin",
    };
    let platform = platforms[`${process.arch} ${process.platform}`];
    if (platform === undefined) {
        await vscode.window.showErrorMessage("Unfortunately we don't ship binaries for your platform yet.");
        return undefined;
    }

    const bin_path = path.join(ctx.globalStorageUri.fsPath, "bin");
    const bin_extract_path = path.join(ctx.globalStorageUri.fsPath, "extract");
    const ext = platform.indexOf("-windows-") !== -1 ? ".exe" : "";
    const dest = path.join(bin_path, `mos${ext}`);
    const exists = await fs.stat(dest).then(() => true, () => false);
    if (exists) {
        const execFile = promisify(require('child_process').execFile);
        let mos_version_output = await execFile(dest, ["--version"]);
        let existing_version = mos_version_output.stdout.split(" ")[1].trim();
        if (existing_version == mos_version) {
            return dest;
        } else {
            const updateResponse = await vscode.window.showInformationMessage(
                `There is a new version of MOS (v${mos_version}), you currently have v${existing_version} installed.`,
                "Update now",
                "Dismiss"
            );
            if (updateResponse !== "Update now") return dest;
        }
    } else {
        const userResponse = await vscode.window.showInformationMessage(
            `The mos extension requires the MOS binary, which is not yet installed.`,
            "Download now",
            "Dismiss"
        );
        if (userResponse !== "Download now") return undefined;
    }

    let release = await downloadWithRetryDialog(async () => {
        return await fetchRelease(mos_version, null, null);
    })
    const arch_ext = platform.indexOf("-windows-") !== -1 ? "zip" : "tar.gz";
    const archive = `mos-${mos_version}-${platform}.${arch_ext}`;
    const artifact = release.assets.find(artifact => artifact.name === archive)!!;

    const full_archive_path = path.join(ctx.globalStorageUri.fsPath, archive);
    await downloadWithRetryDialog(async () => {
        await download({
            url: artifact.browser_download_url,
            dest: full_archive_path,
            progressTitle: "Downloading MOS",
            mode: 0o755,
            httpProxy: undefined,
        });
    });

    await fs.mkdir(bin_extract_path, { recursive: true });
    switch (arch_ext) {
        case "tar.gz": {
            await tar_extract({
                file: full_archive_path,
                cwd: bin_extract_path,
                strip: 1,
            });
            break;
        }
        case "zip": {
            let unzip = new Promise<void>(function(resolve, reject) {
                createReadStream(full_archive_path)
                    .pipe(Parse({ path: bin_extract_path }))
                    .on('entry', entry => {
                        // Strip path
                        entry.path = path.join(bin_extract_path, path.basename(entry.path));
                        if (entry.type === 'File') {
                            entry.pipe(createWriteStream(entry.path));
                        } else {
                            entry.autodrain();
                        }
                    })
                    .on('end', () => resolve())
                    .on('error', reject)
            });
            await unzip;
            break;
        }
    }
    await fs.unlink(full_archive_path);

    // Binary path can be removed, and extracted binary path can be swapped
    rimraf.sync(bin_path);
    renameSync(bin_extract_path, bin_path);

    return dest;
}

async function downloadWithRetryDialog<T>(fn: () => Promise<T>): Promise<T> {
    while (true) {
        try {
            return await fn();
        } catch (e) {
            const selected = await vscode.window.showErrorMessage("Failed to download: " + e.message, {}, {
                title: "Retry",
                retry: true,
            }, {
                title: "Dismiss",
            });

            if (selected?.retry) {
                continue;
            }
            throw e;
        }
    }
}
