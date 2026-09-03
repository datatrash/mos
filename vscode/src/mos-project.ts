import * as vscode from "vscode";
import {parseBuildEntry} from "./project-config.js";

const CONFIG_FILE_NAME = "mos.toml";

export function configUri(folder: vscode.WorkspaceFolder): vscode.Uri {
  return vscode.Uri.joinPath(folder.uri, CONFIG_FILE_NAME);
}

export async function isMosProject(
  folder: vscode.WorkspaceFolder
): Promise<boolean> {
  try {
    await vscode.workspace.fs.readFile(configUri(folder));
    return true;
  } catch {
    return false;
  }
}

/** The build entry file for a MOS workspace, or undefined when it is not a MOS project. */
export async function buildEntryUri(
  folder: vscode.WorkspaceFolder
): Promise<vscode.Uri | undefined> {
  let contents: Uint8Array;
  try {
    contents = await vscode.workspace.fs.readFile(configUri(folder));
  } catch {
    return undefined;
  }
  const entry = parseBuildEntry(new TextDecoder().decode(contents));
  return vscode.Uri.joinPath(
    folder.uri,
    ...entry.replaceAll("\\", "/").split("/")
  );
}
