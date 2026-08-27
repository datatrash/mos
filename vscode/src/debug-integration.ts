import * as vscode from "vscode";
import type {LanguageRuntime} from "./language-runtime.js";
import {DEBUG_ADAPTER_HOST} from "./protocol.js";

export class MosDebugAdapterFactory
  implements vscode.DebugAdapterDescriptorFactory
{
  constructor(private readonly runtime: LanguageRuntime) {}

  async createDebugAdapterDescriptor(
    session: vscode.DebugSession
  ): Promise<vscode.DebugAdapterDescriptor> {
    const configuredWorkspace: unknown = session.configuration.workspace;
    const folder =
      session.workspaceFolder ??
      (typeof configuredWorkspace === "string"
        ? vscode.workspace.workspaceFolders?.find(
            ({uri}) => uri.fsPath === configuredWorkspace
          )
        : undefined);
    const port = await this.runtime.ensureStarted(folder);
    return new vscode.DebugAdapterServer(port, DEBUG_ADAPTER_HOST);
  }
}

export class MosDebugConfigurationProvider
  implements vscode.DebugConfigurationProvider
{
  resolveDebugConfiguration(
    folder: vscode.WorkspaceFolder | undefined,
    configuration: vscode.DebugConfiguration
  ): vscode.DebugConfiguration | undefined {
    if (Object.keys(configuration).length === 0) {
      configuration.type = "mos";
      configuration.request = "launch";
      configuration.name = "Launch with MOS";
    }

    const workspaceFolder = folder ?? activeWorkspaceFolder();
    if (configuration.workspace === undefined && workspaceFolder !== undefined) {
      configuration.workspace = workspaceFolder.uri.fsPath;
    }
    if (typeof configuration.workspace !== "string") {
      void vscode.window.showErrorMessage(
        "Open a workspace containing mos.toml before starting MOS debugging."
      );
      return undefined;
    }
    return configuration;
  }
}

export async function runSingleTest(
  testName: string,
  noDebug: boolean
): Promise<void> {
  const folder = activeWorkspaceFolder();
  if (folder === undefined) {
    throw new Error("Open a MOS workspace before running a test.");
  }
  const started = await vscode.debug.startDebugging(folder, {
    name: `${noDebug ? "Run" : "Debug"} ${testName}`,
    noDebug,
    request: "launch",
    testRunner: {
      testCaseName: testName
    },
    type: "mos",
    workspace: folder.uri.fsPath
  });
  if (!started) {
    throw new Error(`Could not ${noDebug ? "run" : "debug"} test ${testName}.`);
  }
}

function activeWorkspaceFolder(): vscode.WorkspaceFolder | undefined {
  const editor = vscode.window.activeTextEditor;
  return editor === undefined
    ? vscode.workspace.workspaceFolders?.[0]
    : vscode.workspace.getWorkspaceFolder(editor.document.uri);
}
