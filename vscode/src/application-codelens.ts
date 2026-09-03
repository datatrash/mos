import * as vscode from "vscode";
import {findApplicationStarts} from "./application-starts.js";
import {
  createMosLaunchConfiguration,
  hasLaunchConfiguration,
  parseLaunchConfigurations,
  writableLaunchConfigurations
} from "./launch-config.js";
import {buildEntryUri, isMosProject} from "./mos-project.js";

export class ApplicationCodeLensProvider
  implements vscode.CodeLensProvider, vscode.Disposable
{
  private readonly changedEmitter = new vscode.EventEmitter<void>();
  readonly onDidChangeCodeLenses = this.changedEmitter.event;

  dispose(): void {
    this.changedEmitter.dispose();
  }

  refresh(): void {
    this.changedEmitter.fire();
  }

  async provideCodeLenses(
    document: vscode.TextDocument
  ): Promise<vscode.CodeLens[]> {
    if (
      !vscode.workspace
        .getConfiguration("mos", document.uri)
        .get<boolean>("applicationCodeLens", true)
    ) {
      return [];
    }
    const folder = vscode.workspace.getWorkspaceFolder(document.uri);
    if (folder === undefined || !(await isMosProject(folder))) {
      return [];
    }
    const entryUri = await buildEntryUri(folder);
    if (entryUri === undefined || normalizedUri(document.uri) !== normalizedUri(entryUri)) {
      return [];
    }

    return findApplicationStarts(document.getText()).flatMap((start) => {
      const begin = document.positionAt(start.offset);
      const range = new vscode.Range(
        begin,
        document.positionAt(start.offset + start.length)
      );
      return [
        new vscode.CodeLens(range, {
          arguments: [document.uri],
          command: "mos.runApplication",
          title: "$(play) Run MOS"
        }),
        new vscode.CodeLens(range, {
          arguments: [document.uri],
          command: "mos.debugApplication",
          title: "$(debug-alt) Debug MOS"
        })
      ];
    });
  }
}

export async function launchApplication(
  resource: vscode.Uri,
  noDebug: boolean
): Promise<void> {
  const folder = vscode.workspace.getWorkspaceFolder(resource);
  if (folder === undefined) {
    throw new Error("Open the MOS application in a workspace before launching it.");
  }
  const configuredName = vscode.workspace
    .getConfiguration("mos", resource)
    .get<string>("launchConfiguration", "Launch with MOS");
  const configurationName = configuredName.trim() || "Launch with MOS";
  if (!(await ensureLaunchConfiguration(folder, configurationName))) {
    return;
  }
  const started = await vscode.debug.startDebugging(folder, configurationName, {
    noDebug
  });
  if (!started) {
    throw new Error(
      `Could not start launch configuration '${configurationName}'. Create it in .vscode/launch.json or change mos.launchConfiguration.`
    );
  }
}

async function ensureLaunchConfiguration(
  folder: vscode.WorkspaceFolder,
  configurationName: string
): Promise<boolean> {
  const launchSettings = vscode.workspace.getConfiguration("launch", folder.uri);
  const configurations = parseLaunchConfigurations(
    launchSettings.get<unknown>("configurations")
  );
  if (hasLaunchConfiguration(configurations, configurationName)) {
    return true;
  }

  const selected = await vscode.window.showInformationMessage(
    `MOS launch configuration '${configurationName}' is missing. Create a default launch.json?`,
    {modal: true},
    "Create launch.json"
  );
  if (selected !== "Create launch.json") {
    return false;
  }

  const emulator = await vscode.window.showOpenDialog({
    canSelectFiles: true,
    canSelectFolders: false,
    canSelectMany: false,
    filters:
      process.platform === "win32" ? {Executables: ["exe"]} : undefined,
    openLabel: "Use Emulator",
    title: "Select the VICE emulator executable (for example, x64sc.exe)"
  });
  const emulatorUri = emulator?.[0];
  if (emulatorUri === undefined) {
    return false;
  }

  const inspected = launchSettings.inspect<unknown>("configurations");
  const hasWorkspaceFile = vscode.workspace.workspaceFile !== undefined;
  const writableConfigurations = writableLaunchConfigurations(
    inspected?.workspaceValue,
    inspected?.workspaceFolderValue,
    hasWorkspaceFile
  );
  await launchSettings.update(
    "configurations",
    [
      ...writableConfigurations,
      createMosLaunchConfiguration(configurationName, emulatorUri.fsPath)
    ],
    hasWorkspaceFile
      ? vscode.ConfigurationTarget.WorkspaceFolder
      : vscode.ConfigurationTarget.Workspace
  );
  return true;
}

function normalizedUri(uri: vscode.Uri): string {
  const value = uri.toString();
  return process.platform === "win32" && uri.scheme === "file"
    ? value.toLowerCase()
    : value;
}
