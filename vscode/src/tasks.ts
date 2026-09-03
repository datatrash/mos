import * as vscode from "vscode";
import type {BinaryManager} from "./binary-manager.js";

export type MosTaskCommand = "build" | "test";

interface MosTaskDefinition extends vscode.TaskDefinition {
  readonly command: MosTaskCommand;
  readonly type: "mos";
}

export class MosTaskProvider implements vscode.TaskProvider {
  constructor(private readonly binaries: BinaryManager) {}

  async provideTasks(): Promise<vscode.Task[]> {
    const executable = await this.binaries.getExecutable();
    return (vscode.workspace.workspaceFolders ?? []).flatMap((folder) => [
      createTask(folder, executable, "build"),
      createTask(folder, executable, "test")
    ]);
  }

  async resolveTask(task: vscode.Task): Promise<vscode.Task | undefined> {
    if (!isMosTaskCommand(task.definition.command)) {
      return undefined;
    }
    const folder =
      task.scope !== undefined && task.scope !== vscode.TaskScope.Global
        ? task.scope
        : activeWorkspaceFolder();
    if (typeof folder === "number" || folder === undefined) {
      return undefined;
    }
    return createTask(
      folder,
      await this.binaries.getExecutable(),
      task.definition.command
    );
  }
}

export async function executeMosTask(
  provider: MosTaskProvider,
  command: MosTaskCommand
): Promise<void> {
  const folder = activeWorkspaceFolder();
  if (folder === undefined) {
    throw new Error("Open a workspace with mos.toml at its root first.");
  }
  const tasks = await provider.provideTasks();
  const task = tasks.find(
    (candidate) =>
      candidate.scope === folder && candidate.definition.command === command
  );
  if (task === undefined) {
    throw new Error(`Could not create the MOS ${command} task.`);
  }
  await vscode.tasks.executeTask(task);
}

function createTask(
  folder: vscode.WorkspaceFolder,
  executable: string,
  command: MosTaskCommand
): vscode.Task {
  const definition: MosTaskDefinition = {command, type: "mos"};
  const label = command === "build" ? "Build" : "Run all tests";
  const execution = new vscode.ProcessExecution(executable, [command], {
    cwd: folder.uri.fsPath
  });
  const task = new vscode.Task(
    definition,
    folder,
    label,
    "mos",
    execution,
    ["$mos"]
  );
  task.group =
    command === "build" ? vscode.TaskGroup.Build : vscode.TaskGroup.Test;
  return task;
}

function activeWorkspaceFolder(): vscode.WorkspaceFolder | undefined {
  const editor = vscode.window.activeTextEditor;
  return editor === undefined
    ? vscode.workspace.workspaceFolders?.[0]
    : vscode.workspace.getWorkspaceFolder(editor.document.uri);
}

function isMosTaskCommand(value: unknown): value is MosTaskCommand {
  return value === "build" || value === "test";
}
