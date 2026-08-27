import * as vscode from "vscode";
import {
  ApplicationCodeLensProvider,
  launchApplication
} from "./application-codelens.js";
import {BinaryManager} from "./binary-manager.js";
import {
  MosDebugAdapterFactory,
  MosDebugConfigurationProvider,
  runSingleTest
} from "./debug-integration.js";
import {LanguageRuntime} from "./language-runtime.js";
import {MosLogger} from "./logging.js";
import {LANGUAGE_ID} from "./protocol.js";
import {executeMosTask, MosTaskProvider} from "./tasks.js";

let runtime: LanguageRuntime | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const logger = new MosLogger();
  const binaries = new BinaryManager(context, logger);
  const languageRuntime = new LanguageRuntime(binaries, logger);
  runtime = languageRuntime;
  const tasks = new MosTaskProvider(binaries);
  const applicationCodeLens = new ApplicationCodeLensProvider();
  const projectConfigWatcher =
    vscode.workspace.createFileSystemWatcher("**/mos.toml");
  const status = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left,
    50
  );
  status.name = "MOS toolchain";
  status.text = "$(tools) MOS";
  status.tooltip = "Install or update the MOS toolchain";
  status.command = "mos.installOrUpdate";
  status.show();

  context.subscriptions.push(
    logger,
    languageRuntime,
    applicationCodeLens,
    projectConfigWatcher,
    projectConfigWatcher.onDidChange(() => applicationCodeLens.refresh()),
    projectConfigWatcher.onDidCreate(() => applicationCodeLens.refresh()),
    projectConfigWatcher.onDidDelete(() => applicationCodeLens.refresh()),
    status,
    vscode.tasks.registerTaskProvider("mos", tasks),
    vscode.languages.registerCodeLensProvider(
      {language: LANGUAGE_ID, scheme: "file"},
      applicationCodeLens
    ),
    vscode.debug.registerDebugAdapterDescriptorFactory(
      "mos",
      new MosDebugAdapterFactory(languageRuntime)
    ),
    vscode.debug.registerDebugConfigurationProvider(
      "mos",
      new MosDebugConfigurationProvider()
    ),
    vscode.commands.registerCommand("mos.installOrUpdate", () =>
      runCommand(logger, async () => {
        status.text = "$(sync~spin) MOS";
        try {
          const executable = await binaries.getExecutable(true);
          await languageRuntime.restart();
          status.tooltip = `MOS: ${executable}`;
          void vscode.window.showInformationMessage(
            "MOS is installed and up to date."
          );
        } finally {
          status.text = "$(tools) MOS";
        }
      })
    ),
    vscode.commands.registerCommand("mos.restartLanguageServer", () =>
      runCommand(logger, async () => {
        await languageRuntime.restart();
      })
    ),
    vscode.commands.registerCommand("mos.build", () =>
      runCommand(logger, async () => executeMosTask(tasks, "build"))
    ),
    vscode.commands.registerCommand("mos.test", () =>
      runCommand(logger, async () => executeMosTask(tasks, "test"))
    ),
    vscode.commands.registerCommand(
      "mos.runApplication",
      (resource: unknown) =>
        runCommand(logger, async () => {
          await launchApplication(requireResource(resource), true);
        })
    ),
    vscode.commands.registerCommand(
      "mos.debugApplication",
      (resource: unknown) =>
        runCommand(logger, async () => {
          await launchApplication(requireResource(resource), false);
        })
    ),
    vscode.commands.registerCommand(
      "mos.runSingleTest",
      (testName: unknown) =>
        runCommand(logger, async () => {
          await runSingleTest(requireTestName(testName), true);
        })
    ),
    vscode.commands.registerCommand(
      "mos.debugSingleTest",
      (testName: unknown) =>
        runCommand(logger, async () => {
          await runSingleTest(requireTestName(testName), false);
        })
    ),
    vscode.workspace.onDidGrantWorkspaceTrust(() => {
      for (const folder of openMosWorkspaceFolders()) {
        void startRuntime(languageRuntime, logger, status, folder);
      }
    }),
    vscode.workspace.onDidOpenTextDocument((document) => {
      if (vscode.workspace.isTrusted && document.languageId === LANGUAGE_ID) {
        const folder = vscode.workspace.getWorkspaceFolder(document.uri);
        if (folder !== undefined) {
          void startRuntime(languageRuntime, logger, status, folder);
        }
      }
    }),
    vscode.workspace.onDidChangeWorkspaceFolders((event) => {
      for (const folder of event.removed) {
        void runCommand(logger, async () => {
          await languageRuntime.stopFolder(folder);
        });
      }
    }),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (
        event.affectsConfiguration("mos.executablePath") ||
        event.affectsConfiguration("mos.path") ||
        event.affectsConfiguration("mos.trace.server")
      ) {
        void runCommand(logger, async () => {
          await languageRuntime.restart();
        });
      }
      if (
        event.affectsConfiguration("mos.applicationCodeLens") ||
        event.affectsConfiguration("mos.launchConfiguration")
      ) {
        applicationCodeLens.refresh();
      }
    })
  );

  if (vscode.workspace.isTrusted) {
    for (const folder of openMosWorkspaceFolders()) {
      await startRuntime(languageRuntime, logger, status, folder);
    }
  }
}

export async function deactivate(): Promise<void> {
  await runtime?.stop();
  runtime = undefined;
}

async function startRuntime(
  languageRuntime: LanguageRuntime,
  logger: MosLogger,
  status: vscode.StatusBarItem,
  folder?: vscode.WorkspaceFolder
): Promise<void> {
  status.text = "$(sync~spin) MOS";
  try {
    await languageRuntime.ensureStarted(folder);
    status.text = "$(tools) MOS";
  } catch (error) {
    status.text = "$(error) MOS";
    logger.error("Could not start MOS", error);
    const selected = await vscode.window.showErrorMessage(
      `Could not start MOS: ${errorMessage(error)}`,
      "Show Log"
    );
    if (selected === "Show Log") {
      logger.channel.show();
    }
  }
}

async function runCommand(
  logger: MosLogger,
  command: () => Promise<void>
): Promise<void> {
  try {
    await command();
  } catch (error) {
    logger.error("MOS command failed", error);
    const selected = await vscode.window.showErrorMessage(
      `MOS: ${errorMessage(error)}`,
      "Show Log"
    );
    if (selected === "Show Log") {
      logger.channel.show();
    }
  }
}

function openMosWorkspaceFolders(): vscode.WorkspaceFolder[] {
  const folders = new Map<string, vscode.WorkspaceFolder>();
  for (const document of vscode.workspace.textDocuments) {
    if (document.languageId === LANGUAGE_ID) {
      const folder = vscode.workspace.getWorkspaceFolder(document.uri);
      if (folder !== undefined) {
        folders.set(folder.uri.toString(), folder);
      }
    }
  }
  return [...folders.values()];
}

function requireTestName(value: unknown): string {
  if (typeof value !== "string" || value.trim() === "") {
    throw new Error("The MOS language server did not provide a test name.");
  }
  return value;
}

function requireResource(value: unknown): vscode.Uri {
  if (!(value instanceof vscode.Uri)) {
    throw new Error("The application CodeLens did not provide a source file.");
  }
  return value;
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
