import * as net from "node:net";
import * as vscode from "vscode";
import {
  LanguageClient,
  Trace,
  type LanguageClientOptions,
  type ServerOptions
} from "vscode-languageclient/node";
import type {BinaryManager} from "./binary-manager.js";
import type {MosLogger} from "./logging.js";
import {
  DEBUG_ADAPTER_HOST,
  LANGUAGE_ID,
  lspArguments
} from "./protocol.js";

export class LanguageRuntime implements vscode.Disposable {
  private readonly clients = new Map<string, RuntimeClient>();
  private operation: Promise<void> | undefined;

  constructor(
    private readonly binaries: BinaryManager,
    private readonly logger: MosLogger
  ) {}

  async ensureStarted(
    folder: vscode.WorkspaceFolder | undefined = activeWorkspaceFolder()
  ): Promise<number> {
    if (folder === undefined) {
      throw new Error("Open a MOS workspace before starting the language server.");
    }
    const key = folder.uri.toString();
    await this.runExclusively(async () => {
      if (this.clients.has(key)) {
        return;
      }

      const executable = await this.binaries.getExecutable();
      const port = await findAvailablePort();
      const serverOptions: ServerOptions = {
        command: executable,
        args: lspArguments(port),
        options: {
          cwd: folder.uri.fsPath,
          env: {...process.env}
        }
      };
      const clientOptions: LanguageClientOptions = {
        diagnosticCollectionName: "mos",
        documentSelector: [
          {
            language: LANGUAGE_ID,
            pattern: {
              baseUri: folder.uri.toString(),
              pattern: "**/*.asm"
            },
            scheme: "file"
          }
        ],
        outputChannel: this.logger.channel,
        traceOutputChannel: this.logger.channel,
        workspaceFolder: folder
      };
      const client = new LanguageClient(
        `mos-${folder.index}`,
        `MOS Language Server (${folder.name})`,
        serverOptions,
        clientOptions
      );

      try {
        await client.start();
        await client.setTrace(configuredTrace());
        this.clients.set(key, {client, folder, port});
        this.logger.info(
          `MOS language server started for ${folder.name}; debug adapter uses port ${port}.`
        );
      } catch (error) {
        await client.dispose();
        throw error;
      }
    });

    const runtimeClient = this.clients.get(key);
    if (runtimeClient === undefined) {
      throw new Error("MOS language server did not provide a debug adapter port.");
    }
    return runtimeClient.port;
  }

  async restart(): Promise<void> {
    let folders: vscode.WorkspaceFolder[] = [];
    await this.runExclusively(async () => {
      folders = [...this.clients.values()].map(({folder}) => folder);
      await this.stopClients();
    });
    if (folders.length === 0) {
      const active = activeWorkspaceFolder();
      if (active !== undefined) {
        folders.push(active);
      }
    }

    for (const folder of folders) {
      await this.ensureStarted(folder);
    }
  }

  async stopFolder(folder: vscode.WorkspaceFolder): Promise<void> {
    await this.runExclusively(async () => {
      const key = folder.uri.toString();
      const runtimeClient = this.clients.get(key);
      if (runtimeClient === undefined) {
        return;
      }
      this.clients.delete(key);
      await runtimeClient.client.dispose();
      this.logger.info(`MOS language server stopped for ${folder.name}.`);
    });
  }

  dispose(): void {
    void this.runExclusively(async () => {
      await this.stopClients();
    });
  }

  async stop(): Promise<void> {
    await this.runExclusively(async () => {
      await this.stopClients();
    });
  }

  private async stopClients(): Promise<void> {
    const clients = [...this.clients.values()];
    this.clients.clear();
    await Promise.all(
      clients.map(async ({client, folder}) => {
        await client.dispose();
        this.logger.info(`MOS language server stopped for ${folder.name}.`);
      })
    );
  }

  private async runExclusively(operation: () => Promise<void>): Promise<void> {
    const previous = this.operation;
    let release: (() => void) | undefined;
    this.operation = new Promise<void>((resolve) => {
      release = resolve;
    });
    await previous;
    try {
      await operation();
    } finally {
      release?.();
    }
  }

}

function configuredTrace(): Trace {
  switch (
    vscode.workspace
      .getConfiguration("mos")
      .get<string>("trace.server", "off")
  ) {
    case "messages":
      return Trace.Messages;
    case "verbose":
      return Trace.Verbose;
    default:
      return Trace.Off;
  }
}

interface RuntimeClient {
  readonly client: LanguageClient;
  readonly folder: vscode.WorkspaceFolder;
  readonly port: number;
}

function activeWorkspaceFolder(): vscode.WorkspaceFolder | undefined {
  const editor = vscode.window.activeTextEditor;
  return editor === undefined
    ? vscode.workspace.workspaceFolders?.[0]
    : vscode.workspace.getWorkspaceFolder(editor.document.uri);
}

function findAvailablePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.unref();
    server.once("error", reject);
    server.listen(0, DEBUG_ADAPTER_HOST, () => {
      const address = server.address();
      if (address === null || typeof address === "string") {
        server.close();
        reject(new Error("Could not allocate a debug adapter port."));
        return;
      }
      server.close((error) => {
        if (error === undefined) {
          resolve(address.port);
        } else {
          reject(error);
        }
      });
    });
  });
}
