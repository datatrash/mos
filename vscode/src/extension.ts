import * as vscode from 'vscode';
import {existsSync, promises as fs} from "fs";
import {getMosBinary} from "./download-binary";
import {log} from "./log";
import {LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";
import {BuildTaskProvider} from "./build-task-provider";

let client: LanguageClient;

let buildTaskProvider: vscode.Disposable | undefined;

export class State {
    workspaceFolder!: vscode.WorkspaceFolder;
    mosPath!: string;
}

export async function activate(ctx: vscode.ExtensionContext) {
    let state = await getState(ctx);
    if (!state) {
        return;
    }

    buildTaskProvider = vscode.tasks.registerTaskProvider("build", new BuildTaskProvider(state));

    let serverOptions: ServerOptions = {
        command: state.mosPath, args: ["-v", "lsp"], options: {}
    };

    let clientOptions: LanguageClientOptions = {
        diagnosticCollectionName: "mos",
        documentSelector: [{scheme: 'file', language: 'asm'}],
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'mos',
        'MOS Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

async function getState(ctx: vscode.ExtensionContext): Promise<State | undefined> {
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
    if (!workspaceFolder) {
        return;
    }

    await fs.mkdir(ctx.globalStorageUri.fsPath, { recursive: true });

    let mosPath;
    while (true) {
        mosPath = await getMosBinary(ctx);
        if (!mosPath) {
            // User chose not to locate or download a binary, so bail
            log.info("No MOS binary found or downloaded. Extension will not activate.");
            return;
        }

        if (existsSync(mosPath)) {
            break;
        }

        await vscode.window.showErrorMessage("Could not find MOS executable. Please configure the mos.path setting or leave it blank to automatically download the latest version.", "Retry");
    }
    log.info(`Using mos executable: ${mosPath}`);

    let state = new State();
    state.workspaceFolder = workspaceFolder;
    state.mosPath = mosPath;
    return state;
}

export function deactivate(): Thenable<void> | undefined {
    if (buildTaskProvider) {
        buildTaskProvider.dispose();
        buildTaskProvider = undefined;
    }
    if (!client) {
        return undefined;
    }
    return client.stop();
}