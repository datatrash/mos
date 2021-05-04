import * as vscode from 'vscode';
import {existsSync, promises as fs} from "fs";
import {getMosBinary} from "./auto-update/download-binary";
import {log} from "./log";
import {LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";
import {BuildTaskProvider} from "./build-task-provider";
import {
    debug,
    DebugAdapterDescriptor,
    DebugAdapterDescriptorFactory,
    DebugAdapterExecutable, DebugAdapterServer,
    DebugSession,
    ProviderResult
} from "vscode";
import * as net from "net";
import {AddressInfo} from "net";

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

    let debugAdapterPort = await findFreePort();

    buildTaskProvider = vscode.tasks.registerTaskProvider("build", new BuildTaskProvider(state));

    let serverOptions: ServerOptions = {
        command: state.mosPath, args: ["lsp", "--debug-adapter-port", debugAdapterPort.toString()], options: {}
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

    log.info(`Trying to start debug adapter on port ${debugAdapterPort}`);
    ctx.subscriptions.push(debug.registerDebugAdapterDescriptorFactory("mos", new DebuggerAdapter(debugAdapterPort)));
}

class DebuggerAdapter implements DebugAdapterDescriptorFactory {
    constructor(private port: number) {}

    createDebugAdapterDescriptor(session: DebugSession, executable: DebugAdapterExecutable | undefined): ProviderResult<DebugAdapterDescriptor> {
        return new DebugAdapterServer(this.port);
    }
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

export function deactivate(): void {
    (async () => {
        if (buildTaskProvider) {
            buildTaskProvider.dispose();
            buildTaskProvider = undefined;
        }
        if (!client) {
            return;
        }
        await client.stop();
    })();
}

function findFreePort(): Promise<number> {
    return new Promise(resolve => {
        const srv = net.createServer(sock => {
            sock.end();
        });
        srv.listen(0, () => {
            let address = <AddressInfo>srv.address()!;
            resolve(address.port);
        });
    });
}