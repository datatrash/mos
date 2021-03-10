import * as vscode from 'vscode';
import {LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";
import {existsSync, promises as fs} from "fs";
import {getMosBinary} from "./download_binary";
import {log} from "./log";

let client: LanguageClient;

export async function activate(ctx: vscode.ExtensionContext) {
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

    let serverOptions: ServerOptions = {
        command: mosPath, args: ["-v", "lsp"], options: {}
    };

    let clientOptions: LanguageClientOptions = {
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

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}