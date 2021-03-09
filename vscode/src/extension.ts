import * as vscode from 'vscode';
import {LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";
import * as path from "path";
import * as fs from "fs";

let client: LanguageClient;

export async function activate(_context: vscode.ExtensionContext) {
    let mosPath;
    while (true) {
        let cfg = vscode.workspace.getConfiguration("mos");
        mosPath = cfg.get<string>("path", "mos");

        if (!path.isAbsolute(mosPath)) {
            // Relative path, so make it absolute from the workspace folder
            if (vscode.workspace.workspaceFolders !== undefined) {
                let workspacePath = vscode.workspace.workspaceFolders[0].uri.fsPath;
                mosPath = path.join(workspacePath, mosPath);
            }
        }

        if (fs.existsSync(mosPath)) {
            break;
        } else {
            await vscode.window.showErrorMessage("Could not find mos executable. Please configure the mos.path setting.", "Retry");
        }
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