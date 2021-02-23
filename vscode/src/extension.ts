import * as vscode from 'vscode';
import {LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";
import log from "./log";
import * as path from "path";

let client: LanguageClient;

export function activate(_context: vscode.ExtensionContext) {
    let extensionPath = vscode.extensions.getExtension("datatra.sh.mos")!.extensionPath;
    let mosPath = path.join(extensionPath, "..", "target", "debug", "mos");
    log.appendLine("Trying to launch MOS language server from: " + mosPath);

    let serverOptions: ServerOptions = {
        command: mosPath, args: ["lsp"], options: {}
    };

    let clientOptions: LanguageClientOptions = {
        documentSelector: [{scheme: 'file', language: 'asm'}],
        synchronize: {}
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