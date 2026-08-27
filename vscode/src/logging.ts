import * as vscode from "vscode";

export class MosLogger implements vscode.Disposable {
  readonly channel: vscode.LogOutputChannel;

  constructor() {
    this.channel = vscode.window.createOutputChannel("MOS", {log: true});
  }

  dispose(): void {
    this.channel.dispose();
  }

  info(message: string): void {
    this.channel.info(message);
  }

  warn(message: string): void {
    this.channel.warn(message);
  }

  error(message: string, error?: unknown): void {
    const details =
      error instanceof Error
        ? error.message
        : typeof error === "string"
          ? error
          : error === undefined
            ? ""
            : JSON.stringify(error);
    this.channel.error(details === "" ? message : `${message}: ${details}`);
  }
}
