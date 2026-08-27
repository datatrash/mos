export interface StorageUri {
  readonly fsPath: string;
  readonly scheme: string;
}

export function managedStoragePath(uri: StorageUri): string {
  if (uri.fsPath.trim() === "") {
    throw new Error("VS Code did not provide a managed storage path.");
  }
  return uri.fsPath;
}
