export const DEBUG_ADAPTER_HOST = "127.0.0.1";
export const DEBUG_TYPE = "mos";
export const LANGUAGE_ID = "mos6502";

export function lspArguments(debugAdapterPort: number): string[] {
  if (
    !Number.isInteger(debugAdapterPort) ||
    debugAdapterPort < 1 ||
    debugAdapterPort > 65_535
  ) {
    throw new Error(`Invalid debug adapter port: ${debugAdapterPort}`);
  }
  return ["lsp", "--debug-adapter-port", String(debugAdapterPort)];
}
