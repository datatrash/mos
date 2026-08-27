import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {
  DEBUG_ADAPTER_HOST,
  DEBUG_TYPE,
  LANGUAGE_ID,
  lspArguments
} from "../protocol.js";

void describe("MOS protocol integration", () => {
  void it("uses the MOS 0.8.2 LSP and DAP command contract", () => {
    assert.deepEqual(lspArguments(6503), [
      "lsp",
      "--debug-adapter-port",
      "6503"
    ]);
    assert.equal(DEBUG_ADAPTER_HOST, "127.0.0.1");
    assert.equal(DEBUG_TYPE, "mos");
    assert.equal(LANGUAGE_ID, "mos6502");
  });

  void it("rejects invalid debug adapter ports", () => {
    assert.throws(() => lspArguments(0), /Invalid debug adapter port/);
    assert.throws(() => lspArguments(65_536), /Invalid debug adapter port/);
  });
});
