import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {managedStoragePath} from "../storage.js";

void describe("managedStoragePath", () => {
  void it("accepts filesystem-backed storage with a non-file URI scheme", () => {
    assert.equal(
      managedStoragePath({
        fsPath: "C:\\Users\\example\\globalStorage\\datatrash.mos",
        scheme: "vscode-userdata"
      }),
      "C:\\Users\\example\\globalStorage\\datatrash.mos"
    );
  });

  void it("rejects an empty storage path", () => {
    assert.throws(
      () => managedStoragePath({fsPath: "", scheme: "vscode-userdata"}),
      /did not provide a managed storage path/
    );
  });
});
