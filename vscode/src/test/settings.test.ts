import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {selectExecutablePath} from "../settings.js";

void describe("selectExecutablePath", () => {
  void it("prefers the current executable setting", () => {
    assert.deepEqual(selectExecutablePath(" current-mos ", "legacy-mos"), {
      path: "current-mos",
      usesLegacySetting: false
    });
  });

  void it("migrates the legacy executable setting", () => {
    assert.deepEqual(selectExecutablePath("", " legacy-mos "), {
      path: "legacy-mos",
      usesLegacySetting: true
    });
  });

  void it("migrates quoted legacy executable paths", () => {
    assert.deepEqual(
      selectExecutablePath("", '"C:\\Program Files\\MOS\\mos.exe"'),
      {
        path: "C:\\Program Files\\MOS\\mos.exe",
        usesLegacySetting: true
      }
    );
  });

  void it("uses managed installation when neither setting is configured", () => {
    assert.equal(selectExecutablePath(undefined, "  "), undefined);
  });
});
