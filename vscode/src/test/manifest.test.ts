import assert from "node:assert/strict";
import {describe, it} from "node:test";
import manifest from "../../package.json";
import {DEBUG_TYPE, LANGUAGE_ID} from "../protocol.js";

void describe("extension manifest", () => {
  void it("activates for MOS files and contributes the expected language", () => {
    const language = manifest.contributes.languages.find(
      ({id}) => id === LANGUAGE_ID
    );
    assert.ok(language);
    assert.ok(language.extensions.includes(".asm"));
    assert.equal(manifest.main, "./dist/extension.js");
    assert.deepEqual(manifest.extensionKind, ["workspace"]);
  });

  void it("wires the MOS debugger and build/test tasks", () => {
    assert.ok(
      manifest.contributes.debuggers.some(({type}) => type === DEBUG_TYPE)
    );
    assert.ok(
      manifest.contributes.taskDefinitions.some(({type}) => type === "mos")
    );
    assert.ok(
      manifest.contributes.commands.some(({command}) => command === "mos.build")
    );
    assert.ok(
      manifest.contributes.commands.some(({command}) => command === "mos.test")
    );
    assert.ok(
      manifest.contributes.commands.some(
        ({command}) => command === "mos.runApplication"
      )
    );
    assert.ok(
      manifest.contributes.commands.some(
        ({command}) => command === "mos.debugApplication"
      )
    );
  });

  void it("keeps executable settings restricted in untrusted workspaces", () => {
    assert.equal(
      manifest.capabilities.untrustedWorkspaces.supported,
      "limited"
    );
    assert.ok(
      manifest.capabilities.untrustedWorkspaces.restrictedConfigurations.includes(
        "mos.executablePath"
      )
    );
    assert.ok(
      manifest.capabilities.untrustedWorkspaces.restrictedConfigurations.includes(
        "mos.path"
      )
    );
  });

  void it("preserves the published extension identity and legacy path setting", () => {
    assert.equal(manifest.publisher, "datatrash");
    assert.equal(manifest.name, "mos");
    assert.equal(manifest.version, "0.0.20");
    assert.ok("mos.path" in manifest.contributes.configuration.properties);
  });

  void it("checks for updates without silently auto-installing them", () => {
    assert.equal(
      manifest.contributes.configuration.properties["mos.checkForUpdates"]
        .default,
      true
    );
    assert.equal(
      "mos.autoUpdate" in manifest.contributes.configuration.properties,
      false
    );
  });

  void it("uses maintained runtime dependencies", () => {
    assert.equal("extract-zip" in manifest.dependencies, false);
    assert.ok("vscode-languageclient" in manifest.dependencies);
    assert.ok("yauzl" in manifest.dependencies);
  });
});
