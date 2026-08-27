import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {
  createMosLaunchConfiguration,
  hasLaunchConfiguration,
  parseLaunchConfigurations,
  writableLaunchConfigurations
} from "../launch-config.js";

void describe("launch configuration helpers", () => {
  void it("creates the default MOS launch configuration", () => {
    assert.deepEqual(
      createMosLaunchConfiguration("Launch with MOS", "C:\\VICE\\x64sc.exe"),
      {
        type: "mos",
        request: "launch",
        name: "Launch with MOS",
        workspace: "${workspaceFolder}",
        preLaunchTask: "mos: Build",
        vicePath: "C:\\VICE\\x64sc.exe"
      }
    );
  });

  void it("preserves and finds existing configurations", () => {
    const configurations = parseLaunchConfigurations([
      {type: "node", name: "Existing"}
    ]);
    assert.equal(hasLaunchConfiguration(configurations, "Existing"), true);
    assert.equal(hasLaunchConfiguration(configurations, "Launch with MOS"), false);
  });

  void it("accepts a missing configurations property", () => {
    assert.deepEqual(parseLaunchConfigurations(undefined), []);
  });

  void it("rejects malformed configurations without overwriting them", () => {
    assert.throws(
      () => parseLaunchConfigurations({name: "Existing"}),
      /must be an array of objects/
    );
  });

  void it("writes only folder configurations in a multi-root workspace", () => {
    const workspace = [{name: "Shared"}];
    const folder = [{name: "Folder"}];
    assert.deepEqual(
      writableLaunchConfigurations(workspace, folder, true),
      folder
    );
  });

  void it("preserves workspace configurations in a single-folder workspace", () => {
    const workspace = [{name: "Existing"}];
    assert.deepEqual(
      writableLaunchConfigurations(workspace, undefined, false),
      workspace
    );
  });
});
