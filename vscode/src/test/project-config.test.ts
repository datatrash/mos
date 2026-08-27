import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {parseBuildEntry} from "../project-config.js";

void describe("parseBuildEntry", () => {
  void it("reads the entry from the build section", () => {
    assert.equal(
      parseBuildEntry('[formatting]\nentry = "wrong.asm"\n[build]\nentry = "app.asm"'),
      "app.asm"
    );
  });

  void it("supports single quotes and comments", () => {
    assert.equal(
      parseBuildEntry("[build] # application\nentry = 'src/main.asm' # entry"),
      "src/main.asm"
    );
  });

  void it("supports a dotted build entry", () => {
    assert.equal(parseBuildEntry('build . entry = "src/app.asm"'), "src/app.asm");
  });

  void it("uses the MOS default entry when none is configured", () => {
    assert.equal(parseBuildEntry("[build]\nlisting = true"), "main.asm");
  });
});
