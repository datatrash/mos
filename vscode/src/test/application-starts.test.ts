import assert from "node:assert/strict";
import {describe, it} from "node:test";
import {findApplicationStarts} from "../application-starts.js";

void describe("findApplicationStarts", () => {
  void it("places a start position on the label referenced by basic_start", () => {
    const source = `
      basic_start(entry_point)

entry_point: lda #0
`;
    const starts = findApplicationStarts(source);

    assert.deepEqual(starts, [
      {
        kind: "basic-label",
        length: "entry_point".length,
        offset: source.indexOf("entry_point:")
      }
    ]);
  });

  void it("places start positions on program-counter assignments", () => {
    const source = "* = $0801\nnop\n    * = $c000\nbrk";
    assert.deepEqual(
      findApplicationStarts(source).map(({kind, offset}) => ({kind, offset})),
      [
        {kind: "program-counter", offset: 0},
        {kind: "program-counter", offset: source.lastIndexOf("*")}
      ]
    );
  });

  void it("ignores markers in comments and strings", () => {
    const source = `
// basic_start(fake)
/* * = $c000 */
.text ascii "basic_start(fake) * = 1"
`;
    assert.deepEqual(findApplicationStarts(source), []);
  });

  void it("ignores markers inside nested block comments", () => {
    const source = "/* outer /* inner */\n* = $c000\n*/";
    assert.deepEqual(findApplicationStarts(source), []);
  });

  void it("requires a matching basic_start label", () => {
    assert.deepEqual(findApplicationStarts("basic_start(missing)\nnop"), []);
  });

  void it("keeps VS Code UTF-16 offsets when Unicode precedes a start", () => {
    const source = "// 😀\nbasic_start(entry)\nentry: nop";
    assert.equal(
      findApplicationStarts(source)[0]?.offset,
      source.indexOf("entry:")
    );
  });
});
