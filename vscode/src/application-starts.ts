export interface ApplicationStart {
  readonly kind: "basic-label" | "program-counter";
  readonly length: number;
  readonly offset: number;
}

export function findApplicationStarts(source: string): ApplicationStart[] {
  const searchable = maskCommentsAndStrings(source);
  const starts = new Map<number, ApplicationStart>();
  const basicStartPattern =
    /\bbasic_start\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)/g;

  for (
    let match = basicStartPattern.exec(searchable);
    match !== null;
    match = basicStartPattern.exec(searchable)
  ) {
    const label = match[1];
    if (label === undefined) {
      continue;
    }
    const labelPattern = new RegExp(
      `^\\s*(${escapeRegExp(label)})\\s*:`,
      "m"
    );
    const labelMatch = labelPattern.exec(searchable);
    if (labelMatch?.index !== undefined && labelMatch[1] !== undefined) {
      const offset = labelMatch.index + labelMatch[0].indexOf(labelMatch[1]);
      starts.set(offset, {
        kind: "basic-label",
        length: label.length,
        offset
      });
    }
  }

  const programCounterPattern = /^(\s*)(\*)\s*=/gm;
  for (
    let match = programCounterPattern.exec(searchable);
    match !== null;
    match = programCounterPattern.exec(searchable)
  ) {
    const indentation = match[1] ?? "";
    const offset = match.index + indentation.length;
    starts.set(offset, {
      kind: "program-counter",
      length: 1,
      offset
    });
  }

  return [...starts.values()].sort((left, right) => left.offset - right.offset);
}

function maskCommentsAndStrings(source: string): string {
  // VS Code offsets use UTF-16 code units, so splitting by code unit is intentional.
  const chars = source.split("");
  let state: "block-comment" | "code" | "line-comment" | "string" = "code";
  let blockCommentDepth = 0;
  let escaped = false;

  for (let index = 0; index < chars.length; index += 1) {
    const current = chars[index];
    const next = chars[index + 1];
    if (current === undefined) {
      continue;
    }

    if (state === "line-comment") {
      if (current === "\n") {
        state = "code";
      } else {
        chars[index] = " ";
      }
      continue;
    }
    if (state === "block-comment") {
      chars[index] = current === "\n" ? "\n" : " ";
      if (current === "/" && next === "*") {
        chars[index + 1] = " ";
        index += 1;
        blockCommentDepth += 1;
      } else if (current === "*" && next === "/") {
        chars[index + 1] = " ";
        index += 1;
        blockCommentDepth -= 1;
        if (blockCommentDepth === 0) {
          state = "code";
        }
      }
      continue;
    }
    if (state === "string") {
      chars[index] = current === "\n" ? "\n" : " ";
      if (!escaped && current === "\"") {
        state = "code";
      }
      escaped = !escaped && current === "\\";
      continue;
    }
    if (current === "/" && next === "/") {
      chars[index] = " ";
      chars[index + 1] = " ";
      index += 1;
      state = "line-comment";
    } else if (current === "/" && next === "*") {
      chars[index] = " ";
      chars[index + 1] = " ";
      index += 1;
      blockCommentDepth = 1;
      state = "block-comment";
    } else if (current === "\"") {
      chars[index] = " ";
      escaped = false;
      state = "string";
    }
  }
  return chars.join("");
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}
