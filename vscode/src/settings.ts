export interface SelectedExecutablePath {
  readonly path: string;
  readonly usesLegacySetting: boolean;
}

export function selectExecutablePath(
  executablePath: string | undefined,
  legacyPath: string | undefined
): SelectedExecutablePath | undefined {
  const current = executablePath?.trim() ?? "";
  if (current !== "") {
    return {path: current, usesLegacySetting: false};
  }
  const legacy = removeMatchingQuotes(legacyPath?.trim() ?? "");
  return legacy === ""
    ? undefined
    : {path: legacy, usesLegacySetting: true};
}

function removeMatchingQuotes(value: string): string {
  const first = value[0];
  return value.length >= 2 &&
    (first === "\"" || first === "'") &&
    value.at(-1) === first
    ? value.slice(1, -1)
    : value;
}
