export function parseBuildEntry(toml: string): string {
  let section = "";
  for (const line of toml.split(/\r?\n/)) {
    if (section === "") {
      const dottedEntryMatch =
        /^\s*build\s*\.\s*entry\s*=\s*["']([^"']+)["']/.exec(line);
      if (dottedEntryMatch?.[1] !== undefined) {
        return dottedEntryMatch[1];
      }
    }
    const sectionMatch = /^\s*\[([^\]]+)\]\s*(?:#.*)?$/.exec(line);
    if (sectionMatch?.[1] !== undefined) {
      section = sectionMatch[1].trim();
      continue;
    }
    if (section !== "build") {
      continue;
    }
    const entryMatch = /^\s*entry\s*=\s*["']([^"']+)["']/.exec(line);
    if (entryMatch?.[1] !== undefined) {
      return entryMatch[1];
    }
  }
  return "main.asm";
}
