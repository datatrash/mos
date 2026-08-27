import * as path from "node:path";

export interface ReleaseAsset {
  readonly digest?: string;
  readonly downloadUrl: string;
  readonly name: string;
  readonly size?: number;
}

export interface MosRelease {
  readonly assets: readonly ReleaseAsset[];
  readonly tag: string;
}

export interface PlatformPackage {
  readonly archiveExtension: "tar.gz" | "zip";
  readonly executableName: "mos" | "mos.exe";
  readonly target: string;
}

export function platformPackage(
  platform: NodeJS.Platform,
  architecture: string
): PlatformPackage | undefined {
  if (architecture !== "x64") {
    return undefined;
  }

  switch (platform) {
    case "win32":
      return {
        archiveExtension: "zip",
        executableName: "mos.exe",
        target: "x86_64-pc-windows-msvc"
      };
    case "linux":
      return {
        archiveExtension: "tar.gz",
        executableName: "mos",
        target: "x86_64-unknown-linux-musl"
      };
    case "darwin":
      return {
        archiveExtension: "tar.gz",
        executableName: "mos",
        target: "x86_64-apple-darwin"
      };
    default:
      return undefined;
  }
}

export function selectReleaseAsset(
  release: MosRelease,
  platform: PlatformPackage
): ReleaseAsset {
  const tag = release.tag.replace(/^v/, "");
  const candidates = [
    `mos-${tag}-${platform.target}.${platform.archiveExtension}`,
    `mos-${platform.target}.${platform.archiveExtension}`
  ];
  const asset = release.assets.find(({name}) => candidates.includes(name));
  if (asset === undefined) {
    throw new Error(
      `MOS ${release.tag} does not provide a ${platform.target} archive.`
    );
  }
  return asset;
}

export function releaseUpdateAvailable(
  installedTag: string,
  latestTag: string
): boolean {
  return normalizeTag(installedTag) !== normalizeTag(latestTag);
}

export function isSafeArchivePath(entryPath: string): boolean {
  const normalized = path.posix.normalize(entryPath.replaceAll("\\", "/"));
  return (
    normalized !== ".." &&
    !normalized.startsWith("../") &&
    !normalized.startsWith("/") &&
    !/^[A-Za-z]:\//.test(normalized)
  );
}

function normalizeTag(tag: string): string {
  return tag.trim().replace(/^v/i, "");
}
