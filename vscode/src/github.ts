import {createHash} from "node:crypto";
import {createWriteStream, promises as fs} from "node:fs";
import type {IncomingMessage} from "node:http";
import {get, type RequestOptions} from "node:https";
import {pipeline} from "node:stream/promises";
import {HttpsProxyAgent} from "https-proxy-agent";
import type {CancellationToken, Progress} from "vscode";
import type {MosRelease, ReleaseAsset} from "./release.js";

const API_URL = "https://api.github.com/repos/datatrash/mos/releases/latest";
const MAX_REDIRECTS = 5;

export interface NetworkOptions {
  readonly proxyUrl?: string;
  readonly strictSsl: boolean;
}

interface DownloadProgress {
  readonly increment?: number;
  readonly message?: string;
}

export async function fetchLatestRelease(
  token: CancellationToken,
  network: NetworkOptions
): Promise<MosRelease> {
  const body = await requestBuffer(
    API_URL,
    {
      headers: {
        Accept: "application/vnd.github+json",
        "User-Agent": "datatrash.mos",
        "X-GitHub-Api-Version": "2022-11-28"
      }
    },
    token,
    network
  );
  const parsed: unknown = JSON.parse(body.toString("utf8"));
  return parseRelease(parsed);
}

export async function downloadAsset(
  asset: ReleaseAsset,
  destination: string,
  progress: Progress<DownloadProgress>,
  token: CancellationToken,
  network: NetworkOptions
): Promise<void> {
  const expectedDigest = parseSha256Digest(asset.digest);
  const response = await requestStream(
    asset.downloadUrl,
    {
      headers: {
        Accept: "application/octet-stream",
        "User-Agent": "datatrash.mos"
      }
    },
    token,
    network
  );
  const contentLength = Number(response.headers["content-length"] ?? asset.size);
  const hash = createHash("sha256");
  let downloaded = 0;
  let reported = 0;

  response.on("data", (chunk: Buffer) => {
    hash.update(chunk);
    downloaded += chunk.length;
    if (Number.isFinite(contentLength) && contentLength > 0) {
      const percentage = Math.floor((downloaded / contentLength) * 100);
      progress.report({
        increment: percentage - reported,
        message: `${percentage}%`
      });
      reported = percentage;
    }
  });

  const cancellation = token.onCancellationRequested(() => {
    response.destroy(new Error("The MOS download was cancelled."));
  });
  try {
    await pipeline(response, createWriteStream(destination, {flags: "wx"}));
  } finally {
    cancellation.dispose();
  }
  const actualDigest = hash.digest("hex");
  if (
    expectedDigest !== undefined &&
    actualDigest.toLowerCase() !== expectedDigest.toLowerCase()
  ) {
    await fs.rm(destination, {force: true});
    throw new Error(`Checksum validation failed for ${asset.name}.`);
  }
}

export function parseSha256Digest(digest: string | undefined): string | undefined {
  if (digest === undefined) {
    return undefined;
  }
  const value = /^sha256:([a-fA-F0-9]{64})$/.exec(digest)?.[1];
  if (value === undefined) {
    throw new Error(`Unsupported release digest: ${digest}`);
  }
  return value;
}

export function parseRelease(value: unknown): MosRelease {
  if (!isRecord(value) || typeof value.tag_name !== "string") {
    throw new Error("GitHub returned release metadata without a tag.");
  }
  if (!Array.isArray(value.assets)) {
    throw new Error("GitHub returned release metadata without assets.");
  }

  const assets = value.assets.map((asset): ReleaseAsset => {
    if (
      !isRecord(asset) ||
      typeof asset.name !== "string" ||
      typeof asset.browser_download_url !== "string"
    ) {
      throw new Error("GitHub returned malformed release asset metadata.");
    }
    return {
      digest: typeof asset.digest === "string" ? asset.digest : undefined,
      downloadUrl: asset.browser_download_url,
      name: asset.name,
      size: typeof asset.size === "number" ? asset.size : undefined
    };
  });

  return {assets, tag: value.tag_name};
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

async function requestBuffer(
  url: string,
  options: RequestOptions,
  token: CancellationToken,
  network: NetworkOptions
): Promise<Buffer> {
  const response = await requestStream(url, options, token, network);
  const chunks: Buffer[] = [];
  for await (const chunk of response) {
    chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk as Uint8Array));
  }
  return Buffer.concat(chunks);
}

async function requestStream(
  url: string,
  options: RequestOptions,
  token: CancellationToken,
  network: NetworkOptions,
  redirects = 0
): Promise<IncomingMessage> {
  if (token.isCancellationRequested) {
    throw new Error("The MOS download was cancelled.");
  }

  const parsedUrl = new URL(url);
  if (parsedUrl.protocol !== "https:") {
    throw new Error(`Refusing an insecure download URL: ${parsedUrl.protocol}`);
  }

  return new Promise((resolve, reject) => {
    const requestOptions: RequestOptions = {
      ...options,
      agent:
        network.proxyUrl === undefined
          ? undefined
          : new HttpsProxyAgent(network.proxyUrl, {
              rejectUnauthorized: network.strictSsl
            }),
      rejectUnauthorized: network.strictSsl
    };
    const request = get(parsedUrl, requestOptions, (response) => {
      const status = response.statusCode ?? 0;
      if (status >= 300 && status < 400 && response.headers.location !== undefined) {
        response.resume();
        if (redirects >= MAX_REDIRECTS) {
          reject(new Error("Too many redirects while downloading MOS."));
          return;
        }
        const redirect = new URL(response.headers.location, parsedUrl).toString();
        void requestStream(
          redirect,
          options,
          token,
          network,
          redirects + 1
        ).then(resolve, reject);
        return;
      }
      if (status < 200 || status >= 300) {
        response.resume();
        reject(new Error(`GitHub returned HTTP ${status} for ${parsedUrl.pathname}.`));
        return;
      }
      resolve(response);
    });
    request.once("error", reject);
    const cancellation = token.onCancellationRequested(() => {
      request.destroy(new Error("The MOS download was cancelled."));
    });
    request.once("close", () => {
      cancellation.dispose();
    });
  });
}
