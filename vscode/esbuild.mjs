import * as esbuild from "esbuild";

const watch = process.argv.includes("--watch");
const context = await esbuild.context({
  bundle: true,
  entryPoints: {
    extension: "src/extension.ts",
    "test/archive.test": "src/test/archive.test.ts",
    "test/application-starts.test": "src/test/application-starts.test.ts",
    "test/github.test": "src/test/github.test.ts",
    "test/install-publisher.test": "src/test/install-publisher.test.ts",
    "test/launch-config.test": "src/test/launch-config.test.ts",
    "test/manifest.test": "src/test/manifest.test.ts",
    "test/protocol.test": "src/test/protocol.test.ts",
    "test/project-config.test": "src/test/project-config.test.ts",
    "test/release.test": "src/test/release.test.ts",
    "test/settings.test": "src/test/settings.test.ts",
    "test/storage.test": "src/test/storage.test.ts"
  },
  external: ["vscode"],
  format: "cjs",
  logLevel: "info",
  minify: !watch,
  outdir: "dist",
  platform: "node",
  sourcemap: true,
  target: "node20"
});

if (watch) {
  await context.watch();
} else {
  await context.rebuild();
  await context.dispose();
}
