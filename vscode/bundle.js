require('esbuild').build({
    entryPoints: ['src/extension.ts'],
    bundle: true,
    platform: 'node',
    external: ['vscode'],
    minify: true,
    treeShaking: true,
    outdir: 'out',
}).catch(e => {
    console.log(e);
    process.exit(1);
})