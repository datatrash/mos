require('esbuild').build({
    entryPoints: ['src/extension.ts'],
    bundle: true,
    platform: 'node',
    external: ['vscode'],
    minify: true,
    treeShaking: true,
    outfile: 'out/extension.js',
}).catch(e => {
    console.log(e);
    process.exit(1);
})