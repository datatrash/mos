# MOS for Visual Studio Code

Modern language, build, test, and debugging support for the [MOS assembler](https://mos.datatra.sh/).

Opening an `.asm` file activates the extension. In a trusted workspace, it downloads the latest MOS release for the current platform into VS Code's extension storage and starts `mos lsp`. On later starts it checks GitHub for a newer release and asks before updating. Set `mos.executablePath` to use a custom build instead.

Version 0.0.20 replaces the original `datatrash.mos` implementation while preserving its Marketplace identity, so existing installations upgrade in place. The legacy `mos.path` setting remains supported; new configurations should use `mos.executablePath`.

## Features

- Syntax highlighting, diagnostics, completion, hover information, formatting, symbols, references, and rename through the MOS language server
- Build and test tasks using process execution and the MOS problem matcher
- Debug Adapter Protocol integration for VICE and MOS unit tests
- Automatic, cancellable updates with atomic installation and SHA-256 verification when GitHub supplies a digest
- Local, Remote SSH, Dev Container, WSL, and Codespaces support
- Multi-root support with an isolated language/debug server pair for each workspace folder
- Inline **Run MOS** and **Debug MOS** actions at `basic_start` target labels and `* = ...` program-counter assignments in the build entry file
- Restricted Mode support: highlighting remains available, but binaries are not downloaded or executed

Managed downloads are available for x64 Windows and for x64 or ARM64 Linux and macOS. Other platforms can use `mos.executablePath`.

## Commands

- **MOS: Build**
- **MOS: Run All Tests**
- **MOS: Install or Update Toolchain**
- **MOS: Restart Language Server**

The inline application actions use the launch configuration named **Launch with MOS** by default. Change `mos.launchConfiguration` when your `.vscode/launch.json` uses a different name. If the configuration is missing, the extension offers to create it and prompts you to select your VICE executable.

## Debugging

Create a `launch.json` configuration:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "mos",
      "request": "launch",
      "name": "Launch with MOS",
      "workspace": "${workspaceFolder}",
      "preLaunchTask": "mos: Build",
      "vicePath": "/path/to/x64sc"
    }
  ]
}
```

VICE 3.5 or newer is required for its binary monitor support.

## Development

Use Node.js 22 or newer.

```console
npm install
npm run check
npm run package
```

The packaged VSIX is written to the repository's `target` directory.
