# MOS for IntelliJ

Language, build, test, and debugging support for the [MOS assembler](https://mos.datatra.sh/), powered by [LSP4IJ](https://github.com/redhat-developer/lsp4ij).

Opening an `.asm` file in a project whose root contains a `mos.toml` starts `mos lsp`. In a trusted project, unless a custom executable is configured under **Settings | Languages & Frameworks | MOS**, the plugin downloads the latest MOS release for the current platform into the IntelliJ system directory. Without a `mos.toml` the plugin stays inactive: no server is started, nothing is downloaded, and the MOS actions and CodeLens are hidden.

## Features

- Syntax and semantic highlighting, diagnostics, completion, hover, formatting, symbols, references, rename, and go to definition
- LSP CodeLens actions for running and debugging individual MOS tests
- Run and debug actions for MOS applications
- Debug Adapter Protocol integration, including source breakpoints
- Build and test actions
- Managed MOS downloads for x64 Windows and x64/ARM64 Linux and macOS

VICE 3.5 or newer is required to run or debug an application.
