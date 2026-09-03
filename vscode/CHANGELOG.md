# Change Log

## 0.0.21

- A workspace is now only treated as a MOS project when `mos.toml` is at its root, matching the IntelliJ plugin. The language server, Run MOS/Debug MOS CodeLens, and build/test tasks no longer activate for folders that only contain a nested `mos.toml`.

## 0.0.20

- Replaced the legacy extension with a modern language server and debug adapter integration.
- Added managed MOS installation and user-controlled update checks.
- Added build/test tasks and inline Run MOS and Debug MOS actions.
- Added guided `launch.json` creation with a VICE executable picker.
- Preserved the legacy `mos.path` setting as a deprecated migration fallback.
