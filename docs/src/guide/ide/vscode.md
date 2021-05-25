# Visual Studio Code extension

MOS has a Visual Studio Code extension. Follow the [project setup guide](../#visual-studio-code-extension) to install it.

Once you have it installed, the following features will become available:
* Building your program
* Launching and debugging your program in VICE
* Syntax highlighting
* Find usages
* Go to definition
* Format document
* Format on-type
* Automatic indentation

The debugger supports breakpoints, local symbols, watches and evaluating expressions.

## Building and launching
To build your application from within VSCode, use the `mos: Build` build command.

You can also automatically create a `launch.json` configuration file, but here is a manual configuration:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "mos",
            "request": "launch",
            "name": "Launch",
            "workspace": "${workspaceFolder}",
            "preLaunchTask": "mos: Build",
            "vicePath": "<path to x64/x64sc binary>",
        }
    ]
}
```

If you specify a valid path to a VICE executable in the `vicePath` option you can launch or debug your application from within Visual Studio Code.

## Options
The following plugin options are available:

| Key | Type | Description |
| --- | ---- | ----------- |
| `mos.path` | Path | By default, the extension will automatically download and update the `mos` executable. If for some reason you want to use your own `mos` executable you can fill in the path to this executable here. |