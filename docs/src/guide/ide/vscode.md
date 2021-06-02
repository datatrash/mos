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
* Show function documentation on hover

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

## Running tests
The extension provides a "run all tests" extension that allows you to run all tests in your project.

When you define a test (using the `.test` directive) a CodeLens will appear which will allow you to run or debug that specific test using an emulated 6502.

## Showing documentation on hover
You can annotate a symbol with a special `///` comment syntax. When you hover over the symbol somewhere in your code the comment will be shown as markdown.

So, for instance:
```asm6502
/// This subroutine is **awesome**.
my_subroutine: {
    rts
}

jsr my_subroutine // <-- hovering over my_subroutine will show the documentation
```

## Watch expressions
You can add watch expressions to keep track of the value of symbols whenever you hit a breakpoint.

You can also access a few extra symbols that allow you to access CPU registers and memory locations:

| Key | Description |
| --- | ----------- |
| cpu.a | The accumulator (A) register |
| cpu.x | The X register |
| cpu.y | The Y register |
| cpu.sp | THe stack pointer |
| * | The program counter |
| cpu.flags.zero | The Z flag |
| cpu.flags.carry | The C flag |
| cpu.flags.interrupt_disable | The I flag |
| cpu.flags.decimal | The D flag |
| cpu.flags.overflow | The V flag |
| cpu.flags.negative | The N flag |
| ram(...) | Read a byte from ram, e.g. `ram($d020)` |
| ram16(...) | Read a word from ram, e.g. `ram16($0314)` |

## Options
The following plugin options are available:

| Key | Type | Description |
| --- | ---- | ----------- |
| `mos.path` | Path | By default, the extension will automatically download and update the `mos` executable. If for some reason you want to use your own `mos` executable you can fill in the path to this executable here. |