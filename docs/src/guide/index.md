# Overview
MOS is an assembler targeting the MOS 6502 CPU.

::: danger
**This documentation is preliminary.**

It's just here to make sure current features at least have _some_ documentation. Everything is subject to change and will hopefully improve significantly :smile:
:::

# Getting started
Let's dive into the **MOS** pit :metal: (...sorry).

## Getting the tools
There are two ways to work with MOS. You can download the CLI executable, but a quicker way is to install the Visual Studio Code extension.

### Visual Studio Code extension
Installing the extension consists of these steps:
* Open Visual Studio Code
* In the `Extensions` menu, install the extension called `mos`.
* Open a folder and place an empty `mos.toml` in this folder. This is the main configuration file for `mos` and the presence of this file will activate the extension.
* The extension will ask if you want to install the `mos` executable automatically. Allow it to do so.
* Done! If you'd like to know more, you can find more detailed documentation [here](./ide/vscode).

### Command line
You can download the latest release of MOS [here](https://github.com/datatrash/mos/releases).

Extracting the archive will leave you with a binary called `mos`. You can put it in some easily accessible place. The rest of the documentation will assume it's in your `PATH`.

The current version of MOS only generates Commodore-compatible `.prg` files. So, let's try to build a tiny Commodore program.

## Building a sample application
Let's create a new file called `main.asm` and fill it with these contents:
```asm6502
inc $d020
rts
```
This is a tiny program that changes the border colour on a Commodore 64.

Next, you can build it like so:
```
> mos build
```

If everything went well you will see no output, but a `main.prg` file will have been created in a directory called `target`. You can load this `.prg` in your favourite C64 emulator. Run it by typing `sys 8192` in the BASIC prompt. The border colour will change, whoo!

::: tip
You may be wondering why MOS is compiling `main.asm` when no input filename was passed in. Well, you can configure the input filename and many other things by creating a configuration file called `mos.toml`. For now, let's just keep the defaults.
:::

## Errors in your code
What happens when there is an error :boom:? Well, let's edit `main.asm` and change it to this:
```asm6502
inx $d020
rts
```
`inx $d020` is not a valid instruction, and this is what you will see:
```{2}
> mos build
main.asm:1:5: error: unexpected '$d020'
```

The error indicates that it is located in file `main.asm`, on line 1, column 5. In this case, it didn't expect an operand because `inx` does not need any.

## What next?
Alright, that was the quickest possible introduction! The remainder of the documentation will go over all the features in greater detail.