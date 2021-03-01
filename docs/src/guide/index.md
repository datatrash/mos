# Overview
MOS is a command-line tool that supports assembling and formatting of assembly code for the 6502.

::: warning
**This documentation is preliminary.**

It's just here to make sure current features at least have _some_ documentation. Everything is subject to change and will hopefully improve significantly :smile:
:::

# Getting started
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
```shell
mos build test.asm
```

If everything went well you will see no output, but a `test.prg` file will have been created. You can load this `.prg` in your favourite C64 emulator. Run it by typing `sys 8192` in the BASIC prompt. The border colour will change, whoo!

## Errors in your code
What happens when there is an error? Well, let's edit `main.asm` and change it to this:
```asm6502
inx $d020
rts
```
`inx $d020` is not a valid instruction, and this is what you will see:
```shell
mos build test.asm

main.asm:1:5: error: unexpected '$d020'
```

As you can see, MOS prints a few descriptive errors. Hopefully this allows you to fix the error quickly.

## Formatting
You can also apply automatic formatting to your code:
```shell
mos format test.asm
```

Again, if everything went well you will see no output, but your source file will have been updated.

## Wrapping up
Alright, that was the quickest possible introduction, let's now dive into more details.