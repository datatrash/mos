# Command-line interface

The MOS binary has a command-line interface. `mos` can be invoked with various subcommands.

Many of these subcommands don't have any parameters, since all configuration is read from the `mos.toml` project configuration file.

## build
To build your application call `mos build`.

## format
To format the source code of your application in-place, call `mos format`.

## init
To create an empty project configuration, file, call `mos init`.

## lsp
To launch the [Language Server](https://microsoft.github.io/language-server-protocol/), call `mos lsp`. Typically you will only do this if you are developing an IDE plugin.