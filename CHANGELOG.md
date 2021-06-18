# Changelog

## 0.7.0 (2021-06-18)

### New features
* "Banks" can be used to control how the final binary is laid out (e.g. to create cartridge files)
* Strings can be used as symbols, with support for string interpolation and concatenation
* Error formatting is improved and made configurable through the `--error-style` option
* A listing file can be generated for every segment by specifying `listing = true` in the `[build]` section of `mos.toml`
* Instead of just generating a Commodore-compatible `.prg` file, it is now also possible to generate a raw binary or a raw binary per segment

### Notable bugfixes
* Nested macros are now looked up correctly ([#168](https://github.com/datatrash/mos/issues/168))
* Symbol resolution doesn't return incorrect symbols when lookups fail halfway through
* Error reporting shows correct paths on Windows now, instead of UNC paths ([#192](https://github.com/datatrash/mos/issues/192)) 
* Files imported from subdirectories are resolved in a platform-independent way (i.e. forward or backslashes may be used)
* Uninvoked macros and untaken if/else paths now get proper refactoring support ([#180](https://github.com/datatrash/mos/issues/180))
* When attempting to debug with an unsupported version of VICE a proper error message is created ([#186](https://github.com/datatrash/mos/issues/186))
* Syntax grammar files were not packaged in the VSCode extension ([#185](https://github.com/datatrash/mos/issues/185))

For a full list of changes please refer to the [0.7.0 milestone on GitHub](https://github.com/datatrash/mos/milestone/8?closed=1).

## 0.6.0 (2021-06-02)

### New features
* Support for writing unit tests
* Support for running unit tests in an emulated 6502
* Support for running and debugging unit tests from within Visual Studio Code
* Symbols may be annotated with markdown comments, which will show on hover

### Bugfixes
* Various scoping issues have been resolved

## 0.5.0 (2021-05-25)

### New features
* Support for debugging in Visual Studio Code (via the Debug Adapter protocol)
* Formatting now works with label/code/comment columns
* Can now use - and + to refer to start and end of scopes
* Visual Studio Code: Now provides a 'build' task and associated problem matcher

### Bugfixes
* Fixed a number of edge cases related to code completion
* Code completion doesn't seem to work inside a (segment) block
* Fix forward references to other segments
* Fix infinite loop with label before end of block
* Formatting: No empty lines after a non-block label
* Formatting: Empty line after a block
* C/C++-style comments are not highlighted nicely
* LSP not shutting down correctly
* Error output is not sorted by line/file

## 0.4.1 (2021-03-26)

### Bugfixes
* Formatting now works correctly for projects containing multiple files

## 0.4.0 (2021-03-24)

## 0.3.1 (2021-03-21)

### Bugfixes
* Linux version now shows correct version information

## 0.3.0 (2021-03-16)

### New features
* Support for macros
* Support for loops
* Can now use `-` and `+` to refer to start and end of scopes
* Visual Studio Code: Now provides a 'build' task and associated problem matcher

### Bugfixes
* Rewrote code generation to be both simpler and more accurate
* The `no-color` parameter now works as expected

## 0.2.0 (2021-03-11)

### New features
* Project configuration file is now required in lieu of command-line arguments
* Language Server support

## 0.1.0 (2021-02-22)

First public alpha release!