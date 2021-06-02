# Changelog

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