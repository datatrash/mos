# Project setup

You can create a new MOS project by creating a file called `mos.toml` in the root of your project. This is the MOS **configuration file** and it allows you to customize things like how MOS assembles and formats your project.

The simplest way to create this file is via MOS itself:
```
> mos init
```

This creates a `mos.toml` file with a few basic settings. This is enough to get started, but you can customize these settings too.

## Build options
These are the default options under the `build` section in `mos.toml`:

```toml
[build]
entry = "main.asm"
target-directory = "target"
listing = false
symbols = []
output-format = "prg"
```

| Key | Type | Description |
| --- | ---- | ----------- |
| `entry` | filename | The source file from which MOS should start assembling |
| `target-directory` | directory name | The directory in which all output (binaries, symbols) is placed
| `listing` | boolean | Generate listing files, containing disassembled code? |
| `symbols` | array | Which symbol files to generate. Currently only `"vice"` is supported.
| output-filenaame | string | In case the output format results in a single file, you can specify the output filename here. Otherwise the name of the entry source file will be used. (with a different extension). |
| output-format | `"prg`", `"bin`", `"bin-segments"` | See below for details. |

So, if you want to leave all defaults as-is, but would want to generate symbols for Vice, the `build` section in your `mos.toml` would look like this:

```toml
[build]
symbols = ["vice"]
```

### Output format
The available output formats are:

| Format | Description |
| ------ | ----------- |
| `"prg"` | This will write all segments merged together in one file, with a 2-byte header indicating the load address. |
| `"bin"` | This will write all segments merged together in one file without any kind of header. |
| `"bin-segments"` | This will write all segments to separate files, unless a segment's filename is explicitly specified. In that case all segments that share the same filename will be merged together. |

## Formatting options
The formatter has a few options you can tweak, but it is not extensive yet. The following `mos.toml` represents the default formatting options:

```toml
[formatting]
mnemonics.casing = 'lowercase'
mnemonics.register-casing = 'lowercase'
braces.position = 'same-line'
whitespace.indent = 4
whitespace.label-margin = 20
whitespace.label-alignment = right
whitespace.code-margin = 30
listing.num-bytes-per-line = 8
```

| Key | Type | Description |
| --- | ---- | ----------- |
| mnemonics.casing | `uppercase`, `lowercase` | The casing of mnemonics (e.g. `NOP`) |
| mnemonics.register-casing | `uppercase`, `lowercase` | The casing of register suffixes (e.g. the `x` in `lda $fb,x`) |
| braces.position | `same_line`, `new_line` | Where to place the braces in things like if statements |
| whitespace.indent | number | How many spaces (no tabs yet... :innocent:) to indent in block statements |
| whitespace.label-margin | number | How many characters to reserve for labels |
| whitespace.label-alignment | `left`, `right` | How to align labels |
| whitespace.code-margin | number | How many characters to reserve for code (the rest is reserved for comments) |
| listing.num-bytes-per-line | number | When generating a listing file, this specifies how many bytes should be emitted per line |