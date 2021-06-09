# Advanced features

## Imports
When your source file grows too large to manage, you may consider splitting it up into multiple files.

### The basics
Let's look at a file called `border.asm`:
```asm6502
set_black:
    lda #0
    sta $d020
    rts

set_white:
    lda #1
    sta $d020
    rts   
```

You can import it into your `main.asm` like so:
```asm6502
.import * from "border.asm"
```

Now, `main.asm` can use these symbols:
```asm6502
jsr set_white
rts

.import * from "border.asm"
```

::: tip
The machine code will be emitted at the location you put the `.import` directive.
:::

### More control
It's also possible to choose which symbols to import, like so:

```asm6502
jsr set_white
rts

.import set_white from "border.asm"
```

The imported symbols may also be renamed by adding an `as` directive:

```asm6502
jsr sw
rts

.import set_white as sw from "border.asm"
```

### Importing with scope
When importing, you can also choose to add a scope to your `.import` directive, allowing you to pass some data to the imported file.

For example, let's change `main.asm` to:
```asm6502
jsr set_border_white
jsr set_screen_white
rts

.import set_white as set_border_white from "border.asm" {
    .const ADDRESS = $d020
}
.import set_white as set_screen_white from "border.asm" {
    .const ADDRESS = $d021
}
```

Then `border.asm` can look like this:
```asm6502
set_white:
    lda #1
    sta ADDRESS
    rts
```

### Conditional inclusion
The parameter passing mechanism can also be used to determine which blocks of code to import, by wrapping them in an `.if defined` directive.

For instance, `border.asm` can do something like:
```asm6502
.if defined(include_set_black) {
    set_black:
        lda #0
        sta ADDRESS
        rts
}

.if defined(include_set_white) {
    set_white:
        lda #1
        sta ADDRESS
        rts
}
```

And then `main.asm` can be:
```asm6502
jsr set_border_white
jsr set_screen_white
rts

.import set_white as set_border_white from "border.asm" {
    .const ADDRESS = $d020
    .const INCLUDE_SET_WHITE = 1
}
.import set_white as set_screen_white from "border.asm" {
    .const ADDRESS = $d021
    .const INCLUDE_SET_WHITE = 1
}
```

## Macros
Macros may be defined by specifying a name and an argument list.

For instance, here's a macro shamelessly stolen from the [KickAssembler documentation](http://theweb.dk/KickAssembler/webhelp/content/ch07s02.html):

```asm6502
.macro ClearScreen(screen, clearByte) {
    lda #clearByte
    ldx #0
    {
        sta screen, x
        sta screen + $100, x
        sta screen + $200, x
        sta screen + $300, x
        inx
        bne -
    }
}
```

To invoke it, use something like:
```asm6502
ClearScreen($0400, 32)
```

Any labels, constants or variables that a macro defines appear in the scope of the caller. If you want to limit the macro's scope, you can enclose it in a scope of its own. For instance, with a label:

```asm6502
my_scope: {
    ClearScreen($0400, 32)
}
```

## Segments
Segments can be used to assemble different blocks to different memory locations. If no segment is configured, a default segment is created that starts at `$2000`.

### A simple example
Let's say we want to assemble some code to `$c000` and some data to `$2000`. We can do this by creating two segments and switching between them.

```asm6502
.define segment {
    name = code
    start = $c000
}

.define segment {
    name = data
    start = $2000
}

.segment code {
    lda $1234
}

.segment data {
    .byte 1, 2, 3, 4
}
```

If there is only one segment it will be used automatically and there is no need for the `.segment` directive.

### Segment dependencies
It's also possible to relate the start or end of a segment to another segment.

For instance, to have a segment start where another segment ends, you could do something like:

```asm6502
.define segment {
    name = code
    start = $c000
}

.define segment {
    name = data
    start = segments.code.start
}
```

### Available options

| Key | Type | Description |
| --- | ---- | ----------- |
| `name` | string | The name of the segment. It must be a valid identifier. |
| `start` | address | Where to place the resulting segment in memory. |
| `pc` | address | Use a program counter that is different from `start`. See [below](#the-pc-option) for details. |
| `write` | `true`, `false` | You can disable writing the contents of the segment to disk by setting `write` to `false` |
| `bank` | string | The bank to assign the segment to (see [below](#banks) for details). |

#### The `pc` option
It is possible to change the program counter that is used when assembling, for example if the segment will later be relocated. The program counter to use can be set with `pc`.

For example:
```asm6502
.define segment {
    name = to_relocate
    start = $4000
    pc = $8000
}
```

This segment will be assembled to `$4000` and onwards, but the assembled code will be assembled as if the code is located at `$8000` and onwards.

## Banks
Normally all segments will be merged before they are written to your output file. So, for instance, if your first segment is at $1000-$2000 and your second segment is at $8000-$8100 then your output file would be from $1000-$8100. There can be situations where you want more control over what your output looks like. For instance, when you want to write cartridge formats you need to include all kinds of headers. For this purpose you can define **banks**.

Let's take an example where you first have a 64-byte header and then the rest of your program. You could define your banks like this:

```asm6502
.define bank {
    name = header
    size = 64
    fill = 0
}
.define bank {
    name = main
}
```

The first bank (`header`) is defined to be exactly 64 bytes long, and if fewer bytes get emitted they will be padded with the fill-byte `0`.

The second bank (`main`) has no size restrictions.

::: warning
The order in which you define the banks is the order in which they will appear in your final output!
:::

How do the banks know which data to emit, though? Well, you assign segments to banks.

Let's extend the above example:
```asm6502
...bank definitions from above...

.define segment {
    name = my_header
    bank = header
}
.define segment {
    name = code
    start = $1000
}
.define segment {
    name = data
    start = $4000
}
```

As you can see, you can define your segments as usual except this time there is an additional `bank` parameter being set. We don't need to set it for the `code` and `data` segments: omitting the bank name will assign the segment to the `default` bank.

To round it all up, let's write some data to the segments:

```asm6502
...bank and segment definitions from above...

.segment my_header {
    .byte 1, 5, 8
}
.segment code {
    nop
}
.segment data {
    .byte 1, 2, 3, 4
}        
```

Based on all the configuration, your output file will now consist of a 64-byte header which contains the bytes `1, 5, 8` followed by zeroes. Then, there will be a NOP opcode, a bunch of zeroes to span the range `$1001-$3fff` and then the data segment will start which contains `1, 2, 3, 4`.

Keep in mind that what we've called a `header` bank here is just the header because we've listed it first in our bank definitions. You can have as many banks as you want!

### Limitations
Banks are not available when you've set `build.output-format` in `mos.toml` to `prg`.

### Available options

| Key | Type | Description |
| --- | ---- | ----------- |
| `name` | string | The name of the bank. It must be a valid identifier. |
| `size` | number | The size (in bytes) of the bank. If the bank is not exactly filled, you can choose to pad it with the `fill` byte. |
| `fill` | byte | The byte to fill banks with that were not exactly filled up. |
| `filename` | string | The filename of the file the bank should be written to. If you have multiple banks with different filenames then your build will generate multiple files as well. |
