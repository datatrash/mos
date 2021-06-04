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
| `name` | string | The name of a segment. It must be a valid identifier. |
| `start` | address | Where to place the resulting segment in memory. |
| `pc` | address | Use a program counter that is different from `start`. See [below](#the-pc-option) for details. |
| `write` | `true`, `false` | You can disable writing the contents of the segment to disk by setting `write` to `false` |
| `filename` | string | The filename of the file the segment should be written to. Only applies when using the `bin-segments` output format. |

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