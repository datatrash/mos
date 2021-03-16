# Advanced features

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