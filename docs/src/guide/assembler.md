# Assembler
The assembler allows you to write regular 6502 assembly instructions. However, some more powerful features are of course also available.

## Labels
Labels can be defined to make it easier to refer to memory locations. Labels should consist of a valid identifier followed by a colon. A valid identifier starts with a character or underscore and may contain only characters, underscores or numbers.

For example, a label can be used to loop:
```asm6502
ldx #$20
label:
dex
bne label
```

You can also add braces after the label to aid formatting:
```asm6502
ldx #$20
label: {
    dex
    bne label
}
```

## Expressions
Simple calculations may be performed.

This program:
```asm6502
lda #5 + 10
```

Is equal to:
```asm6502
lda #15
```

### Operators
Supported operators are:
- `*` Multiplication
- `/` Division
- `<<` Shift left
- `>>` Shift right
- `^` Exclusive or (XOR)
- `+` Addition
- `-` Subtraction
 
### Equality tests 
Equality tests may also be performed. They will evaluate to `0` when false and `1` when true:
- `==` Equality
- `!=` Inequality
- `>` Greater than
- `>=` Greater than or equal
- `<` Less than
- `<=` Less than or equal
- `&&` And
- `||` Or

### Nibble operators
Additionally, the high or low byte of 16-bit variables may be accessed using the `<` and `>` modifiers, e.g.:

```asm6502
.const ADDRESS = $1234
lda #<ADDRESS   // a will now contain '$34'
ldx #>ADDRESS   // x will now contain '$12'
```

### Modifiers
There are two ways to modify a factor in an expression.

- Prefix with `!` to convert `0` into `1` and any positive number into `0`
- Prefix with `-` to negate the value

### Built-in functions
Currently the only built-in function is `defined` which evaluates to `1` if a variable or constant is defined and to `0` otherwise.

```asm6502
.const ADDRESS = $1234
lda defined(ADDRESS)   // a will now contain '1'
lda !defined(ADDRESS)  // a will now contain '0'
```

## Variables and constants
You can define variables and constants using the `.var` and `.const` directives respectively. They can then be used in expressions.

For example:
```asm6502
.const BORDER_COLOUR = $d020

lda #7
sta BORDER_COLOUR
```

Variables may be redefined, constants may not.

## Data definition
You may include data inline like so:

```asm6502
.byte 1, 2, 3
```

Supported data types are `.byte`, `.word` and `.dword`.

### Including from files
It is also possible to include data from files, like so:

```asm6502
.include "foo.bin"
```

The file is located relative to the source file that contains the `.include` directive.

## Comments
Lines may end with a C++-style `//` comment, like so:

```asm6502
nop  // hello, I am a comment
```

C-style comment blocks are also supported:
```asm6502
/*
   hello there!
*/
nop
```

C-style comments may also be nested.

## Conditional assembly
It is possible to conditionally assemble chunks of code by wrapping them in an `.if` block:

```asm6502
.const FOO = 1

.if defined(FOO) {
    nop
} else {
    brk
}
```

The `else` clause is optional.

## Program counter
During assembly it is possible to change the current program counter (i.e. the location where instructions are assembled to).

### Setting
You can set the program counter with the `*` directive, like so:

```asm6502
* = $0800
```

### Aligning
You can move the program counter forward to make it align on a certain number of bytes, using the `.align` directive:

```asm6502
.align 256
nop         // This will always be assembled to $xx00
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

### Available options
##### name
The name of a segment. It must be a valid identifier.

##### start
Instructions are assembled to the `start` location.

##### pc
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

##### write
This can be set to `false` to disable writing the segment to disk.
