// woof
.define segment {
    name = default
    start = $2000 + 4 - %00100
}

.const test /* test value */ = 1
.var test2 = 5

* = $1000
{
    lda data
    sta data
    stx data

    .if test {
        nop
    }

    .if test {
        sta $d020,x
    } else {
        nop
    }

    rts
}

.segment default {
    nop
}

.align 8

data: {
    /* here it is */
    .byte 1 // hello
    .word 4
    nop
}

nop