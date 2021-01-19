    lda data
    sta $d020, x
    rts

data:
    .byte 1 // hello
    .word 4