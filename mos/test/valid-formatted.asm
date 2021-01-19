    lda data
    sta $d020
    rts

data:
    .byte 1 // hello
    .word 4