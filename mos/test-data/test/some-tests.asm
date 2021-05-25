    lda data
    sta $d020
    rts

data:
    .byte 1

.test "test a" {
    nop
}