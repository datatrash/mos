    ldx #$00
loop:
    lda data, x
    beq end
    sta $0400, x
    inx
    jmp loop
end:
    rts

data:
    .file "include.bin"
    .byte 0
