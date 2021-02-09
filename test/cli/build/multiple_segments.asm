.define segment { name = a start = $4000 target_address = $1000 }
.define segment { name = b start = $1020 }

    lda data
    sta $d020
    rts

    .segment b { nop }

data:
    .byte 1