.define segment {
    name = default
    start = $2000
}

    * = $1000
    {
        lda data
        {
            sta $d020, x
        }

        rts
    }

.segment default
data:
    .byte 1 // hello
    .word 4