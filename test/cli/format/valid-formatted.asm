.define segment {
    name = default
    start = $2000
}

.const test = 1
    * = $1000
    {
        lda data
        .if test {
            sta $d020, x
        }
        rts
    }

.segment default
data:
    .byte 1 // hello
    .word 4