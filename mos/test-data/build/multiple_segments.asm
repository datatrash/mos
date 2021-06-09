// Segment 'a' is assembled as if it is run from $4000, but it is stored in the target from $1000
.define segment {
    name = a
    start = $1000
    pc = $4000
}

// Segment 'b' starts right after segment a
.define segment {
    name = b
    start = segments.a.end
}

// Segment 'c' starts right after segment b
.define segment {
    name = c
    start = segments.b.end
}

    lda data
    sta $d020
    rts

    .segment b { nop }
    .segment c { asl }

data:
    .byte 1
