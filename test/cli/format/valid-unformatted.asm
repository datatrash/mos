        // woof
.define segment { name=default
start=$2000 + 4 -   4}

    .include   "foo.bin"
.const test=1

*=$1000
  {lda data
            .if test { sta             $d020 ,  x }
        rts
}

.segment default     {nop }
  .align   8
  // here is some data
            data:           /* here it is */
 .byte          1// hello
 .word  4