        // woof
.define segment { name=default
start=$2000 + 4 -%00100}

    .file   "foo.bin"
.const   test   /* test value */   =1
.var   test2    = 5

// first comment
// second comment
*=$1000
  {lda data
  sta data
  stx data
           .if test { nop  }
            .if test { sta             $d020 ,  x }    else {  nop       }
        rts
}
.segment default     {nop /* nice*/}
  .align   8

  // here is some data
            data: {          /* here it is */
 .byte          1,2// hello
 .word  4
 nop}

 nop

 .macro MyMacro(arg1,arg2){brk}
