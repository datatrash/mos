        // woof
.define segment { name/*hello*/=default
start=$2000 + 4 -%00100}



    .file   "foo.bin"
.const   test   /* test value */   =1
.var   test2    = 5



// first comment
// second comment
*=$1000
  {lda data // interesting
  sta data
  stx data
           .if test { nop  }
            .if test { sta             $d020 ,  x
             asl}    else {  nop       }
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
 MyMacro(1,2)

 .import   foo    as    bar    from    "other.asm" {nop}
    .loop 123 {  nop}