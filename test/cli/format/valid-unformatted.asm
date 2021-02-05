.define segment { name=default
start=$2000}

.const test=1

*=$1000
  {lDa data
            .if test { STa             $d020 ,  x }
        rtS
}

.segment default
            data:
 .byte          1// hello
 .word  4