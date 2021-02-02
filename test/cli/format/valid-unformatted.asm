.define segment { name=default
start=$2000}

*=$1000
  {lDa data
            { STa             $d020 ,  x }
        rtS
}

.segment default
            data:
 .byte          1// hello
 .word  4