                    // Some tests taken from the c64unit examples
                    .test stack_pointer {
                        lda #6
                        pha
                        lda #4
                        pha

                        pla
                        pla

                        tsx

                        .assert cpu.sp == $fd

                        brk
                    }

                    .test will_fail {
                        .loop 8 {
                            .trace (index, cpu.pc, ram($2000))
                        }
                        .trace
                        .assert cpu.pc == $1234

                        // will never be reached
                        nop
                    }

                    .test will_succeed {
                        brk
                    }