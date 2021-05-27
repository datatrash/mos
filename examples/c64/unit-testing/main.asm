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
                        .assert cpu.pc == $1234

                        brk
                    }

                    .test will_succeed {
                        brk
                    }