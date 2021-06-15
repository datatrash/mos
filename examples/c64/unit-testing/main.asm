                    // Based on a c64unit example
                    .test "stack_pointer" {
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

                    .test "will_fail" {
                        .loop 2 {
                            .trace (index, *, ram($2000))
                        }
                        .trace
                        .assert * == $1234

                        // will never be reached
                        nop
                    }

                    .test "will_succeed" {
                        brk
                    }