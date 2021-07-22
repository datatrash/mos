                    /// Thanks to F#READY for help in preparing this example
                    /// After loading, hit 'start', then wait two seconds and hit 'select'. Colors should appear.
                    .import * from "atari800.asm"

                    xex_load_header()

                    ///////////////////////////////////////////////////
                    // First XEX segment
                    ///////////////////////////////////////////////////
                    xex_segment_header("first", segments.first.start, segments.first.end - 1)

                    .define bank {
                        name = "first"
                    }

                    .define segment {
                        name = "first"
                        bank = "first"
                        start = $0600
                    }

                    .segment "first" {
                 first: lda #0
                        sta 710

            wait_start: lda $d01f
                        cmp #6
                        bne wait_start
                        rts                           // continue loading
                    }

                    xex_segment_ini("first", first)

                    ///////////////////////////////////////////////////
                    // Second XEX segment
                    ///////////////////////////////////////////////////
                    xex_segment_header("second", segments.second.start, segments.second.end - 1)

                    .define bank {
                        name = "second"
                    }

                    .define segment {
                        name = "second"
                        bank = "second"
                        start = segments.first.end
                    }

                    .segment "second" {
                second: lda #34
                        sta 710

                        lda #0
                        sta 20
             wait_2sec: lda 20
                        cmp #100
                        bne wait_2sec

           wait_select: lda $d01f
                        cmp #5
                        bne wait_select
                        rts                           // continue loading
                    }

                    xex_segment_ini("second", second)

                    ///////////////////////////////////////////////////
                    // Main XEX segment
                    ///////////////////////////////////////////////////
                    xex_segment_header("main", segments.main.start, segments.main.end - 1)

                    .define bank {
                        name = "main"
                    }

                    .define segment {
                        name = "main"
                        bank = "main"
                        start = segments.second.end
                    }

                    .segment "main" {
                  main: lda $d40b
                        adc 20
                        asl
                        sta $d40a
                        sta $d018
                        jmp main
                    }

                    xex_segment_run("main", main)