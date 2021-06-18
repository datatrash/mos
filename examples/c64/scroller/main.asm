                    .import * from "../shared/c64.asm"

                    basic_start(start)

             start: lda #colors.gray
                    sta cursor_color
                    jsr kernal.clrscr
                    lda #0
                    sta vic.background
                    sta vic.foreground

                    // Set 38-column mode
                    lda vic.xscroll
                    and #%11110111
                    sta vic.xscroll

                    {
                        // Run all this code once per frame
                        lda vic.raster_pos
                        cmp #$80
                        bne -

                        // Do per-pixel soft-scroll
                        dec xscroll
                        bpl apply_xscroll

                        // the xscroll has wrapped around, so reset it
                        // and shift the screen by one char
                        lda #7
                        sta xscroll

                        ldx #0

                        {
                            lda $0401, x
                            sta $0400, x
                            inx
                            cpx #39
                            bne -
                        }

                        // Read a new character from our scroller.
                        // If it's $ff we should wrap around.
                        ldx text_pos
                        lda text, x
                        cmp #$ff
                        bne write_char

                        lda #$00
                        sta text_pos
                        jmp apply_xscroll

            write_char: sta $0427
                        inc text_pos

                        // Apply our soft-scroll value to VIC's xscroll register
         apply_xscroll: lda vic.xscroll
                        and #%11111000
                        ora xscroll
                        sta vic.xscroll

                        // Back to top
                        jmp -
                    }

           xscroll: .byte 0
          text_pos: .byte 0

              text: {
                        .text petscreen "mos says hello!      "
                        .byte $ff
                    }