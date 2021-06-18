                    .import * from "../shared/c64.asm"

                    .macro cart_header() {
                        .define bank {
                            name = "cart_header"
                            create-segment = true
                            size = 64
                        }

                        .segment "cart_header" {
                            // signature
                            .text "C64 CARTRIDGE   "
                            // header size
                            .byte 0, 0, 0, $40
                            // version
                            .word 1
                            // hardware type 'MAGIC DESK'
                            .byte 0, 19
                            // EXROM line status
                            .byte 1
                            // GAME line status
                            .byte 0
                            // reserved
                            .byte 0, 0, 0, 0, 0, 0
                            // cartridge name and padding
                            .text "EXAMPLE CARTRIDGE FOR MOS"
                            .byte 0, 0, 0, 0, 0, 0, 0
                        }
                    }

                    .macro bank_header(bank_idx) {
                        .define bank {
                            name = "bank_header_{bank_idx}"
                            size = 16
                            create-segment = true
                        }

                        .segment "bank_header_{bank_idx}" {
                            // signature
                            .text "CHIP"
                            // packet length
                            .byte 0, 0, $20, $10
                            // chip type (0 = ROM, 1 = RAM / no ROM data, 2 = Flash ROM)
                            .byte 0, 0
                            // bank number
                            .byte 0, bank_idx
                            // starting load address
                            .byte $80, $00
                            // ROM image size
                            .byte $20, $00
                        }

                        .define bank {
                            name = "bank_{bank_idx}"
                            size = 8192
                            fill = 0
                        }

                        .define segment {
                            name = "bank_{bank_idx}"
                            bank = "bank_{bank_idx}"
                            start = $8000
                        }
                    }

                    cart_header()
                    bank_header(0)

                    .segment "bank_0" {
                        .word coldstart
                        .word warmstart
                        .byte $C3, $C2, $CD, $38, $30

             coldstart: sei
                        stx $d016
                        jsr $fda3
                        jsr $fd50
                        jsr $fd15
                        jsr $ff5b
                        cli

             warmstart: inc $d020
                        jmp warmstart
                    }

                    .segment "bank_0" {
                        .test "header_is_in_the_right_place" {
                            .assert ram($8004) == $C3

                        }
                    }