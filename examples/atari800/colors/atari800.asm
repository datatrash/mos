                    .macro xex_load_header() {
                        .define bank {
                            name = "xex-load-header"
                            create-segment = true
                        }

                        .segment "xex-load-header" {
                            .byte $ff, $ff
                        }
                    }

                    .macro xex_segment_header(name, start, end) {
                        // the end of a segment is inclusive, so the last emitted byte is actually at end - 1
                        .const end_offset = end - 1

                        .define bank {
                            name = "${name}-header"
                            create-segment = true
                        }

                        .segment "${name}-header" {
                            .byte <start, >start, <end_offset, >end_offset
                        }
                    }

                    .macro xex_segment_ini(name, addr) {
                        .define bank {
                            name = "${name}-ini"
                            create-segment = true
                        }

                        .segment "${name}-ini" {
                            .byte $e2, $02, $e3, $02, <addr, >addr
                        }
                    }

                    .macro xex_segment_run(name, addr) {
                        .define bank {
                            name = "${name}-run"
                            create-segment = true
                        }

                        .segment "${name}-run" {
                            .byte $e0, $02, $e1, $02, <addr, >addr
                        }
                    }