                    /// Color of the cursor (taken into effect when clearing screen)
                    .const cursor_color = $0286

               vic: {
                        /// The low-byte of the raster position
                        .const raster_pos = $d012

                        /// Bits 7-6: Unused
                        ///
                        /// Bit 5: Reset-Bit
                        ///
                        /// Bit 4: Multi-Color Mode
                        ///
                        /// Bit 3: 38/40 column (1 = 40 cols)
                        ///
                        /// Bit 2-0: Smooth scroll
                        .const xscroll = $d016

                        /// Background color
                        .const background = $d020

                        /// Foreground color
                        .const foreground = $d021
                    }

            colors: {
                        .const black = 0
                        .const white = 1
                        .const red = 2
                        .const cyan = 3
                        .const purple = 4
                        .const green = 5
                        .const blue = 6
                        .const yellow = 7
                        .const orange = 8
                        .const brown = 9
                        .const light_red = 10
                        .const dark_gray = 11
                        .const gray = 12
                        .const light_green = 13
                        .const light_blue = 14
                        .const light_gray = 15
                    }

            kernal: {
                        /// Clears the screen in the current cursor color
                        .const clrscr = $e544
                    }

                    /// Constructs a `0 sys*` basic line
                    .macro basic_start(address) {
                        * = $0801

                        .byte $0c, $08, $00, $00, $9e

                        .if address >= 10000 {
                            .byte $30 + (address / 10000) % 10
                        }

                        .if address >= 1000 {
                            .byte $30 + (address / 1000) % 10
                        }

                        .if address >= 100 {
                            .byte $30 + (address / 100) % 10
                        }

                        .if address >= 10 {
                            .byte $30 + (address / 10) % 10
                        }

                        .byte $30 + address % 10
                        .byte 0, 0, 0
                    }