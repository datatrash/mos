// Some tests taken from the c64unit examples
.test stack_pointer {
    lda #6
 	pha
 	lda #4
 	pha

 	pla
 	pla

 	tsx

 	// TODO: add assertions
 	rts
}