; x16.inc by SlithyMatt
.include "x16.inc"
.include "zsm.inc"
.include "macros.inc"

.export helloworld

; ---------------------------------------------------------------------------
; Hello World:	prints a message to the screen.
;
; This is just a placeholder left over from when I first started using
; multi-file soruces for this project. I should remove it, as there's
; definitely no intention to have "hello world" as a function in zsound. :)
;
; ---------------------------------------------------------------------------

.segment  "CODE"

.proc helloworld: near

		ldx	#0
nextchar:
		lda str_hello,x
		beq	done
		jsr CHROUT
		inx
		bra nextchar
done:	RTS                    ; Return to caller
.endproc

.segment "RODATA"

str_hello:
	.byte	"zsound ", 0
strlen	= (* - str_hello)
string_length:	.byte	strlen
