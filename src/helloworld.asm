; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"
.include "macros.inc"

EXPORT_TAGGED "helloworld"

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
.if X16_VERSION = 38
	.byte	"built for x16emu r38", 0
.else
	.byte	"built for r39 / real hardware", 0
.endif
strlen	= (* - str_hello)
string_length:	.byte	strlen
