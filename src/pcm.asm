; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"

.ident(.sprintf("%s%d","helloworld",X16_VERSION)) := helloworld
.export	.ident(.sprintf("%s%d","helloworld",X16_VERSION))

; ---------------------------------------------------------------------------
; Hello World:	prints "hello world"
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
