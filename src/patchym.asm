; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"

.export		patchym
.segment  "CODE"

; A = voice #, X/Y = address of patch

.proc patchym: near
		stx	r0L
		sty r0H
		ldy #0
		clc
		adc	#$20
		tax
		lda	(r0),y
:		bit YM_data	; wait for YM non-busy
		bmi	:-
		stx YM_reg
		nop
		sta	YM_data
		iny
		txa
		clc
		adc #$18
		tax
		lda (r0),y
:		bit YM_data	; wait for YM non-busy
		bmi	:-
		stx YM_reg
		nop
		sta YM_data
next:	txa
		clc
		adc #$08
		bcs	done
		iny
		tax
		lda (r0),y
:		bit YM_data	; wait for YM non-busy
		bmi	:-
		stx YM_reg
		nop
		sta	YM_data
		bra next
done:	RTS                    ; Return to caller

.endproc

.segment "RODATA"

str_hello:	.byte	"hello world", 0
strlen	= (* - str_hello)
string_length:	.byte	strlen
