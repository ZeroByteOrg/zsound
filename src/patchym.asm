; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"

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
		YM_BUSY_WAIT
		stx YM_reg
		nop
		sta	YM_data
		iny
		txa
		clc
		adc #$18
		tax
		lda (r0),y
		YM_BUSY_WAIT
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
		YM_BUSY_WAIT
		stx YM_reg
		nop
		sta	YM_data
		bra next
done:	RTS                    ; Return to caller

.endproc
