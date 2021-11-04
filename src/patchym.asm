; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"

; create revision-suffixed identifiers for the symbols being exported
; e.g. init_player38 := init_player
.ident(.sprintf("%s%d","patchym",X16_VERSION)) := patchym

; export the revision-suffixed symbols
.export	.ident(.sprintf("%s%d","patchym",X16_VERSION))


;.if X16_VERSION = 38
;	.export		patchym38
;.else
;	.export		patchym39
;.endif
;
;patchym38	:=	patchym
;patchym39	:=	patchym

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
