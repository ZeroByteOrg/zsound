.ifndef __MACROS_INC__
.define __MACROS_INC__

.macro YM_BUSY_WAIT
:	bit	YM_data
	bmi :-
.endmacro

.macro HIRAM_NEXT
	iny
	bne :+
	jsr nextpage
:
.endmacro

.macro HIRAM_SKIP n
.if (n = 1)
	HIRAM_NEXT
.else
	tya
	clc
	adc #n
	tay
	bcc :+
	jsr nextpage
:
.endif
.endmacro

.macro VERA_SELECT_PSG stride
	lda #$FE
	and VERA_ctrl	; clear bit0 of CTRL register to select data0
	sta VERA_ctrl
	;set stride = 0 by default, else use stride value provided
	.ifnblank stride
	  .if stride < 0
		 lda #((^VRAM_psg) | $08 | ((0-stride) << 4))
	  .else
		 lda #((^VRAM_psg) | (stride << 4))
	  .endif
	.else
	  lda #(^VRAM_psg)
	.endif
	sta VERA_addr_bank
	lda #>VRAM_psg
	sta VERA_addr_high
	; not setting low byte - only pointing at the PSG page of VRAM
.endmacro

.macro HIPTR_add_word _src, _dst
	; expects .XY as the word to add
	; TODO: assembler directives to check that _src and _dst don't overlap
	lda _src+2
	sta _dst+2
	txa
	clc
	adc _src
	sta _dst
	lda #0
	adc #0
	sta _dst+1	; temporarily store carry in _dst page
	tya
	; .Y %= $20 and _dst bank += .Y / $20
:	cmp #$20
	bcc :+
	sbc #$20	; carry guaranteed to be set by CMP if we reach this instruction
	inc _dst+2
	bra :-
	; add (.Y % $20) to _dst page including carry from above
:	adc _dst+1	; add carry from above
	adc _src+1
:	cmp #$c0	; final bank-wrap check
	bcc :+
	inc _dst+2
	sbc #$20
:	sta _dst+1
.endmacro


.endif
