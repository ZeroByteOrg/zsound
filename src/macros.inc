.ifndef __MACROS_INC__
.define __MACROS_INC__

.macro YM_BUSY_WAIT
:	bit	YM_data
	bmi :-
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

; Since this project is currently designed to build both r38 and r39 compatible
; targets, the individual sources need to export/import different symbols.
; These macros help keep the import/export lists in the various sources easy-to-read.

; EXPORT_TAGGED essentially exports symbol as symbol+rev: foo --> foo38
; IMPORT_TAGGED does the inverse: foo38 --> foo

.macro EXPORT_TAGGED symbol
	.ident(.sprintf("%s%d",symbol,X16_VERSION)) := .ident(.sprintf("%s",symbol))
	.export	.ident(.sprintf("%s%d",symbol,X16_VERSION))
.endmacro

.macro IMPORT_TAGGED symbol
	;import revision-suffixed symbol and alias it with a non-suffixed symbol
	.import	.ident(.sprintf("%s%d",symbol,X16_VERSION))
	.ident(.sprintf("%s",symbol))	:=	.ident(.sprintf("%s%d",symbol,X16_VERSION))
.endmacro

.macro IMPORTZP_TAGGED symbol
	;import revision-suffixed symbol and alias it with a non-suffixed symbol
	.importzp .ident(.sprintf("%s%d",symbol,X16_VERSION))
	.ident(.sprintf("%s",symbol))	:=	.ident(.sprintf("%s%d",symbol,X16_VERSION))
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
