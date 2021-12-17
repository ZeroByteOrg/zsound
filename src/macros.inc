.ifndef __MACROS_INC__
.define __MACROS_INC__

.macro YM_BUSY_WAIT
:	bit	YM_data
	bmi :-
.endmacro

.macro VERA_SELECT_PSG
	lda #$FE
	and VERA_ctrl	; clear bit0 of CTRL register to select data0
	sta VERA_ctrl
	lda #$01		; bank 1, 0 stride
	sta VERA_addr_bank
;	lda #$f9		; PSG are on page $F9 of VRAM
	lda #>VRAM_psg
	sta VERA_addr_high
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


.endif
