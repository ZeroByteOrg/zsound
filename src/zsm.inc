.ifndef __ZSM_INC__
.define __ZSM_INC__

.struct SONGPTR
	addr		.word	1
	bank		.byte	1
.endstruct

.struct	CHANMASK
	fm			.byte	1
	psg			.word	1
.endstruct

.struct ZSM_HEADER
	loop		.tag	SONGPTR		; offset to loop point (relative to load address)
	pcm			.tag	SONGPTR		; offset to PCM table (relative to load address)
	chanmask	.tag	CHANMASK	; bit flags of channels used by the song
	tickrate	.word	1			; Hz rate of song playback speed
	reserved	.byte	5			; future use
.endstruct

.macro YM_BUSY_WAIT
:	bit	YM_data
	bmi :-
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

.endif
