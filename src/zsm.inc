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

.endif
