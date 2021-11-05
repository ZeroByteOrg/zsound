; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"


; create revision-suffixed identifiers for the symbols being exported
; e.g. symbol38 := symbol
.ident(.sprintf("%s%d","playzfx",X16_VERSION)) := playmusic

; export the revision-suffixed symbols
.export	.ident(.sprintf("%s%d","playzfx",X16_VERSION))

ZFX_EOF			=	$FF

.segment "ZEROPAGE"

ZFXdata:	.tag	SONGPTR
ZFXdelay:	.res	1


.segment "CODE"

; loop through the channels, and play any active ZFX sounds
.proc playzfx: near
	rts
.endproc
