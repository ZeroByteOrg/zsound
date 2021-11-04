.include	"x16.inc"
.include	"zsm.inc"

; import data (zeropage music pointer) and stopmusic routine
; symbols were exported as e.g. data38 or data39. Import the appropriate
; one, and alias it with data := data38...

;.define	DATA	= .ident(.sprintf("%s%d","data",X16_VERSION)

; data is a zeropage symbol, so import it in that segment....
.segment "ZEROPAGE"
.import	.ident(.sprintf("%s%d","data",X16_VERSION))
data	:=	.ident(.sprintf("%s%d","data",X16_VERSION))

; everything else can be referenced as absolutes
.segment "CODE"

; import stopmusic routine
.import	.ident(.sprintf("%s%d","stopmusic",X16_VERSION))
stopmusic	:=	.ident(.sprintf("%s%d","stopmusic",X16_VERSION))

; export nextdata routine
.ident(.sprintf("%s%d","nextdata",X16_VERSION)) := nextdata
.export	.ident(.sprintf("%s%d","nextdata",X16_VERSION))

.segment "CODE"
.proc nextdata: near
			; advance the data pointer, with bank-wrap if necessary
			inc	data
			beq	:+
			rts
			; next page
:			lda data+1
			inc
			cmp	#$c0		; Check for bank wrap.
			bcc nobankwrap
			; bank wrapped.
			lda #$a0		; return to page $a000
			inc RAM_BANK	; bank in the next RAM bank
			inc data + SONGPTR::bank
			
			; TODO: Make this a cpx w/ actual # of avail banks.
			;       (don't assume 2MB of HIRAM installed)
			beq	die		; out-of-memory error
nobankwrap:
			sta	data+1
			rts
die:
			; stop the music and return error (carry bit = 1)
			jsr stopmusic
			sec
			rts
.endproc
