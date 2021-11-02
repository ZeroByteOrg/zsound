.include	"x16.inc"
.include	"zsm.inc"

.export		nextdata

.segment	"ZEROPAGE"

.import		data

.segment	"CODE"

.import		stopmusic

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
