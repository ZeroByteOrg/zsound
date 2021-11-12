.include	"x16.inc"
.include	"zsm.inc"

; import data (zeropage music pointer) and stopmusic routine
; symbols were exported as e.g. data38 or data39. Import the appropriate
; one, and alias it without the revision suffix.
; e.g.: data := data38

; IMPORTS / EXPORTS:

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

;-----------------------------------------------------------------------
; nextdata
;
; Advances the ZP pointer "data" by one byte through the HIRAM window.
; It is necessary to call this routine instead of the typical
; (ZP),Y method, as the data pointer could be pointing at the end of
; the window at any given time, and if it advances past the end, it
; must be wrapped back to $A000 and swap in the next bank.
;

.segment "CODE"
.proc nextdata: near
			; advance the data pointer, with bank-wrap if necessary
			inc	data
			beq	nextpage
			rts				; pointer remained in the same page. Done.
nextpage:	lda data+1		; advance the "page" address
			inc
			cmp	#$c0		; Check for bank wrap.
			bcc nobankwrap
			; bank wrapped.
			lda #$a0		; return to page $a000
			inc RAM_BANK	; bank in the next RAM bank
			inc data + SONGPTR::bank
			
			; TODO: Make this a cmp w/ actual # of avail banks.
			;       (don't assume 2MB of HIRAM installed)
			beq	die			; out-of-memory error
nobankwrap:
			sta	data+1		; store the new "page" address
			rts				; done
die:
			; stop the music and return error (carry bit = 1)
			jsr stopmusic
			sec
			rts
.endproc
