; ZFX sound sequence (PSG voice format):
ZFX_psgtest:
	.byte       $00, $bc, $01, $02, $03, $3f, $0a, $ff
	.byte       $0b, $37
	.byte       $0b, $2f
	.byte		$0b, $27
	.byte		$0b, $1f
	.byte		$0b, $17
	.byte		$0b, $0f
	.byte		$0b, $07
	.byte		$0b, $00
	.byte		$02, $00, $ff

; Ding from Flappy Bird game (written in C)
; This data sequence assumes the FM voice has already been patched
; prior to playing this sound. If no patch is loaded, it will not
; create any sound. A ZFX sequence could just as well include the
; patch values embedded within itself.
;
; Consider the fact that patch loads are slow on the YM due to its
; requirement that you wait ~150 CPU cycles between writes.
;
; It you have a certain tone that is reused over and over on the same
; voice, it is more efficient to just patch that voice once and use
; an FX sequence that does not have any patch data in it. This is an
; example of that concept.
;
; Another advantage of storing patches separately is that the same
; sound can be played with different instruments by simply loading
; a different patch before playing it.

; patch data (YMP format)
YMP_ding:
	.byte	$c6,$00,$31,$01,$32,$30,$51,$26
	.byte	$0c,$0f,$1f,$1f,$1f,$1f,$09,$09
	.byte	$09,$09,$00,$03,$03,$03,$f5,$f5
	.byte	$f5,$f4

; ZFX sound sequence (FM voice format):
.if REV=38
ZFX_ding: ; (pitch transposed for R38)
	;       reg  val  delay
	.byte	$28, $77, $00	; set KC (pitch) = $77 (F#-7)
	.byte	$08, $00, $00   ; KeyUP (release any previous existing note)
	.byte	$08, $78, $01	; KeyDN (play the note)
	.byte	$08, $00, $07	; KeyUP (release note)
	.byte	$28, $7c, $00	; KC = $7c (A#-7)
	.byte	$08, $78, $01	; KeyDN
	.byte	$08, $00, $ff	; KeyUP (delay $FF = EOF)
.else
ZFX_ding: ; (pitch transposed for R39)
	;       reg  val  delay
	.byte	$28, $79, $00	; KC = $79 (G#-7)
	.byte	$08, $00, $00   ; KeyUP
	.byte	$08, $78, $01	; KeyDN
	.byte	$08, $00, $07	; KeyUP
	.byte	$28, $7e, $00	; KC = $7e (C-7)
	.byte	$08, $78, $01	; KeyDN
	.byte	$08, $00, $ff	; KeyUP
.endif

; For reference, the YM registers used are:
; $28: KC value for a channel
; #08: Key trigger register (all 8 voices use this one register)

; Note that while all data is written for voice 0, it can be used on any
; channel, as the ZFX engine modifies the register numbers according to
; whichever voice is in use during playback.
; i.e. $28 is changed to $2A if playing on voice 2, etc.
