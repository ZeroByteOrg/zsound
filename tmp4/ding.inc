YMP_ym_ding:
	.byte	$C6,$00,$32,$02,$33,$31,$51,$26
	.byte	$0C,$0F,$1F,$1F,$1F,$1F,$09,$09
	.byte	$09,$09,$00,$03,$03,$03,$F5,$F5
	.byte	$F5,$F4

dingsound1:
	.byte	$c6,$00,$31,$01,$32,$30,$51,$26
	.byte	$0c,$0f,$1f,$1f,$1f,$1f,$09,$09
	.byte	$09,$09,$00,$03,$03,$03,$f5,$f5
	.byte	$f5,$f4

;Patch Table (low together, high together)
ding_lo:
	.byte	<YMP_ym_ding
ding_hi:
	.byte	>YMP_ym_ding

;Patch table indexes:
	YM_DING_ID	= 0


; static const sfxframe ding[] = { 
;	{0x28, 0x76, 0}, {0x08, YM_KeyUp, 0}, {0x08, YM_KeyDn, 1},
;	{0x08, YM_KeyUp, 7}, {0x28, 0x7c, 0}, {0x08, YM_KeyDn, 1},
;	{0x08, YM_KeyUp, 0x00}, {0,0,0xff}
; };

ZFX_ding: ; transposed for R39
	;.byte $03, $28, $79, $08, $00, $08, $78, $81
	.byte          $28, $79, $00, $08, $00, $00, $08, $78, $01
	;.byte $01, $08, $00, $87
	.byte          $08, $00, $07
	;.byte $02, $28, $7e, $08, $78, $81
	.byte          $28, $7e, $00, $08, $78, $01
	;.byte $01, $08, $00, $80
	.byte          $08, $00, $ff

ZFX_psgtest:
	;.byte $04, $00, $bc, $01, $02, $03, $3f, $02, $ff, $82
	.byte       $00, $bc, $01, $02, $03, $3f, $0a, $ff
	;          $00, $bc, $00, $01, $02, $00, $03, $3f, $00, $02, $ff, $02 
	;.byte $01, $03, $37, $82	; 14 10 15
	.byte       $0b, $37
	;          $03, $37, $02
	;.byte $01, $03, $2f, $82	; 18 12 18
	.byte       $0b, $2f
	;.byte $01, $03, $27, $82	; 22 14 21
	.byte		$0b, $27
	;.byte $01, $03, $1f, $82	; 26 16 24
	.byte		$0b, $1f
	;.byte $01, $03, $17, $82    ; 30 18 27
	.byte		$0b, $17
	;.byte $01, $03, $0f, $82    ; 34 20 30
	.byte		$0b, $0f
	;.byte $01, $03, $07, $82    ; 38 22 33
	.byte		$0b, $07
	;.byte $01, $03, $00, $82    ; 42 24 36
	.byte		$0b, $00
	;.byte $01, $02, $00, $80    ; 46 26 39
	.byte		$02, $00, $ff

