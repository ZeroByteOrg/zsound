.include "x16.inc"
.include "macros.inc"

EXPORT_TAGGED "init_pcm"
EXPORT_TAGGED "start_digi"
EXPORT_TAGGED "play_pcm"
EXPORT_TAGGED "stop_pcm"
EXPORT_TAGGED "set_pcm_volume"

; bare minimum info needed in a data file: VERA_CTRL, RATE, length
; digi parameter table also needs to store the location of the data
.struct DIGITAB
	addr		.addr
	bank		.byte
	size		.word	; 24bit digi size
	sizehi		.byte	; ...
	cfg			.byte	; VERA_audio_ctrl value
	rate		.byte	; VERA_audio_rate
.endstruct
DIGITAB_LAST		= DIGITAB::rate

.struct PCMSTATE
	state		.byte
	digi		.tag	DIGITAB
	byterate	.word	; (computed whenever playback rate is set)
	halfrate	.word	; Speed to use when AFLOW is clear
.endstruct


; TODO: these ZP addresses aren't permanent storage. Convert to using
; other temporary space in ZP and not hogging bytes just for this player.
.segment "ZEROPAGE"
pcm_pages:		.res 1
zp_tmp:			.res 2
zp_tmp2:		.res 2

.segment "BSS"
digi:			.tag PCMSTATE

active_digi		:= digi + PCMSTATE::state


;---------------------------------------------------------------
.segment "CODE"
.proc init_pcm: near
	jsr stop_pcm
	lda #$0f	; init PCM paramaters to 8bit mono, max volume
	sta VERA_audio_ctrl
	; TODO: at some point I plan to have hooks in ZSM player
	; for using PCM, and this routine should setup the hooks
	; appropriately, as I'm thinking "jump table" to be the
	; way to do that without directly referencing symbols in this
	; module, forcing it to be assembled in whether or not it's
	; desired.
	rts
.endproc

;---------------------------------------------------------------
; .A = RAM bank
; .X/.Y = pointer to a digi's parameter table
; (TODO: implement HiRAM support for integration with zsound)
;---------------------------------------------------------------
; notes: This is probably going to be a higher-level API call for
; supporting tables of digi clips. There should probably be lower-level
; calls such as "pcm_setpointer, pcm_setparams, pcm_start, pcm_stop, etc.
;

.segment "CODE"
.proc start_digi: near

	stx	zp_tmp
	sty zp_tmp+1
	tax
	jsr stop_pcm
	; bank in the RAM with the digi index table.
	lda RAM_BANK
	sta BANK_SAVE
	stx RAM_BANK

	; copy the digi table data into the PCM engine's state table.
	ldy #DIGITAB_LAST
loop:
	lda (zp_tmp),y
	sta digi + PCMSTATE::digi,y
	dey
	bpl loop
	
	lda digi + PCMSTATE::digi + DIGITAB::cfg
	and #$30
	sta digi + PCMSTATE::digi + DIGITAB::cfg
	lda VERA_audio_ctrl
	and #$0f	; get current volume level
	ora #$80	; set the clear_FIFO bit when setting the PCM parameters.
	ora digi + PCMSTATE::digi + DIGITAB::cfg
	sta VERA_audio_ctrl
		; TODO: Make the playback engine work in a way that doesn't require
		; clearing the buffer, yet is able to change parameters at the correct
		; time when the previous sound finishes draining. Challenge accepted!

	; pre-load the FIFO
	ldx digi + PCMSTATE::digi + DIGITAB::rate
	jsr set_byte_rate
	dec active_digi
;	stz frac_bytes
	jsr play_pcm	; prime the FIFO with at least 1 frame's worth of data.
	
	; enable VERA PCM playback
	ldx digi + PCMSTATE::digi + DIGITAB::rate
	stx VERA_audio_rate
exit:
	lda #$FF
	BANK_SAVE := (*-1)
	sta RAM_BANK
	rts
.endproc

;---------------------------------------------------------------
; .A = VERA_audio_ctrl
; .X = VERA_audio_rate setting
.segment "CODE"
.proc set_byte_rate: near

	fullrate = digi + PCMSTATE::byterate
	halfrate = digi + PCMSTATE::halfrate

	dex
	bmi bad_rate
	ldy pcmrate,x	; get rate from LUT
	stz fullrate+1
	
	; See if value needs to be div by 2 or 4 due to PCM formatting:
	; LUT = 16bit stereo rate. /2 if mono and /2 if 8bit.
	; Value needs to <<4 if unaltered, less if one or more /2s 
	ldx #($100-3)
check_16bit:
	bit #$10 ; check the 16bit format flag
	bne check_stereo
	inx
check_stereo:
	bit #$20 ; check stereo flag
	bne set_speeds
	inx

	; shift up to 4 MSB of pcmrate into hi byte of fullrate
set_speeds:
	tya		; .A = rate value from LUT (will become low-byte)
loop:
	asl
	rol	fullrate+1
	inx
	bmi loop	; do n-1 shifts in the loop.
	; Save as halfrate before final shift.
	sta halfrate
	ldx fullrate+1
	stx halfrate+1
	asl
	sta fullrate
	rol fullrate+1
	clc
	rts
bad_rate:
	jmp stop_pcm
.endproc


.if(0)
;---------------------------------------------------------------
.segment "CODE"
.proc play_pcm: near

	totalbytes		= digi + PCMSTATE::digi + DIGITAB::size
	bytesperframe 	= digi + PCMSTATE::byterate
	fracframe		= digi + PCMSTATE::byterate_f

	lda	active_digi		; quick check whether digi player is active
	
	beq noop
	bmi :+
	dec active_digi
noop:
	rts

:
	; totalbytes -= bytesperframe + pcm_pages
	sec
	lda totalbytes
	sbc pcm_pages
	sbc bytesperframe
	tax
	lda totalbytes+1
	sbc bytesperframe+1
	tay
	lda totalbytes+2
	sbc #0
	bmi	last_frame
	; update totalbytes with the new remaining amount after this load.
	sta totalbytes+2
	sty totalbytes+1
	stx totalbytes
	; load .XY with totalbytes + fractional frame
	clc
	lda pcm_pages
	adc bytesperframe
	tax
	lda bytesperframe+1
	adc #0
	tay
	jmp load_fifo
last_frame:
	ldx totalbytes
	ldy totalbytes+1
	stz totalbytes
	stz totalbytes+1
	stz frac_bytes
	stz active_digi
	jmp load_fifo
.endproc
;================================================[ play_pcm ]=====^
.endif

;............
; play_pcm2 :
;=====================================================
.segment "CODE"
.proc play_pcm

	bytesleft	= digi + PCMSTATE::digi + DIGITAB::size
	fullrate 	= digi + PCMSTATE::byterate
	halfrate	= digi + PCMSTATE::halfrate
	thisrate    = zp_tmp

	lda	active_digi		; quick check whether digi player is active
	
	beq noop
	bmi :+
	dec active_digi
noop:
	rts

:	clc
	lda #$08
	and VERA_isr	; check AFLOW
	bne aflow_set
	; choose halfrate
	bra min
	lda halfrate+1
	sta thisrate+1
	lda halfrate
	sta thisrate
aflow_set:
	; choose fullrate
	lda fullrate+1
	sta thisrate+1
	lda fullrate
	sta thisrate
min:
	lda bytesleft
	sec
	sbc thisrate
	tax
	lda bytesleft+1
	sbc thisrate+1
	tay
	lda bytesleft+2
	sbc #0
	bmi	last_frame ; use bytesleft instead
	; update bytesleft with the new remaining amount after this load.
	sta bytesleft+2
	sty bytesleft+1
	stx bytesleft
	ldx thisrate
	ldy thisrate+1
	jmp load_fifo
last_frame:
	ldx bytesleft
	ldy bytesleft+1
	stz bytesleft
	stz bytesleft+1
	stz active_digi ; <-- wanna get this concept working....
	jmp load_fifo	
.endproc
;================================================[ play_pcm2 ]=====^

;............
; load_fifo :
;=====================================================
; blits PCM data into the VERA PCM FIFO as fast as possible.
; Arguments: .XY = number of bytes to copy.
; Affects  : updates the PCM_STATE active digi pointer to point
;          : to the next address after the final byte copied.
;          : RAM bank is preserved at the end of the routine.
;
; Since this routine assumes everything is set up properly, and
; does NO sanity or safety checking, it should not be exposed to
; the client program. 
.segment "CODE"
.proc load_fifo: near

	pcm_ptr		= digi + PCMSTATE::digi + DIGITAB::addr
	pcm_bank	= digi + PCMSTATE::digi + DIGITAB::bank
	bytes_left	= zp_tmp
	
	__CPX		= $e0	; opcode for cpx immediate
	__BNE		= $d0
	
	; swap in the current RAM bank of the sample stream
	lda RAM_BANK
	sta BANK_SAVE
	lda pcm_bank
	sta RAM_BANK
	; self-mod the page of the LDA below to the current page of pcm_ptr
	lda pcm_ptr+1
	sta data_page0
	sta data_page1
	sta data_page2
	sta data_page3
	; page-align the pcm_ptr
	txa			;.A now holds the low-byte of n-bytes to copy
	ldx pcm_ptr	;.X now points at the page-aligned offset
	; add the delta to bytes_left
	clc
	adc pcm_ptr
	sta bytes_left
	bcc :+
	iny
	; determine whether we have > $FF bytes to copy. If $100 or more, then
	; use the full-page dynamic comparator. Else use the last-page comparator.
:	cpy #0
	beq last_page	; if 0, then use the last_page comparator.
	; self-mod the instruction at dynamic_comparator to:
	; BNE copy_byte
	; note that if the distance between dynamic_comparator and copy_byte
	; is changed due to code changes, BE SURE TO FIX THIS VALUE (-9)
	lda #__BNE
	sta dynamic_comparator
	lda #.lobyte(-30)
	sta dynamic_comparator+1
	; compute num-steps % 4 (the mod4 is done by shifting the 2 LSB into Z and C)
	txa
enter_loop:
	ror
	ror
	bcc :+
	bmi copy_byte3	; 18
	bra copy_byte2	; 20
:	bmi copy_byte1	; 19

copy_byte0:
	lda $FF00,x
	data_page0 = (*-1)
	sta VERA_audio_data
	inx
copy_byte1:
	lda $FF00,x
	data_page1 = (*-1)
	sta VERA_audio_data
	inx
copy_byte2:
	lda $FF00,x
	data_page2 = (*-1)
	sta VERA_audio_data
	inx
copy_byte3:
	lda $FF00,x
	data_page3 = (*-1)
	sta VERA_audio_data
	inx
dynamic_comparator:
	bne copy_byte0
	; the above instruction is modified to CPX #(bytes_left) on the last page of data
	bne copy_byte0	; branch for final page's CPX result.
	cpx #0
	bne done		; .X can only drop out of the loop on non-zero during the final page.
					; Thus X!=0 means we just finished the final page. Done.
	; advance data pointer before checking if done on a page offset of zero.
	lda data_page0
	inc
	cmp #$c0
	beq do_bankwrap
no_bankwrap:
	; update the self-mod for all 4 iterations of the unrolled loop
	sta data_page0
	sta data_page1
	sta data_page2
	sta data_page3
check_done:
	cpy #0		; .Y = high byte of "bytes_left"
	beq done	; .X must be zero as well if we're here. Thus 0 bytes left. Done.
	dey
	bne copy_byte0	; more than one page remains. Continue with full-page mode copy.
last_page:
	lda bytes_left
	beq done		; if bytes_left=0 then we're done at offset 0x00, so exit.
	; self-mod the instruction at dynamic_comparator to be:
	; CPX #(bytes_left)
	sta dynamic_comparator+1
	lda #__CPX
	sta dynamic_comparator
	; compute the correct loop entry point with the new exit index
	; Find: bytes_left - .X
	txa
	eor #$ff
	sec	; to carry in the +1 for converting 2s complement of .X
	adc bytes_left
	; .A *= -1 to align it with the loop entry jump table
	eor #$ff
	inc
	bra enter_loop

done:
	;update the data pointer from .X and data_page
	;pcm_bank was updated on the fly in do_bankwrap
	stx pcm_ptr
	lda data_page0
	sta pcm_ptr+1
	; restore RAM_BANK to what it was when load_fifo was called
	lda #$FF
	BANK_SAVE = (*-1)
	sta RAM_BANK
	rts

do_bankwrap:
	lda #$a0
	inc RAM_BANK
	inc pcm_bank
	bra no_bankwrap
	
.endproc

;---------------------------------------------------------------
.segment "CODE"
.proc stop_pcm: near
	; stop playback
	stz VERA_audio_rate
	; flush fifo
	lda #$80
	ora VERA_audio_ctrl
	stz active_digi
	rts
.endproc

;.................
; set_pcm_volume :
;=====================================================
.segment "CODE"
.proc set_pcm_volume: near
	and #$0F	; mask off the non-volume-control bits
	sta zp_tmp
	lda VERA_audio_ctrl
	and #$30	; keep only the format control bits
	ora zp_tmp
	sta VERA_audio_ctrl
	rts
.endproc
;---------------------------------------------------------------

; LUT for bytes-per-frame at all possible play rates 1..128
; (loader does dex once before using as index, since 0 = not playing)
;
; Consider moving this into Bank RAM.... but zsound doesn't have
; the "workbank" implemented at this time, so here it stays for now. :)
;
; hmmm, that would have to be generated at run-time....

.segment "RODATA"

pcmrate:
	.byte $03,$04,$06,$07,$09,$0b,$0c,$0e,$0f,$11,$13,$14,$16,$18,$19,$1b
	.byte $1c,$1e,$20,$21,$23,$24,$26,$28,$29,$2b,$2c,$2e,$30,$31,$33,$34
	.byte $36,$38,$39,$3b,$3c,$3e,$40,$41,$43,$44,$46,$48,$49,$4b,$4c,$4e
	.byte $50,$51,$53,$55,$56,$58,$59,$5b,$5d,$5e,$60,$61,$63,$65,$66,$68
	.byte $69,$6b,$6d,$6e,$70,$71,$73,$75,$76,$78,$79,$7b,$7d,$7e,$80,$81
	.byte $83,$85,$86,$88,$89,$8b,$8d,$8e,$90,$91,$93,$95,$96,$98,$99,$9b
	.byte $9d,$9e,$a0,$a2,$a3,$a5,$a6,$a8,$aa,$ab,$ad,$ae,$b0,$b2,$b3,$b5
	.byte $b6,$b8,$ba,$bb,$bd,$be,$c0,$c2,$c3,$c5,$c6,$c8,$ca,$cb,$cd,$cf

.if(0)
pcmrate: ; packed as rate>>4 for 16bit stereo rates. adjust accordingly.
	.byte $03,$04,$06,$08,$09,$0b,$0d,$0e,$10,$12,$13,$15,$17,$18,$1a,$1c
	.byte $1d,$1f,$21,$22,$24,$26,$27,$29,$2b,$2c,$2e,$30,$31,$33,$35,$36
	.byte $38,$3a,$3b,$3d,$3f,$40,$42,$44,$45,$47,$49,$4a,$4c,$4e,$50,$51
	.byte $53,$55,$56,$58,$5a,$5b,$5d,$5f,$60,$62,$64,$65,$67,$69,$6a,$6c
	.byte $6e,$6f,$71,$73,$74,$76,$78,$7a,$7b,$7d,$7f,$80,$82,$84,$85,$87
	.byte $89,$8a,$8c,$8e,$8f,$91,$93,$94,$96,$98,$99,$9b,$9d,$9e,$a0,$a2
	.byte $a3,$a5,$a7,$a8,$aa,$ac,$ad,$af,$b1,$b2,$b4,$b6,$b7,$b9,$bb,$bc
	.byte $be,$c0,$c1,$c3,$c5,$c6,$c8,$ca,$cb,$cd,$cf,$d0,$d2,$d4,$d5,$d7
.endif

.if(0)

pcmrate_fr: ; fraction per frame
	.byte $5C,$B7,$13,$6E,$CA,$26,$81,$DD,$38,$94,$F0,$4B,$A7,$02,$5E
	.byte $BA,$15,$71,$CC,$28,$84,$DF,$3B,$97,$F2,$4E,$A9,$05,$61,$BC
	.byte $18,$73,$CF,$2B,$86,$E2,$3D,$99,$F5,$50,$AC,$07,$63,$BF,$1A
	.byte $76,$D1,$2D,$89,$E4,$40,$9B,$F7,$53,$AE,$0A,$65,$C1,$1D,$78
	.byte $D4,$2F,$8B,$E7,$42,$9E,$F9,$55,$B1,$0C,$68,$C4,$1F,$7B,$D6
	.byte $32,$8E,$E9,$45,$A0,$FC,$58,$B3,$0F,$6A,$C6,$22,$7D,$D9,$34
	.byte $90,$EC,$47,$A3,$FE,$5A,$B6,$11,$6D,$C8,$24,$80,$DB,$37,$92
	.byte $EE,$4A,$A5,$01,$5C,$B8,$14,$6F,$CB,$26,$82,$DE,$39,$95,$F1
	.byte $4C,$A8,$03,$5F,$BB,$16,$72,$CD

pcmrate_lo:
	.byte $06,$0C,$13,$19,$1F,$26,$2C,$32,$39,$3F,$45,$4C,$52,$59,$5F
	.byte $65,$6C,$72,$78,$7F,$85,$8B,$92,$98,$9E,$A5,$AB,$B2,$B8,$BE
	.byte $C5,$CB,$D1,$D8,$DE,$E4,$EB,$F1,$F7,$FE,$04,$0B,$11,$17,$1E
	.byte $24,$2A,$31,$37,$3D,$44,$4A,$50,$57,$5D,$64,$6A,$70,$77,$7D
	.byte $83,$8A,$90,$96,$9D,$A3,$A9,$B0,$B6,$BD,$C3,$C9,$D0,$D6,$DC
	.byte $E3,$E9,$EF,$F6,$FC,$02,$09,$0F,$16,$1C,$22,$29,$2F,$35,$3C
	.byte $42,$48,$4F,$55,$5B,$62,$68,$6F,$75,$7B,$82,$88,$8E,$95,$9B
	.byte $A1,$A8,$AE,$B5,$BB,$C1,$C8,$CE,$D4,$DB,$E1,$E7,$EE,$F4,$FA
	.byte $01,$07,$0E,$14,$1A,$21,$27,$2D

pcmrate_hi:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $03,$03,$03,$03,$03,$03,$03,$03
.endif

canary:
	.byte	"pcm player included"	; looking for this in the PRG for
									; simpleplayer when I re-build it
									; with this module in the library
