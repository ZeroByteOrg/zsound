.include "x16.inc"
.include "macros.inc"

.export init_pcm
.export start_digi
.export play_pcm
.export stop_pcm
.export set_pcm_volume

; bare minimum info needed in a data file: VERA_CTRL, RATE, length
; digi parameter table also needs to store the location of the data
.struct DIGITAB
	addr		.addr
	bank		.byte
	size		.word	; 24bit digi size
	sizehi	.byte	; ...
	cfg			.byte	; VERA_audio_ctrl value
	rate		.byte	; VERA_audio_rate
.endstruct
DIGITAB_LAST		= DIGITAB::rate

.struct PCMSTATE
	state			.byte
	digi			.tag	DIGITAB
	byterate	.word	; (computed whenever playback rate is set)
	halfrate	.word	; Speed to use when AFLOW is clear
.endstruct


; TODO: these ZP addresses aren't permanent storage. Convert to using
; other temporary space in ZP and not hogging bytes just for this player.
.segment "ZEROPAGE"
pcm_pages:	.res 1
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

	; define some easy-to-read equivalents to the asm struct syntax
	digitab   = digi + PCMSTATE::digi
	digi_cfg  = digi + PCMSTATE::digi + DIGITAB::cfg
	digi_rate = digi + PCMSTATE::digi + DIGITAB::rate
	digi_addr = digi + PCMSTATE::digi + DIGITAB::addr
	digi_bank = digi + PCMSTATE::digi + DIGITAB::bank

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
	sta digitab,y
	dey
	bpl loop

	; check whether PCM data pointer is zero (from ZCM file header)
	; if so, then use load point + 8.
	lda digi_addr
	ora digi_addr+1
	ora digi_bank
	bne set_vera_params
	lda #8
	clc
	adc zp_tmp
	sta digi_addr
	lda zp_tmp+1
	adc #0
	sta digi_addr+1
	lda RAM_BANK
	sta digi_bank

	; set VERA pcm playback parameters
set_vera_params:
	lda digi_cfg
	and #$30  ; clear any volume bits from the digitab. Initialize to max volume.
	sta digi_cfg
	lda VERA_audio_ctrl
	and #$0f	; get current volume level (should use a variable instead of VERA)
	ora #$80	; set the clear_FIFO bit when setting the PCM parameters.
	ora digi_cfg
	sta VERA_audio_ctrl
		; TODO: Make the playback engine work in a way that doesn't require
		; clearing the buffer, yet is able to change parameters at the correct
		; time when the previous sound finishes draining. Challenge accepted!

	; pre-load the FIFO
	ldx digi_rate
	jsr set_byte_rate
	; srz active_digi ; This is currently done in stop_pcm (left here as reminder)
	dec active_digi
	dec active_digi	; signal play_pcm to set VERA playback rate after the load

	lda #$FF
	BANK_SAVE := (*-1)
	sta RAM_BANK
	clc ; for now, always return success. (C flag set = fail)
	rts
.endproc

;---------------------------------------------------------------
; .A = VERA_audio_ctrl
; .X = VERA_audio_rate setting
; AFFECTS Y
.segment "CODE"
.proc set_byte_rate: near

	fullrate = digi + PCMSTATE::byterate
	halfrate = digi + PCMSTATE::halfrate
	tmp_lo   = zp_tmp
	tmp_hi   = zp_tmp+1

	dex
	bmi bad_rate
	; get rates from LUT
	ldy pcmrate_fast,x
	sty tmp_hi
	ldy pcmrate_slow,x
	sty tmp_lo

	stz halfrate+1
	stz fullrate+1

	; See if value needs to be div by 2 or 4 due to PCM formatting:
	; LUT = 16bit stereo rate. /2 if mono and /2 if 8bit.
	; Value needs to <<4 if unaltered, less if one or more /2s
	ldx #2
check_16bit:
	bit #$10 ; check the 16bit format flag in .A
	beq check_stereo
	inx
check_stereo:
	bit #$20 ; check stereo flag in .A
	beq loop
	inx

	; shift up to 4 MSB of fast/slow rates into high bytes
	; of fullrate/halfrate
loop:
	asl tmp_hi
	rol	fullrate+1
	asl tmp_lo
	rol halfrate+1
	dex
	bne loop

	lda tmp_hi
	sta fullrate
	lda tmp_lo
	sta halfrate

	clc
	rts
bad_rate:
	jmp stop_pcm
.endproc


;...........
; play_pcm :
;=====================================================
.segment "CODE"
.proc play_pcm

	bytesleft	= digi + PCMSTATE::digi + DIGITAB::size
	fullrate 	= digi + PCMSTATE::byterate
	halfrate	= digi + PCMSTATE::halfrate
	thisrate  = zp_tmp
	nextstate	= zp_tmp2

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
	jmp go
last_frame:
	ldx bytesleft
	ldy bytesleft+1
	stz bytesleft
	stz bytesleft+1
	stz active_digi
go:
	jsr load_fifo
	lda active_digi
	beq done
	cmp #($100-2) ; why doesn't ca65 let me use #-2 ??
	bne done
activate_playback:
	inc
	sta active_digi
	ldx digi + PCMSTATE::digi + DIGITAB::rate
	stx VERA_audio_rate
done:
	rts

.endproc
;================================================[ play_pcm ]=====^

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
;===============================================[ load_fifo ]=====^

;...........
; stop_pcm :
;=====================================================
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

;candidate formulae:
; fast:
;ceil (2^(m-1-c) * ( (floor(vera_rate)/128)*48828.125 ) / 60 ) {c = correction factor like 0.05}
; slow:
;floor(2^(m-1+c) * ( (floor(vera_rate)/128)*48828.125 ) / 60 ) {c = correction factor like 0.05}

.if(0)
pcmrate:
	.byte $03,$04,$06,$07,$09,$0b,$0c,$0e,$0f,$11,$13,$14,$16,$18,$19,$1b
	.byte $1c,$1e,$20,$21,$23,$24,$26,$28,$29,$2b,$2c,$2e,$30,$31,$33,$34
	.byte $36,$38,$39,$3b,$3c,$3e,$40,$41,$43,$44,$46,$48,$49,$4b,$4c,$4e
	.byte $50,$51,$53,$55,$56,$58,$59,$5b,$5d,$5e,$60,$61,$63,$65,$66,$68
	.byte $69,$6b,$6d,$6e,$70,$71,$73,$75,$76,$78,$79,$7b,$7d,$7e,$80,$81
	.byte $83,$85,$86,$88,$89,$8b,$8d,$8e,$90,$91,$93,$95,$96,$98,$99,$9b
	.byte $9d,$9e,$a0,$a2,$a3,$a5,$a6,$a8,$aa,$ab,$ad,$ae,$b0,$b2,$b3,$b5
	.byte $b6,$b8,$ba,$bb,$bd,$be,$c0,$c2,$c3,$c5,$c6,$c8,$ca,$cb,$cd,$cf
.endif

pcmrate_fast: ; <<4 for 16+stereo, <<3 for 16|stereo, <<2 for 8+mono
	.byte $03,$04,$06,$07,$09,$0B,$0C,$0E,$10,$11,$13,$15,$16,$17,$19,$1A
	.byte $1C,$1E,$1F,$21,$22,$24,$26,$27,$29,$2A,$2C,$2E,$2F,$31,$33,$34
	.byte $36,$37,$39,$3B,$3C,$3E,$3F,$41,$43,$44,$46,$47,$49,$4B,$4C,$4E
	.byte $50,$51,$53,$54,$56,$58,$59,$5B,$5C,$5E,$60,$61,$63,$65,$66,$68
	.byte $69,$6B,$6D,$6E,$70,$71,$73,$75,$76,$78,$79,$7B,$7D,$7E,$80,$82
	.byte $83,$85,$86,$88,$8A,$8B,$8D,$8E,$90,$92,$93,$95,$97,$98,$9A,$9B
	.byte $9D,$9F,$A0,$A2,$A3,$A5,$A7,$A8,$AA,$AC,$AD,$AF,$B0,$B2,$B4,$B5
	.byte $B7,$B8,$BA,$BC,$BD,$BF,$C0,$C2,$C4,$C5,$C7,$C9,$CA,$CC,$CD,$CF

pcmrate_slow:
	.byte $01,$02,$04,$05,$07,$09,$0A,$0C,$0E,$0F,$11,$12,$14,$16,$17,$19
	.byte $1A,$1C,$1E,$1F,$21,$22,$24,$26,$27,$29,$2A,$2C,$2E,$2F,$31,$32
	.byte $34,$36,$37,$39,$3A,$3C,$3D,$3F,$41,$42,$44,$45,$47,$49,$4A,$4C
	.byte $4D,$4F,$51,$52,$54,$55,$57,$58,$5A,$5C,$5D,$5F,$60,$62,$64,$65
	.byte $67,$68,$6A,$6C,$6D,$6F,$70,$72,$74,$75,$77,$78,$7A,$7B,$7D,$7F
	.byte $80,$82,$83,$85,$87,$88,$8A,$8B,$8D,$8F,$90,$92,$93,$95,$96,$98
	.byte $9A,$9B,$9D,$9E,$A0,$A2,$A3,$A5,$A6,$A8,$AA,$AB,$AD,$AE,$B0,$B1
	.byte $B3,$B5,$B6,$B8,$BA,$BC,$BE,$BF,$C1,$C2,$C4,$C6,$C7,$C9,$CA,$CC



canary:
	.byte	"pcm player included"	; looking for this in the PRG for
									; simpleplayer when I re-build it
									; with this module in the library
