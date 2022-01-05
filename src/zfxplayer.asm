; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zfx.inc"
.include "macros.inc"

; UNDER CONSTRUCTION. Obviously, no SFX player here yet.....

EXPORT_TAGGED "update"
EXPORT_TAGGED "init"
EXPORT_TAGGED "play"

.segment "ZEROPAGE"
zp_tmp1:		.res 2
zp_tmp2:		.res 2
zp_tmp3:		.res 1

.segment "BSS"
zfx_state:	.tag PLAYERSTATE	; 171 bytes

ZFX_VECTOR_TABLE = *
ZFX_VECTOR_done:		.res 2	; callback to handle ZSM data EOF
ZSM_VECTOR_notify:		.res 2	; callback when an FX ends
ZFX_VECTOR_COUNT	= (*-ZFX_VECTOR_TABLE)

; idea - make SFX work like MOD/trackers with patterns and a sequence
; table. 



; ......
; init :
; ===========================================================================
; Arguments: none
; Returns: none
; Affects: A,X
; ---------------------------------------------------------------------------
; call before using any of the routines in this module.
; initializes the BSS data structures

.segment "CODE"
.proc init: near
	ldx #.sizeof(PLAYERSTATE)
nextbyte:
	dex
	stz zfx_state,x
	bne nextbyte
	rts
.endproc

; ......
; play :
; ===========================================================================
; Arguments:
;	X:  voice # (0-7 = FM, 8-23 = PSG)
;	AY: address of ZFX data (A = low byte, Y = hi byte)
; Returns: CC=success CS=error
; Affects: 
; ---------------------------------------------------------------------------
; Triggers playback of a ZFX sound.

.segment "CODE"
.proc play: near

	; make some easily-readable aliases into the state data table
	active   = zfx_state + PLAYERSTATE::active
	psg_active   = zfx_state + PLAYERSTATE::psg + PSGSTATE::active
	psg_delay    = zfx_state + PLAYERSTATE::psg + PSGSTATE::delay
	psg_ptr_lo   = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::addr_lo
	psg_ptr_hi   = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::addr_hi
	psg_ptr_bank = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::bank
	psg_loop_lo   = zfx_state + PLAYERSTATE::psg + PSGSTATE::loop + PSGPTR::addr_lo
	psg_loop_hi   = zfx_state + PLAYERSTATE::psg + PSGSTATE::loop + PSGPTR::addr_hi
	psg_loop_bank = zfx_state + PLAYERSTATE::psg + PSGSTATE::loop + PSGPTR::bank
	fm_active    = zfx_state + PLAYERSTATE::fm  + FMSTATE::active
	fm_delay     = zfx_state + PLAYERSTATE::fm  + FMSTATE::delay
	fm_ptr_lo    = zfx_state + PLAYERSTATE::fm  + FMSTATE::data  + FMPTR::addr_lo
	fm_ptr_hi    = zfx_state + PLAYERSTATE::fm  + FMSTATE::data  + FMPTR::addr_hi
	fm_ptr_bank  = zfx_state + PLAYERSTATE::fm  + FMSTATE::data  + FMPTR::bank
	fm_loop_lo   = zfx_state + PLAYERSTATE::fm + FMSTATE::loop + FMPTR::addr_lo
	fm_loop_hi   = zfx_state + PLAYERSTATE::fm + FMSTATE::loop + FMPTR::addr_hi
	fm_loop_bank = zfx_state + PLAYERSTATE::fm + FMSTATE::loop + FMPTR::bank

	cpx #(FM_CHANNELS + PSG_CHANNELS)
	bcs done
	cpx #FM_CHANNELS
	bcs play_psg
play_fm:
	stz fm_delay,x	; temporarily disable playback on the channel in case
					; program is driving ZFX during IRQ
	sta fm_ptr_lo,x
	sta fm_loop_lo,x
	tya
	sta fm_ptr_hi,x
	sta fm_loop_hi,x
	stz fm_ptr_bank,x
	stz fm_loop_bank,x
	inc fm_delay,x
	inc fm_active
	inc active
done:
	rts
play_psg:
	stz psg_delay-8,x	; temporarily disable playback on the channel in case
						; program is driving ZFX during IRQ
	sta psg_ptr_lo-8,x
	sta psg_loop_lo-8,x
	tya
	sta psg_ptr_hi-8,x
	sta psg_loop_hi-8,x
	stz psg_ptr_bank-8,x
	stz psg_loop_bank-8,x
	inc psg_delay-8,x
	inc psg_active
	inc active
	rts
.endproc


; .........
; update :
; ===========================================================================
; Arguments: none
; Returns: none
; Affects: A,X,Y VERA: Data0 port and VERA_ctl (to select port 0)
; ---------------------------------------------------------------------------
; loop through the channels, and play any active ZFX sounds
; Call once per frame to process sound effects


.segment "CODE"
.proc update: near

	; make some easily-readable aliases into the state data table
	active   = zfx_state + PLAYERSTATE::active
	psg_active   = zfx_state + PLAYERSTATE::psg + PSGSTATE::active
	psg_delay    = zfx_state + PLAYERSTATE::psg + PSGSTATE::delay
	psg_ptr_lo   = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::addr_lo
	psg_ptr_hi   = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::addr_hi
	psg_ptr_bank = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::bank
	fm_active    = zfx_state + PLAYERSTATE::fm  + FMSTATE::active
	fm_delay     = zfx_state + PLAYERSTATE::fm  + FMSTATE::delay
	fm_ptr_lo    = zfx_state + PLAYERSTATE::fm  + FMSTATE::data  + FMPTR::addr_lo
	fm_ptr_hi    = zfx_state + PLAYERSTATE::fm  + FMSTATE::data  + FMPTR::addr_hi
	fm_ptr_bank  = zfx_state + PLAYERSTATE::fm  + FMSTATE::data  + FMPTR::bank

	data = zp_tmp1
	counter = zp_tmp2+1	; skipping zp_tmp2+0 in case of bank support using zp_tmp1+2
	voice	= zp_tmp3
	
	lda active
	bne update_psg
	rts
update_psg:
	lda psg_active
	beq update_fm
	stz psg_active	; each FX that remains active will inc this value.
	VERA_SELECT_PSG
	ldx #PSG_CHANNELS
@loop:
	dex
	bmi update_fm
	lda psg_delay,x
	beq @loop
	inc psg_active
	dec
	sta psg_delay,x
	bne @loop
	jsr process_fx_psg
	bcs @loop
	; todo: callback
	dec psg_active
	bra @loop
update_fm:
	lda fm_active
	beq done
	stz fm_active
	ldx #FM_CHANNELS
@loop:
	dex
	bmi done
	lda fm_delay,x
	beq @loop
	inc fm_active
	dec
	sta fm_delay,x
	bne @loop
	jsr process_fx_fm ; returns CC if still active, CS if done
	bcc @loop
	; todo callback
	dec fm_active
	bra @loop
done:
	lda fm_active
	ora psg_active
	sta active
	rts
.endproc

; expects X=voice
.segment "CODE"
.proc process_fx_psg: near
	data = zp_tmp1
	reg  = zp_tmp2+1

	psg_delay    = zfx_state + PLAYERSTATE::psg + PSGSTATE::delay
	psg_ptr_lo   = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::addr_lo
	psg_ptr_hi   = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::addr_hi
	psg_ptr_bank = zfx_state + PLAYERSTATE::psg + PSGSTATE::data + PSGPTR::bank

	txa	; convert basic voice # in X into base VERA register address ($c0 + 4*x)
	asl
	asl
	ora #$c0 ; since 4x is < 64, just ORA instead of CLC ; ADC
	sta reg
	; create page-aligned data pointer in ZP
	stz data
	lda psg_ptr_hi,x
	sta data+1
	lda psg_ptr_lo,x
	tay
loop:
	; get delay+reg offset byte. (6msb = delay, 2lsb = reg offset)
	lda (data),y
	cmp #$fc	; delay of all-ones = EOF flag. (ignore 2 lsb)
	bcs exit
	sta zp_tmp2 ; store delay amount in zp_tmp2
	lsr zp_tmp2 ; >>2 to remove the 2 PSG register offset bits.
	lsr zp_tmp2
	and #$03	; mask off the delay bits from A
	ora reg		; add the register base to get actual VERA address
	sta VERA_addr_low
	iny
	bne :+
	inc data+1
:	lda (data),y
	iny
	bne :+
	inc data+1
:	sta VERA_data0
	lda zp_tmp2	; retreive the delay amount (might be zero)
	beq loop	; if zero, then continue with next reg/val pair
	sta psg_delay,x
	; update the data pointer in main data structure
	lda data+1
	sta psg_ptr_hi,x
	tya
	sta psg_ptr_lo,x
	sec	; signal "active" to main loop
	rts
exit:
	clc
	rts
.endproc

; expects X=voice
.segment "CODE"
.proc process_fx_fm: near

	data = zp_tmp1
	voice = zp_tmp2+1
	fm_delay    = zfx_state + PLAYERSTATE::fm + FMSTATE::delay
	fm_ptr_lo   = zfx_state + PLAYERSTATE::fm + FMSTATE::data + FMPTR::addr_lo
	fm_ptr_hi   = zfx_state + PLAYERSTATE::fm + FMSTATE::data + FMPTR::addr_hi
	fm_ptr_bank = zfx_state + PLAYERSTATE::fm + FMSTATE::data + FMPTR::bank
	
	stx voice
	; create page-aligned data pointer in ZP
	stz data
	lda fm_ptr_hi,x
	sta data+1
	lda fm_ptr_lo,x
	tay
loop:
	lda (data),y	; get YM reg
	cmp #$20		; if < 0x20 then check for KeyUP/DN
	bcc get_value
	and #$f8
	ora voice
get_value:
	tax
	iny
	bne :+
	inc data+1
:	lda (data),y
	cpx #$08
	bne write_ym
	and #$f8
	ora voice
write_ym:
	bit YM_data
	bmi write_ym
	stx YM_reg
	nop
	sta YM_data
	iny
	bne :+
	inc data+1
:	lda (data),y	; get post-write delay
	bne done
	iny
	bne loop
	inc data+1
	bra loop
done:
	ldx voice
	cmp #$ff
	beq :+
	sta fm_delay,x
	iny
	bne :+
	inc data+1
:	lda data+1
	sta fm_ptr_hi,x
	tya
	sta fm_ptr_lo,x
	rts
.endproc
