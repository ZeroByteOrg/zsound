.include "x16.inc"
.segment "ZEROPAGE"

; ZP variables potentially used in zsound
pcm_ptr:		.res 2
pcmbank:		.res 1
pcmbytes:		.res 2
pcmblit:		.res 2
;active_digi:	.res 1	; moved to BSS for now

; for test program only
pcmcfg:			.res 1
pcmrate:		.res 1

.struct DIGITAB
	addr		.addr
	bank		.byte
	cfg			.byte
	perframe	.word
	size		.word
	sizehi		.byte
	rate		.byte
.endstruct
DIGITAB_LAST		= DIGITAB::rate

PCM_RATE_8000	= (8000/(25000000 >> 16)+1)
PCM_RATE_12207	= 32

.segment "RODATA"
pcmtab_lo:	.byte	<digi_coin, <digi_start
pcmtab_hi:	.byte	>digi_coin, >digi_start

.if 0
digi_coin:
	.addr	$a000
	.byte	1
	.byte	$0f
	.word	133
	.word	1739
	.byte	0		; high byte of 24-bit size
	.byte	PCM_RATE_8000
digi_start:
	.addr	$a000 + 1739
	.byte	1
	.byte	$0f
	.word	133
	.word	38830
	.byte	0
	.byte	PCM_RATE_8000
.endif

.if 0
	.include "raw1cfg.inc"
.endif

.if 1
	.include "raw2cfg.inc"
.endif

.segment "BSS"
active_digi:	.res 1
digi:			.tag DIGITAB

.ifdef VRAMLOG
VLBASE = $10000
.segment "DATA"
	VL_low:		.byte	<VLBASE
	VL_high:	.byte	>VLBASE
	VL_bank:	.byte	((^VLBASE) | $10)
.endif


;---------------------------------------------------------------
; test shell
;---------------------------------------------------------------

.segment "STARTUP"
	jmp	init
	jmp coin_sound
	jmp startup_tune
	rts
	
coin_sound:
	ldy #0
	lda pcmtab_lo,y
	tax
	lda pcmtab_hi,y
	tay
	lda #1
	jsr start_digi
	rts
	
startup_tune:
	ldy #1
	lda pcmtab_lo,y
	tax
	lda pcmtab_hi,y
	tay
	lda #1
	jsr start_digi
	rts
	
.ifdef VRAMLOG
wipe_vram:
	sei
	pha
	phx
	phy
	VERA_SET_ADDR $10000,1
	ldx #$cb
	ldy #0
:	stz VERA_data0
	iny
	bne :-
	dex
	bne :-
	ply
	plx
	pla
	cli
	rts
.endif

;---------------------------------------------------------------
; begin zsound PCM player module candidate code
;---------------------------------------------------------------
.segment "CODE"
irqhandler:
	jsr play_pcm
	jmp $ffff
	KERNAL_IRQ := (*-2)

;---------------------------------------------------------------
.segment "CODE"
.proc init: near
	jsr stop_pcm
	lda #<irqhandler
	cmp IRQVec
	bne install_irq
	lda #>irqhandler
	beq	done
install_irq:
	sei
	lda IRQVec
	sta KERNAL_IRQ
	lda IRQVec+1
	sta KERNAL_IRQ+1
	lda #<irqhandler
	sta IRQVec
	lda #>irqhandler
	sta IRQVec+1
	cli
done:
	rts
.endproc

;---------------------------------------------------------------
; .A = RAM bank
; .X/.Y = pointer to digi parameter table
; (TODO: implement HiRAM support for integration with zsound)
;---------------------------------------------------------------
.segment "CODE"
.proc start_digi: near

	NEWDIGI_PTR = pcmblit	; "rename" pmcblit for code readability

.ifdef VRAMLOG
	jsr wipe_vram
.endif
	
	stx	NEWDIGI_PTR
	sty NEWDIGI_PTR+1
	tax
	jsr stop_pcm	; do we really need a full STOP to go by? Probably not.
	lda RAM_BANK
	sta BANK_SAVE
	stx RAM_BANK
	; copy the digi table data into the active digi table memory.
	ldy #DIGITAB_LAST
loop:
	lda (NEWDIGI_PTR),y
	sta digi,y
	dey
	bpl loop
	
	; set up the pointers and indexes, etc for playback.
	lda digi + DIGITAB::addr
	sta pcm_ptr
	lda digi + DIGITAB::addr+1
	sta pcm_ptr+1
	lda digi + DIGITAB::bank
	sta pcmbank
	lda digi + DIGITAB::cfg
	ora #$80	; clear the FIFO when setting the PCM parameters.
	sta VERA_audio_ctrl
	; pre-load the FIFO
	dec active_digi
	jsr play_pcm	; preload 2 frames' worth of samples into the FIFO.
	;jsr play_pcm
	;jsr play_pcm
	; enable VERA PCM playback
	lda digi + DIGITAB::rate
	sta VERA_audio_rate
	lda #$FF
	BANK_SAVE := (*-1)
	sta RAM_BANK
	rts
.endproc

;---------------------------------------------------------------
.segment "CODE"
.proc play_pcm: near

	totalbytes = digi + DIGITAB::size
	bytesperframe = digi + DIGITAB::perframe
	
	lda	active_digi		; quick check whether digi player is active
	beq done
	lda totalbytes		; check whether any bytes remain in the digi
	ora totalbytes+1
	ora totalbytes+2
	bne :+
end_of_digi:
	stz active_digi		; ... if not, then deactivate digi player
:	sec
	; call load_fifo with the lesser of totalbytes or bytesperframe
	lda totalbytes
	sbc bytesperframe
	lda totalbytes+1
	sbc bytesperframe+1
	bcc send_totalbytes
send_bytesperframe:
	lda bytesperframe
	sta pcmbytes
	lda bytesperframe+1
	sta pcmbytes+1
	bra call_load_fifo
send_totalbytes: ; i.e. the last frame's worth of samples.
	lda totalbytes
	sta pcmbytes
	lda totalbytes+1
	sta pcmbytes+1
call_load_fifo:
	jsr	load_fifo
	php
	; subtract bytes transferred (pcmblit) from totalbytes
	sec
	lda totalbytes
	sbc pcmblit
	sta totalbytes
	lda totalbytes+1
	sbc pcmblit+1
	sta totalbytes+1
	lda totalbytes+2
	sbc #0
	sta totalbytes+2
	; note: wouldn't carry=clear here mean the digi is done?
	plp
	bcs	call_load_fifo
done:
	rts
.endproc


;---------------------------------------------------------------
; load_fifo: blits up to one page of PCM data into the FIFO.
;
; Affects: A, X, Y
;
; Arguments: none* (see description)
; Returns: carry = 0: done | 1: not done
;
; Uses ZP var pcmbytes as the count of bytes to be transferred.
; Performs one pass, stopping at either the page boundary or the
; end-of-data (pcmblit=0) in which case it returns 0 in the carry flag.
; 
; This behavior is intended to keep the transfers page-aligned as much
; as possible in order to reduce the extra CPU cycle overhead for
; LDA (ZP),Y crossing page boundaries. Furthermore, it is done in a
; single pass in order to allow programs to interleave the PCM writing
; with other tasks, such as waiting on YM to become not-busy.
;---------------------------------------------------------------
.segment "CODE"
.proc load_fifo: near
	; early exit if no remaining data to transfer
	jsr test_EOD	; sets carry flag 1=not done, 0=done.
	bcs setup
	rts
setup:
	lda RAM_BANK
	sta BANK_SAVE
.ifdef VRAMLOG
	lda VERA_ctrl
	pha
	stz VERA_ctrl
	lda VERA_addr_low
	pha
	lda VERA_addr_high
	pha
	lda VERA_addr_bank
	pha
	lda VL_low
	sta VERA_addr_low
	lda VL_high
	sta VERA_addr_high
	lda VL_bank
	sta VERA_addr_bank
.endif
	; determine which is less: pcmbytes or bytes left in current page
	sec
	lda #0
	sbc pcm_ptr
	sta pcmblit
	tay				; if pcmblit is smaller, then Y should be pcmblit
	lda #0
	adc #0
	sta pcmblit+1	; sets hi byte=1 if 256 bytes on page (full page blit)
	; pcmblit now = num bytes remaining on this page
	
	sec
	lda pcmbytes
	sbc pcmblit
	lda pcmbytes+1
	sbc pcmblit+1
	bpl go			; pcmblit <= pcmbytes, so that's how many to copy
	stz pcmblit+1
	ldy pcmbytes
	sty pcmblit
go:
	;copy number of bytes specified in pcmblit
	; Y = low byte of pcmblit, which is $100 max
	lda pcmbank
	sta RAM_BANK
	dey
	beq last_byte
loop:
	; TODO:	add check for full FIFO
	lda (pcm_ptr),y
.ifdef VRAMLOG
	sta VERA_data0
.endif
	sta VERA_audio_data
	dey
	bne loop
last_byte:
	lda (pcm_ptr),y
.ifdef VRAMLOG
	sta VERA_data0
.endif
	sta VERA_audio_data
	; update PCM data pointer
	clc
	lda pcmblit
	adc pcm_ptr
	sta pcm_ptr
	lda pcmblit+1
	adc pcm_ptr+1
	cmp #$c0		; bank wrap if we ended up at $c000
	bne nobankwrap
	inc pcmbank
	lda #$a0
nobankwrap:
	sta pcm_ptr+1
	; update the pcmbytes counter and exit
	sec
	lda pcmbytes
	sbc pcmblit
	sta pcmbytes
	lda pcmbytes+1
	sbc pcmblit+1
	sta pcmbytes+1
	lda #$FF
	BANK_SAVE := (*-1)
	sta RAM_BANK
.ifdef VRAMLOG
	lda VERA_addr_bank
	sta VL_bank
	lda VERA_addr_high
	sta VL_high
	lda VERA_addr_low
	sta VL_low
	pla
	sta VERA_addr_bank
	pla
	sta VERA_addr_high
	pla
	sta VERA_addr_low
	pla
	sta VERA_ctrl
.endif
	jmp test_EOD
.endproc

;---------------------------------------------------------------
.segment "CODE"
.proc test_EOD: near
	lda	pcmbytes
	bne notfinished
	ora pcmbytes+1
	bne notfinished
finished:
	clc
	rts
notfinished:
	sec
	rts
.endproc

;---------------------------------------------------------------
.segment "CODE"
.proc stop_pcm: near
	stz VERA_audio_rate
	lda #$80
	sta VERA_audio_ctrl
	stz active_digi
	stz pcmbytes
	stz pcmbytes+1
	rts
.endproc
