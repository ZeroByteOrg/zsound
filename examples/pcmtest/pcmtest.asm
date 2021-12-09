.include "x16.inc"
.segment "ZEROPAGE"

; ZP variables potentially used in zsound
pcm_ptr:		.res 2
pcm_bank:		.res 1
pcmbytes:		.res 2


end_ptr:		.res 2
end_bank:		.res 1

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

;---------------------------------------------------------------
; test shell
;---------------------------------------------------------------

;.segment "ONCE"
.segment "RODATA"
filename:	.byte "raw2"
filename_len = (*-filename)

.segment "STARTUP"

	DIGI_BANK = 1
	LOADTO = $a000
	
	; set BANKRAM to the first bank where song should load
	lda	#DIGI_BANK
	sta	RAM_BANK
	; prepare for call to SETNAM Kernal routine
	lda #filename_len
	ldx #<filename
	ldy #>filename
	jsr SETNAM
	; prepare for call to SETLFS Kernal routine
	lda #0	; logical file id 0
	ldx	#8	; device 8
	ldy #0	; 0 = no command
	jsr	SETLFS
	; load song to LOADTO
	lda	#0		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
	ldx	#<LOADTO
	ldy #>LOADTO
	jsr LOAD
	
	jsr	init
	jmp startup_tune
	
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
	cmp IRQVec+1
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

	stx	NEWDIGI_PTR
	sty NEWDIGI_PTR+1
	tax
	jsr stop_pcm
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
	sta pcm_bank
	lda digi + DIGITAB::cfg
	ora #$80	; clear the FIFO when setting the PCM parameters.
	sta VERA_audio_ctrl
	; pre-load the FIFO
	dec active_digi
	jsr play_pcm	; prime the FIFO with at least 1 frame's worth of data.
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

	totalbytes		= digi + DIGITAB::size
	bytesperframe 	= digi + DIGITAB::perframe
	digi_addr		= digi + DIGITAB::addr
	digi_bank		= digi + DIGITAB::bank
	
	lda	active_digi		; quick check whether digi player is active
	clc
	beq noop
	lda totalbytes		; check whether any bytes remain in the digi
	ora totalbytes+1
	ora totalbytes+2
	bne :+
end_of_digi:
	stz active_digi		; ... if not, then deactivate digi player
	rts
:	sec
	; configure the ZP pointer for load_fifo
	lda digi_addr
	sta pcm_ptr
	lda digi_addr+1
	sta pcm_ptr+1
	lda digi_bank
	sta pcm_bank
	; call load_fifo with the lesser of totalbytes or bytesperframe
	lda totalbytes
	sbc bytesperframe
	lda totalbytes+1
	sbc bytesperframe+1
	bcs send_bytesperframe
send_totalbytes: ; i.e. the last frame's worth of samples.
	ldx totalbytes
	ldy totalbytes+1
	bra call_load_fifo
send_bytesperframe:
	ldx bytesperframe
	ldy bytesperframe+1
call_load_fifo:
	jsr	load_fifo
	php
	; update the pcmtab 
	lda pcm_ptr
	sta digi_addr
	lda pcm_ptr+1
	sta digi_addr+1
	lda pcm_bank
	sta digi_bank
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
	bpl done
	brk
	stz active_digi
	; note: wouldn't carry=clear here mean the digi is done?
done:
	plp
noop:
	rts
.endproc


;---------------------------------------------------------------
; load_fifo: 
;
; Copies PCM data into FIFO - stops when either max bytes are copied
; or if the FIFO becomes full. Returns number of bytes copied.
;
; Arguments:
;	.X/.Y = max bytes to copy
;
; Returns:
;	CC if complete. CS if stop early for any reason.
;	Sets pcmblit = number of bytes written into FIFO.
;
; Setup:
;	Before calling, the ZP variables pcm_ptr and pcm_bank should be set.
;
; Affects:
;	ZP variable pcmblit is used to store the remaining byte countdown.
;
;---------------------------------------------------------------
.segment "CODE"
.proc load_fifo: near

	; .XY = num bytes to copy
	; save as bytes_in, and copy to pcmblit (workspace in ZP)
	stx bytes_in
	sty bytes_in+1
	sty pcmblit+1
	ldy pcm_ptr
	stz pcm_ptr		; page-align the data pointer
	lda RAM_BANK
	sta BANK_SAVE
	lda pcm_bank
	sta RAM_BANK
	bra copy_byte
loop:
	dex
	bne copy_byte
	lda pcmblit+1
	clc				; clear carry for return status in case we're done
	beq done
	dec
	sta pcmblit+1
copy_byte:
	bit VERA_audio_ctrl	; check FIFO full bit
	bmi early_exit
	lda (pcm_ptr),y
	sta VERA_audio_data
	nop
	; advance pointer to the next byte, do bank wrap if necessary
	iny
	bne loop
	lda pcm_ptr+1
	inc
	cmp #$c0
	bne nowrap
	lda #$a0
	inc RAM_BANK
nowrap:
	sta pcm_ptr+1
	bra loop

early_exit:
	sec
done:
	php
	stx pcmblit
	sty pcm_ptr
	lda RAM_BANK
	sta pcm_bank
	lda #$FF
	BANK_SAVE = (*-1)
	sta RAM_BANK
	sec
	lda bytes_in
	sbc pcmblit
	sta pcmblit
	lda bytes_in+1
	sbc pcmblit+1
	sta pcmblit+1
	plp
	rts

bytes_in:	.res 2

.endproc	


;---------------------------------------------------------------
.segment "CODE"
.proc stop_pcm: near
	stz VERA_audio_rate
	lda #$80
	sta VERA_audio_ctrl
	stz active_digi
	rts
.endproc
