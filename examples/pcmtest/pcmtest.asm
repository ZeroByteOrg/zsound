.include "x16.inc"
.segment "ZEROPAGE"

; ZP variables potentially used in zsound
pcm_ptr:		.res 2
pcm_bank:		.res 1
end_ptr:		.res 2
end_bank:		.res 1

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

;startup_tune:
;	ldy #1
;	lda pcmtab_lo,y
;	tax
;	lda pcmtab_hi,y
;	tay
;	lda #1
;	jsr start_digi
;	rts

startup_tune:
	ldx pcmtab_lo+1
	stx pcmbytes
	ldx pcmtab_hi+1
	stx pcmbytes+1
	ldy DIGITAB_LAST
:	lda (pcmbytes),y
	sta digi,y
	dey
	bpl :-
	;;;; continue here
	
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
	sta pcm_bank
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
	rts
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
; load_fifo: 
;
; Arguments:
;	.X/.Y = starting address
;
; Setup:	Before calling, the current source RAM bank should be active
;			and the ZP variables end_ptr,end_bank should be set.
;
; Affects:	If a bank wrap occurs, the current source RAM bank will
;			be updated.
;			
; Returns:	CC if complete. CS if stop early for any reason.
;			Modifies the digi's pointer in BSS memory (digi::addr)
;
; Copies PCM data into FIFO from starting address up to and including
; an ending address specified in ZP. Updates the digi information's
; pointer to point at the first un-copied byte.
;
; Exits early with noop if dst page >= $c0
; Exits early if FIFO becomes full
;---------------------------------------------------------------
.segment "CODE"
.proc load_fifo: near

.macro NEXTPAGE
	inx
	cpx #$c0
	bne @nowrap
	ldx #$a0
	inc RAM_BANK
@nowrap:
	stx pcm_ptr+1
.endmacro

	lda #$c0		; avoid infinite loop from end ptr above bank window
	cmp	end_ptr+1
	bcs noop
	lda end_ptr
	sta STOPVAL
	; initialize pcm_ptr aligned to current page.
	stz pcm_ptr
	stx pcm_ptr+1
	bra continue
loop:
	iny
	bne continue
	NEXTPAGE
continue:
	lda #$08
	and VERA_isr	; is FIFO full?
	bne stop_load
	lda (pcm_ptr),y
	sta VERA_audio_data
	; check whether this was the last byte to copy
	cpy #$FF
	STOPVAL = (*-1)	; self-mod target for the end address low-byte
	bne loop
	cpx	end_ptr
	bne loop
	lda end_bank
	cmp RAM_BANK
	bcc loop
	; point at the first byte after the final byte copied.
	iny
	bne done
	NEXTPAGE
done:
	clc
exit:
	sty digi + DIGITAB::addr
	stx digi + DIGITAB::addr+1
	lda RAM_BANK
	sta digi + DIGITAB::bank
	clc
noop:
	rts
stop_load:
	sec
	bra exit
.endproc

;---------------------------------------------------------------
; This code might not be used after the new load_fifo is done...
; remember to delete it if unused.
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
