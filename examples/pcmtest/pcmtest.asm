.include "x16.inc"
.segment "ZEROPAGE"

pcm_ptr:		.res 2
pcmbank:		.res 1
pcmbytes:		.res 2
pcmblit:		.res 2
active_digi:	.res 1

; for test program only
pcmcfg:			.res 1
pcmrate:		.res 1

.struct PCMTAB
	addr		.addr
	bank		.byte
	cfg			.byte
	perframe	.byte 2
	size		.byte 3
	rate		.byte
.endstruct

PCM_RATE_8000	= (8000/(25000000 >> 16))

.segment "RODATA"
pcmtab_lo:	.byte	<digi_coin, <digi_start
pcmtab_hi:	.byte	>digi_coin, >digi_start

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
	.word	33830
	.byte	0
	.byte	PCM_RATE_8000

.segment "BSS"
bytesperframe:	.res 2
totalbytes:		.res 3


;---------------------------------------------------------------
.segment "STARTUP"
	jmp	init
	jmp coin_sound
	jmp startup_tune
	rts
	
coin_sound:
	ldy #0
	lda pcmtab_hi,y
	tax
	lda pcmtab_lo,y
	tay
	lda #1
	jsr start_digi
	rts
	
startup_tune:
	ldy #1
	lda pcmtab_hi,y
	tax
	lda pcmtab_lo,y
	tay
	lda #1
	jsr start_digi
	rts

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
	TABLE = pcmblit
	stx	TABLE
	sty TABLE+1
	tax
	jsr stop_pcm
	lda RAM_BANK
	sta BANK_SAVE
	; address:bank
loop1:
	ldy #2
	lda (TABLE),y
	sta pcm_ptr,y
	dey
	bpl loop1
	ldy #3
	; audio_ctrl (PCM format)
	lda (TABLE),y
	ora #$80
	sta VERA_audio_ctrl
loop2:
	iny
	cpy #PCMTAB::rate
	lda (TABLE),y
	beq :+
	sta bytesperframe,y
	bra loop2
:	stx active_digi
	pha
	; pre-load the FIFO
	jsr play_pcm	; convert this into 2 calls to load_fifo or a step_pcm frontend.
	pla
	sta VERA_audio_rate
	lda #$FF
	BANK_SAVE := (*-1)
	sta RAM_BANK
	rts
.endproc

;---------------------------------------------------------------
.segment "CODE"
.proc play_pcm: near
	lda	#$ff
	cmp active_digi
	beq done
	sec
	; call load_fifo with the lesser of totalbytes or bytesperframe
	lda totalbytes
	sbc bytesperframe
	lda totalbytes+1
	sbc bytesperframe+1
	bmi send_totalbytes
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
	pha
	; determine which is less: pcmblit or bytes left in current page
	sec
	lda #0
	sbc pcm_ptr
	sta pcmblit
	tay				; if pcmblit is smaller, then Y should be pcmblit
	lda #0
	adc #0
	sta pcmblit+1	; pcmblit = num bytes remaining on this page
	
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
	lda pcmbank
	sta RAM_BANK
	dey
	beq last_byte
loop:
	; TODO:	add check for full FIFO
	lda (pcm_ptr),y
	sta VERA_audio_data
	dey
	bne loop
last_byte:
	lda (pcm_ptr),y
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
	pla
	sta RAM_BANK
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
	lda #$ff
	sta active_digi
	rts
.endproc
