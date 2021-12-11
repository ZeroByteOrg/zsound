.include "x16.inc"

.segment "ZEROPAGE"

; MEMO TO SELF: The blit routine is STILL bugging out on some case or other and
; jumping into the BRK whereby a negative number of bytes written to FIFO is
; being computed. Not sure what I'm doing wrong - but that's where to resume
; debugging later.



; ZP variables potentially used in zsound
pcm_ptr:		.res 2
pcmbytes:		.res 2
pcm_bank:		.res 1


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
	perframe	.byte
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
	.byte	1
	.word	1739
	.byte	0		; high byte of 24-bit size
	.byte	PCM_RATE_8000
digi_start:
	.addr	$a000 + 1739
	.byte	1
	.byte	$0f
	.byte	1
	.word	38830
	.byte	0
	.byte	PCM_RATE_8000
.endif

.if 1
digi_coin:
	.addr	$a000
	.byte	1
	.byte	$0f
	.byte	2
	.word	2653
	.byte	0		; high byte of 24-bit size
	.byte	PCM_RATE_12207
digi_start:
	.addr	$a000 + 2653
	.byte	1
	.byte	$0f
;	.byte	2
	.byte	8
	.word	51620
	.byte	0
	.byte	PCM_RATE_12207
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
forever:
	bra forever
	
coin_sound:
	ldy #0
	lda pcmtab_lo,y
	tax
	lda pcmtab_hi,y
	tay
	lda #1
	jmp start_digi

startup_tune:
	ldy #1
	lda pcmtab_lo,y
	tax
	lda pcmtab_hi,y
	tay
	lda #1
	jmp start_digi


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
	pagesperframe 	= digi + DIGITAB::perframe
	digi_addr		= digi + DIGITAB::addr
	digi_bank		= digi + DIGITAB::bank
	
	lda	active_digi		; quick check whether digi player is active
	clc
	bpl noop
	lda totalbytes		; check whether any bytes remain in the digi
	ora totalbytes+1
	ora totalbytes+2
	bne :+
end_of_digi:
	stz active_digi		; ... if not, then deactivate digi player
	rts
:	sec
	; swap in the PCM data bank
	lda RAM_BANK
	sta BANK_SAVE
	lda digi_bank
	sta RAM_BANK
	; configure the ZP pointer for load_fifo
	lda digi_addr+1
	sta pcm_ptr+1
	ldy digi_addr
	; determine if totalbytes < 256*pagesperframe
	lda totalbytes+2
	bne send_pagesperframe
	lda totalbytes+1
	cmp pagesperframe
	bcs send_pagesperframe
send_totalbytes: ; i.e. the last frame's worth of samples.
	ldx totalbytes+1
	stx pcmbytes+1
	ldx totalbytes
	jsr load_fifo
	bcs update_totalbytes
	stz totalbytes
	stz totalbytes+1
	lda pagesperframe
	sta active_digi
	bra exit
noop:	; note - this is branced to from beginning of routine. keep it in range
	beq :+
	dec active_digi
:	rts
send_pagesperframe:
	ldx pagesperframe
	stx pcmbytes+1
	ldx #0
	jsr	load_fifo
	; update the pcmtab 
	lda pcm_ptr
	sta digi_addr
	lda pcm_ptr+1
	sta digi_addr+1
	lda RAM_BANK
	sta digi_bank
update_totalbytes:
	sec
	lda totalbytes
	sbc pcmbytes
	sta totalbytes
	lda totalbytes+1
	sbc pcmbytes+1
	sta totalbytes+1
	lda totalbytes+2
	sbc #0
	sta totalbytes+2
	sec
exit:
	lda #$FF
	BANK_SAVE = (*-1)
	sta RAM_BANK
	rts
.endproc


;---------------------------------------------------------------
; load_fifo: 
; uses ZP: pcm_ptr, pcm_bytes (16bit).
;	note: it's impossible for more than 4000 bytes to be successfully
;	copied so no need to deal with 24-bit values here.
;
; copies until FIFO full or pcm_bytes=0.
; leaves pointer at address after last successful write.
; leaves pcm_bytes = num bytes loaded
; returns CC if finished, CS if FIFO full
;
; .X = low byte of bytes remaining
; .Y = low byte of pcm_ptr
;---------------------------------------------------------------
.segment "CODE"
.proc load_fifo: near

;	lda pcm_ptr+1
;	sta ptr_sent+1
;	sty ptr_sent
;	lda RAM_BANK
;	sta ptr_sent+2
	
	stz pcm_ptr	; ensure the pointer is page-aligned
	stx bytes_requested
	lda pcmbytes+1
	sta bytes_requested+1
;	cpx #0
;	bne copy_byte
;	cmp #0
;	beq finished	; why would you DO that?
	jmp check_done

loop_bankwrap:
	lda #$a0
	inc RAM_BANK
loop:
	sta pcm_ptr+1
check_done:
	cpx #0
	bne dec1
	lda pcmbytes+1
	cmp #0
	bne dec2
finished:
	sty pcm_ptr
	lda bytes_requested
	sta pcmbytes
	lda bytes_requested+1
	sta pcmbytes+1
	clc
	rts
dec2:
	dec pcmbytes+1
dec1:
	dex
copy_byte:
	bit VERA_audio_ctrl	; check FIFO full bit
	bmi fifo_full
	lda (pcm_ptr),y
	sta VERA_audio_data
	iny
	bne check_done
	; advance pointer to the next page. Do bank wrap if necessary
	lda pcm_ptr+1
	inc
	cmp #$c0
	bne loop
	bra loop_bankwrap
fifo_full:
	sty pcm_ptr
	stx pcmbytes
;------ DEBUGGING
;	stx bytes_remaining
;	lda pcmbytes+1
;	sta bytes_remaining+1
;------
	sec
	lda bytes_requested
	sbc pcmbytes
	adc #1	; we dec'd X before determining whether FIFO was full
	sta pcmbytes
	lda bytes_requested+1
	sbc pcmbytes+1
	sta pcmbytes+1
	bcc wtf
	sec
	rts
wtf:
	brk

; for zsound - integrate this with the library's memory instead of putting it here.
bytes_requested: .byte 0,0
bytes_remaining: .byte 0,0
ptr_sent:		 .byte 0,0,0

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
