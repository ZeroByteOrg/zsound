.include "x16.inc"

.segment "ONCE"

.segment "ZEROPAGE"
ptr:	.res 2

.segment "CODE"
main:
	lda #$a0
	sta ptr+1
	stz ptr
	lda #1
	sta RAM_BANK
	stz VERA_audio_rate
	lda #$8f
	sta VERA_audio_ctrl
	ldy #0
	ldx #16	; write 4kb (16 pages) into FIFO
loop:
	lda (ptr),y
	sta VERA_audio_data
	iny
	bne loop
	inc ptr+1
	dex
	bne loop
	;start PCM playback
	lda #32
	sta VERA_audio_rate
	;write into FIFO until done
loop2:
	bit VERA_audio_ctrl
	bmi loop2
	lda (ptr),y
	sta VERA_audio_data
	iny
	bne loop2
	inc ptr+1
	lda #$c0
	cmp ptr+1
	bne loop2
	lda #$a0
	sta ptr+1
	inc RAM_BANK
	lda RAM_BANK
	cmp #8
	bcc loop2
	rts
	
	
.segment "STARTUP"
loadpcm:
	; set BANKRAM to the first bank where song should load
	lda	#1
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
	; load song to $A000
	lda	#0		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
	ldx	#0
	ldy #$a0
	jsr LOAD
	jmp main

.segment "RODATA"
filename:	.byte "raw2"
filename_len	= (*-filename)
