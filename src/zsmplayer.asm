; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"


; import revision-suffixed symbols
.import	.ident(.sprintf("%s%d","nextdata",X16_VERSION))

; alias them with the revision suffixes stripped
nextdata	:=	.ident(.sprintf("%s%d","nextdata",X16_VERSION))

; create revision-suffixed identifiers for the symbols being exported
; e.g. init_player38 := init_player
.ident(.sprintf("%s%d","init_player",X16_VERSION)) := init_player
.ident(.sprintf("%s%d","playmusic",X16_VERSION)) := playmusic
.ident(.sprintf("%s%d","startmusic",X16_VERSION)) := startmusic
.ident(.sprintf("%s%d","stopmusic",X16_VERSION)) := stopmusic
.ident(.sprintf("%s%d","data",X16_VERSION)) := data

; export the revision-suffixed symbols
.export	.ident(.sprintf("%s%d","init_player",X16_VERSION))
.export	.ident(.sprintf("%s%d","playmusic",X16_VERSION))
.export	.ident(.sprintf("%s%d","startmusic",X16_VERSION))
.export	.ident(.sprintf("%s%d","stopmusic",X16_VERSION))
.export	.ident(.sprintf("%s%d","data",X16_VERSION))

ZSM_HDR_SIZE	=	16	; will soon be larger
ZSM_EOF			=	$80	; (equates to pause cmd, value=0)

.segment "ZEROPAGE"

data:	.tag	SONGPTR
delay:	.res	1
cmd:	.res	1

.segment "BSS"

loop_pointer:	.tag	SONGPTR
tmp:			.tag	SONGPTR

.segment "CODE"

; ---------------------------------------------------------------------------
; init_player:
;
; Arguments: (none)
; Returns: (none)
; Affects: A
;
; ---------------------------------------------------------------------------
;
; Initializes the memory locations used by ZSM player to a stopped playback
; state, with the data pointer pointing at an "end-of-data" command frame.
;
.proc init_player: near
			stz delay ; "not playing"
			lda #$FF  ; "no looping"
			sta	loop_pointer + SONGPTR::bank
			stz loop_pointer + SONGPTR::addr
			stz loop_pointer + SONGPTR::addr+1
			lda #<dummy_tune
			sta data + SONGPTR::addr
			lda #>dummy_tune
			sta data + SONGPTR::addr + 1
			stz data + SONGPTR::bank	; bank doesn't matter
			rts
	
dummy_tune:	.byte ZSM_EOF	; dummy "end of tune" byte - point to this
.endproc

; ---------------------------------------------------------------------------
; startmusic: 
;
; Arguments:
;	A	: HIRAM bank of tune
;	X/Y	: Memory address of beginning of ZSM header
;
; Returns: Carry flag: 0=success, 1=fail
;
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Initializes the song data pointer, loop pointer, and sets delay = 1
; Music will begin playing on the following frame
;

.proc startmusic: near
			; ensure music does not attempt to play due to an IRQ
			stz delay
			; store the passed arguments into data pointer and a tmp space
			sta data + SONGPTR::bank
			stx data + SONGPTR::addr
			sty data + SONGPTR::addr+1
			sta tmp + SONGPTR::bank
			stx tmp + SONGPTR::addr
			sty tmp + SONGPTR::addr+1
			; bank in the music data
			ldx RAM_BANK
			phx		; save current BANK to restore later
			sta RAM_BANK
			; copy the loop pointer from the header data into main memory
			lda (data)
			sta loop_pointer + SONGPTR::addr
			jsr nextdata
			lda (data)
			sta	loop_pointer + SONGPTR::addr + 1
			jsr nextdata
			lda	(data)
			sta loop_pointer + SONGPTR::bank
			tax
			; move data pointer past the remaining header bytes
			ldy #(ZSM_HDR_SIZE-2)
:			jsr nextdata
			dey
			bne :-
			cpx #$FF	; check if there is a loop or not
			beq	done
			; add load address to loop pointer
			clc
			lda tmp + SONGPTR::addr
			adc loop_pointer + SONGPTR::addr
			sta loop_pointer + SONGPTR::addr
			lda loop_pointer + SONGPTR::addr + 1
			cmp #$20
			bcs die ; invalid loop data >= $2000 
			adc tmp + SONGPTR::addr + 1
			cmp #$c0
			bcc	calculate_bank
			sbc #$20
			inc loop_pointer + SONGPTR::bank
calculate_bank:
			sta loop_pointer + SONGPTR::addr + 1
			lda tmp + SONGPTR::bank
			adc loop_pointer + SONGPTR::bank
			bcs	die ; loop bank points past end of HIRAM
			cmp #$FF	; did we end up with loop bank = FF?
			beq die		; if so, then die (FF is an invalid loop bank)
			sta loop_pointer + SONGPTR::bank
			
done:		pla
			sta RAM_BANK
			lda #1
			sta delay	; start the music
			clc			; return clear carry flag to indicate no errors
			rts
die:
			pla
			sta RAM_BANK
			stz delay	; ensure the music is not playing
			sec
			rts
.endproc

; ---------------------------------------------------------------------------
; stopmusic: 
;
; Arguments: (none)
; Returns: (none)
; Affects: (none)
; ---------------------------------------------------------------------------
;
; Halts music playback.
; TODO: release the voices used by the tune? Use an arg to determine this?
;

.proc stopmusic: near
			stz	delay
			rts
.endproc

; ---------------------------------------------------------------------------
; playmusic: 
;
; Arguments: (none)
; Returns: Carry flag: 0=success, 1=fail
;
; Affects: A,X,Y, VERA CTRL and data port 0, RAM_BANK register
; ---------------------------------------------------------------------------
;
; Call once per frame - NOT FROM IRQ AS THIS ROUTINE CLOBBERS EVERYTHING
;

noop:		rts

delayframe:
			and #$7F		; mask off the delay command flag
			beq loopsong
			sta delay
			jmp nextdata

playmusic:
			; first check the delay. 0 = not playing.
			lda delay
			beq noop
			; delay >0. Decrement, and if now 0, then play, else exit.
			dec	
			sta delay
			bne noop
			; bank in the song data
			lda data + SONGPTR::bank
			sta RAM_BANK
			; point VERA to PSG page / set data port = 0 in CTRL
			lda #$FE
			and VERA_ctrl	; clear bit0 of CTRL register to select data0
			sta VERA_ctrl
			lda #$01		; bank 1, 0 stride
			sta VERA_addr_bank
			lda #$f9		; PSG are on page $F9 of VRAM
			sta VERA_addr_high

nextnote:
			lda (data)			; 5
			bmi delayframe  	;
								; 2
			bit #$40			; 2
			bne YMPCM			; 
playPSG:						; 2
			clc					; 2
			adc #$c0			; 2		; ...to offset it properly into VRAM location
			sta VERA_addr_low	; 4		; VERA data0 now points at selected PSG register
			jsr nextdata		; +X
			lda (data)			; 5		; get the value for writing into PSG
			sta VERA_data0		; 4		; ... and write it.
			jsr nextdata		; +X
			bra nextnote		; 3

YMPCM:							; 3
			and #$3f			; 2
			beq PCMcommand		;
playYM:							; 2
			tax					; 2		; X now holds number of reg/val pairs to process
nextYM:	
			jsr nextdata
			dex
			bmi nextnote	; note: the most YM writes is 63, so this is a safe test
			lda (data)
			tay				; Y now holds the YM register address
			jsr nextdata
			YM_BUSY_WAIT
			sty YM_reg
			lda (data)
			sta YM_data
			bra nextYM		; 3
PCMcommand:
			jsr nextdata
			rts				; no PCM commands defined yet...

loopsong:
			; check if loop_ptr bank = $FF
			lda loop_pointer+SONGPTR::bank
			cmp	#$FF
			bne :+
			jmp stopmusic
:			sta data + SONGPTR::bank
			sta RAM_BANK
			lda	loop_pointer + SONGPTR::addr
			sta	data + SONGPTR::addr
			lda	loop_pointer + SONGPTR::addr+1
			sta	data + SONGPTR::addr+1
			jmp	nextnote
