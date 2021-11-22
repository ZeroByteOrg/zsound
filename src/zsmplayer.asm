; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"


IMPORT_TAGGED "nextdata"

EXPORT_TAGGED "init_player"
EXPORT_TAGGED "stepmusic"
EXPORT_TAGGED "startmusic"
EXPORT_TAGGED "stopmusic"
EXPORT_TAGGED "data"
EXPORT_TAGGED "playmusic"

ZSM_HDR_SIZE	=	16	; does not include PRG header which isn't loaded
ZSM_EOF			=	$80	; (equates to pause cmd, value=0)

.segment "ZEROPAGE"

data:		.tag	SONGPTR
delay:		.res	1
fracstep:	.res	1			; residual steps per frame
step:		.res	2			; integer steps per frame

.segment "BSS"

loop_pointer:	.tag	SONGPTR
tmp:			.tag	SONGPTR
zsm_chanmask:	.tag	CHANMASK
zsm_fracsteps:	.res	3	; 16.8 fixed point: n steps per 60hz frame
zsm_steps	:= zsm_fracsteps + 1

;macro to choose the fastest available ticks/step routine.
.macro CHOOSE_PLAYER
	ldx #0				; 0=1step/frame, 1=steps is byte, 2=steps is word
	lda zsm_steps+1		; if high byte is nonzero, it's step_word.
	bne @set_16
	lda zsm_fracsteps
	beq @check_60hz
	lda zsm_steps
	cmp #$ff
	beq	@set_16			; if frac > 0 and lo-byte=255, there will be some frames
						; with 256 steps, so use step_word
@check_60hz:
	lda zsm_steps
	cmp #1
	beq @set_1
	bra @set_8
@set_16:
	inx
@set_8:
	inx
@set_1:
	lda player_table_lo,x
	sta playmusic+1
	lda player_table_hi,x
	sta playmusic+2
.endmacro

;--------------------------------------------------------------------------

.segment "CODE"
playmusic:
	jmp	step_byte	; startmusic will modify this to point to the
					; fastest steps-per-frame shell or directly to
					; stepmusic if HZ=60

player_table_lo:	.byte <stepmusic, <step_byte, <step_word
player_table_hi:	.byte >stepmusic, >step_byte, >step_word
	

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
.segment "CODE"
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
			stz zsm_chanmask + CHANMASK::fm
			stz zsm_chanmask + CHANMASK::psg
			stz zsm_chanmask + CHANMASK::psg+1
			stz step
			stz step+1
			stz fracstep
			lda #1
			sta	zsm_steps
			stz zsm_steps+1
			stz zsm_steps+2
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
; Copies channel mask into main memory, and calculates ticks/frame rate
; Music will begin playing on the following frame
;
.segment "CODE"
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
			jsr nextdata
			; move data pointer past the PCM header offset bytes
			jsr nextdata
			jsr nextdata
			jsr nextdata
			; copy channel mask out of banked memory
			ldx #3
:			lda (data)
			sta zsm_chanmask,x
			jsr nextdata
			dex
			bne :-
			; get song playback rate and convert to ticks/frame
			lda (data)
			tax
			jsr nextdata
			lda (data)
			tay
			jsr nextdata
			jsr setmusicspeed
			; move pointer to the first byte of music data
			ldx #5
:			jsr nextdata
			dex
			bne :-
			; check if there is a loop or not
			lda #$FF
			cmp loop_pointer + SONGPTR::bank
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
			bcs	die 	; loop bank points past end of HIRAM
			cmp #$FF	; did we end up with loop bank = FF?
			beq die		; if so, then die (FF is an invalid loop bank)
			sta loop_pointer + SONGPTR::bank
			
done:		pla
			sta RAM_BANK
			lda #1
			sta delay	; start the music
			clc			; return clear carry flag to indicate success
			rts
die:
			pla
			sta RAM_BANK
			stz delay	; ensure the music is not playing
			sec			; return carry flag set to indicate failure
			rts	
.endproc

; ---------------------------------------------------------------------------
; setmusicspeed: 
;
; Arguments:
;	X/Y	: Playback speed in Hz. (x=lo, y=hi)
;
; Returns: (none)
;
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Converts Hz into ticks/frame, sets the zsm_steps.zsm_fracstep variables
; and modifies the shell function playmusic: to call one of the three music
; step functions. stepmusic = once. step_word treats zsm_steps as a 16.8 value
; and step_byte treats it as an 8.8 (used if ticks/frame < 255)
;
.proc setmusicspeed: near
			; X/Y = tick rate (Hz) - divide by 60 and store to zsm_steps
			; use the ZP variable as tmp space
			
			value := r0			; use kernal ZP r0 tmp space
			frac  := r1			; but with meaningul names here
			stx value
			sty value+1
			stz frac
			txa
			ora value+1
			bne hz_to_tickrate
			lda #60				; set speed = 60hz if no rate specified in ZSM
			sta value
hz_to_tickrate:
			ldx #6
			jsr rshift3			; value >> 6 (value = Hz/64)
			; set step = value.
			lda value
			sta step
			lda value+1
			sta step+1
			lda frac
			sta fracstep
			; currently, step = value = Hz/64.
			; To make step = Hz/60, It should now get value/15 added to it.
			; We do value/15 by successively dividing value by 16 and adding
			; the result to step, which approaches the correct value with
			; each pass. After 4 passes, we will have exceeded the precision
			; of 16.4 fixed point, so that's when to stop.
			ldx #4 ; 4 means >> 4
			jsr rshift3 ; still need to rotate all 3 bytes
			jsr add_step
			; 1 pass complete.
			jsr rshift2 ; falls through to add_step
			jsr rshift2
			; 3 passes complete. The last pass is shortest (only frac remains)
			lsr frac
			adc fracstep
			; when doing the carry bit adds, save the results in zsm_steps.zsm_fracsteps
			; because we're finally done.
			sta zsm_fracsteps
			lda step
			adc #0
			sta zsm_steps
			lda step+1
			adc #0
			sta zsm_steps+1
			CHOOSE_PLAYER
			rts
			
rshift3:	; rshift 3 byte value (X = number of shifts)
			lsr value+1
			ror value
			ror frac
			dex
			bne rshift3
			rts
			
rshift2:
			ldx #4	; we're always >>4 in this phase
:			lsr value
			ror frac
			dex
			bne :-
			; fall through to add_step to save a little CPU time
			; i.e. no RTS followed by JSR add_step.
			
add_step:
			; note that the carry flag is set by the rshift routine and is
			; important, so no CLC statement as in the normal ADC usage...
			lda frac
			adc fracstep
			sta fracstep
			lda value
			adc step
			sta step
			lda step+1
			adc #0	; high byte of value is now guaranteed to be 0.
			sta step+1
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
; TODO: release the voices used by the tune
;       rquires implementation of the channel mask from the ZSM header
;		vgm2zsm now writes the header. Next is adding support here
;

.segment "CODE"
.proc stopmusic: near
			stz	delay
			rts
.endproc

; ---------------------------------------------------------------------------
; stepmusic: 
;
; Arguments: (none)
; Returns: Carry flag: (currently broken) 0=playing, 1=stopped or looped
;
; Affects: A,X,Y, VERA CTRL and data port 0, RAM_BANK register
; ---------------------------------------------------------------------------
;
; Advances the music by one tick.
; Call as many times per frame as required by the ZSM's playback rate.
; (usually 60Hz - once per frame)
; THIS ROUTINE IS NOT SAFE to call directly during IRQ, as it clobbers VERA
; registers w/o fixing them (for speed reasons). If your program
; is designed to run the music player during an IRQ, use one of the
; IRQ-safe wrapper functions that save and restore VERA before calling this
; core routine.
;

.proc stepmusic: near
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
			bra nextnote

noop:		rts

delayframe:
			and #$7F		; mask off the delay command flag
			beq loopsong
			sta delay
			jmp nextdata	; advance data pointer and exit

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
			; PCM commands are 4 bytes. Skip for now.
			jsr nextdata
			jsr nextdata
			jsr nextdata
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
.endproc


; ---------------------------------------------------------------------------
; step_16: 
;
; Arguments: (none)
; Returns: should be similar to step_music - not implemented yet here.
; Affects: A,X
; ---------------------------------------------------------------------------
;
; calls stepmusic however many times specified in ZP step.fracstep
; This one is used if steps >= 255 and has nonzero fracstep.
;

.proc step_word: near
;			; early exit if called when data points at EOF.
;			lda (data)
;			cmp #ZSM_EOF
;			beq done
			; add 0.fracsteps to zsm_steps.zsm_fracsteps, storing result
			; in ZP steps.fracsteps
			clc
			lda zsm_fracsteps
			adc fracstep
			sta fracstep
			lda zsm_steps
			adc #0
			sta step
			lda zsm_steps+1
			adc #0
			sta step+1
			; call step_music that many times
next_step:	
			lda step
			bne	call_stepmusic
			lda step+1
			beq done
			dec
			sta step+1
call_stepmusic:
			dec step
			jsr stepmusic
			; TODO: handle nonzero CC flag returns from stepmusic
			bra next_step
done:		rts

.endproc

; ---------------------------------------------------------------------------
; step_8: 
;
; Arguments: (none)
; Returns: should be similar to step_music - not implemented yet here.
; Affects: A,X
; ---------------------------------------------------------------------------
;
; calls stepmusic however many times specified in ZP step.fracstep
; This one is used if steps is < 255. (255 goes to 16bit version because
; chance of carry from frac causing an overflow in a single byte)
;
.proc step_byte: near
;			; early exit if called when data points at EOF.
;			lda (data)
;			cmp #ZSM_EOF
;			beq done
			clc
			lda zsm_fracsteps
			adc fracstep
			sta fracstep
			lda zsm_steps
			adc #0
			sta step
			beq done
next_step:
			jsr	stepmusic
			dec step
			bne next_step
done:		rts
.endproc
