; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"
.include "macros.inc"


IMPORT_TAGGED "nextdata"
IMPORT_TAGGED "helloworld"

EXPORT_TAGGED "init_player"
EXPORT_TAGGED "stepmusic"
EXPORT_TAGGED "startmusic"
EXPORT_TAGGED "stopmusic"
EXPORT_TAGGED "data"
EXPORT_TAGGED "playmusic"
EXPORT_TAGGED "playmusic_IRQ"
EXPORT_TAGGED "setmusicspeed"
EXPORT_TAGGED "setloopcount"
EXPORT_TAGGED "setcallback"

ZSM_HDR_SIZE	=	16	; does not include PRG header which isn't loaded
ZSM_EOF			=	$80	; (equates to pause cmd, value=0)

.segment "ZEROPAGE"

data:		.tag	SONGPTR
delay:		.res	1
; these next two probably need to use TMP ZP space, not permanent.....
fracstep:	.res	1			; residual steps per frame
step:		.res	2			; integer steps per frame

.segment "BSS"

loop_pointer:	.tag	SONGPTR
tmp:			.tag	SONGPTR
loop_count:		.res	1
zsm_chanmask:	.tag	CHANMASK
zsm_fracsteps:	.res	3	; 16.8 fixed point: n steps per 60hz frame
zsm_scale60hz:	.res	1	; flag for whether to use the rate->60hz conversion player

zsm_steps	:= zsm_fracsteps + 1

; Vector table for various callback pointers
ZSM_VECTOR_TABLE = *
ZSM_VECTOR_pcmcall:		.res 2	; PCM music track handler
ZSM_VECTOR_user:		.res 2	; custom data event handler
ZSM_VECTOR_done:		.res 2	; callback to handle ZSM data EOF
ZSM_VECTOR_notify:		.res 2	; callback when music loops or ends
ZSM_VECTOR_play:		.res 2  ; music playback routine
ZSM_VECTOR_COUNT	= (*-ZSM_VECTOR_TABLE)


;macro to choose the fastest available ticks/step routine.
.macro CHOOSE_PLAYER
	lda zsm_scale60hz
	beq @set_1
	lda zsm_steps+1
	bne @set_scale
	lda zsm_fracsteps
	bne @set_scale
	lda zsm_steps
	cmp #1
	bne @set_scale
@set_1:
	lda #<stepmusic
	sta ZSM_VECTOR_play
	lda #>stepmusic
	sta ZSM_VECTOR_play+1
	bra @done	; the next command after this macro's use is rts....
@set_scale:
	lda #<step_word
	sta ZSM_VECTOR_play
	lda #>step_word
	sta ZSM_VECTOR_play+1
@done:
.endmacro

;.macro CHOOSE_PLAYER_ORIG
;	ldx #0				; 0=1step/frame, 1=steps is byte, 2=steps is word
;	lda zsm_steps+1		; if high byte is nonzero, it's step_word.
;	bne @set_16
;	lda zsm_fracsteps
;	beq @check_60hz
;	lda zsm_steps
;	cmp #$ff
;	beq	@set_16			; if frac > 0 and lo-byte=255, there will be some frames
;						; with 256 steps, so use step_word
;	bra @set_8			; else use step_byte
;@check_60hz:
;	lda zsm_steps
;	cmp #1
;	beq @set_1
;	bra @set_8
;@set_16:
;	inx
;@set_8:
;	inx
;@set_1:
;	lda player_table_lo,x
;	sta ZSM_VECTOR_play
;	lda player_table_hi,x
;	sta ZSM_VECTOR_play+1
;.endmacro



;--------------------------------------------------------------------------

.segment "CODE"
null_handler:
			rts

; shells for stepmusic
.segment "CODE"
playmusic:
			; early exit if delay=0 (music not playing).
			lda delay
			beq done
			jmp	(ZSM_VECTOR_play)
done:		rts

playmusic_IRQ:
			; early exit if delay=0 (music not playing).
			lda delay
			beq done
			; save the current active RAM bank
			lda RAM_BANK
			sta V5
			; save the current state of VERA CTRL register
			lda	VERA_ctrl
			sta V4
			stz VERA_ctrl	; use data0 for player routine
			; save the current state of VERA addr registers for data0
			lda VERA_addr_low
			sta	V3
			lda VERA_addr_high
			sta V2
			lda VERA_addr_bank
			sta V1
			; manufacture a RTS return into this routine
			lda #>RETURN
			pha
			lda #<RETURN
			pha
			jmp (ZSM_VECTOR_play)	; since jsr (abs) is not a valid addressing mode...
			; restore VERA data port state, ctrl register, and active RAM bank
			RETURN = (*-1) ; return point from play routine
			lda #$FF	; vera backups stored here as self-mod code
			V1 = (*-1)
			sta VERA_addr_bank
			lda #$FF
			V2 = (*-1)
			sta VERA_addr_high
			lda	#$FF
			V3 = (*-1)
			sta VERA_addr_low
			lda	#$FF
			V4 = (*-1)
			sta VERA_ctrl
			lda #$FF
			V5 = (*-1)
			sta RAM_BANK
			rts

; jump table for playmusic / playmusic_IRQ to use.
; organized as low bytes together, high bytes together to facilitate
; easy lda table,x indexing - startmusic copies the appropriate routine
; address into the JMP () statement of the players.
;player_table_lo:	.byte <stepmusic, <step_byte, <step_word
;player_table_hi:	.byte >stepmusic, >step_byte, >step_word

	

; ---------------------------------------------------------------------------
; init_player:
;
; Arguments: (none)
; Returns: (none)
; Affects: A,X,Y
;
; ---------------------------------------------------------------------------
;
; Initializes the memory locations used by ZSM player to a stopped playback
; state, with the data & loop pointers set to an "end-of-data" command frame.
;
.segment "CODE"
.proc init_player: near
			stz delay ; "not playing"
			jsr clear_song_pointers
			; initialize the ZSM callbacks to null-handler
			ldx #0
			ldy #>null_handler
next_vector:
			lda #<null_handler
			sta ZSM_VECTOR_TABLE,x
			tya
			sta ZSM_VECTOR_TABLE+1,x
			inx
			inx
			cpx #ZSM_VECTOR_COUNT
			bne next_vector
			lda #1
			sta zsm_scale60hz	; default to handling ZSM playback rate internally
			rts
.endproc

.proc clear_song_pointers: near
			; set song and loop pointers to dummy_tune
			lda #<dummy_tune
			ldx #>dummy_tune
			sta data + SONGPTR::addr
			sta loop_pointer + SONGPTR::addr
			stx data + SONGPTR::addr+1
			stx loop_pointer + SONGPTR::addr+1
			stz data + SONGPTR::bank
			stz loop_pointer + SONGPTR::bank
			; clear music channel mask
			stz zsm_chanmask + CHANMASK::fm
			stz zsm_chanmask + CHANMASK::psg
			stz zsm_chanmask + CHANMASK::psg+1
			stz step+1
			stz fracstep
			lda #1
			sta step
			sta zsm_steps
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
			; store the passed arguments into the ZP data pointer
			; and into a tmp space so it's available later to calculate
			; the loop offset relative to the load point.
			sta data + SONGPTR::bank	; A = ZSM starting bank
			stx data + SONGPTR::addr	; X = ZSM bank window address lo byte
			sty data + SONGPTR::addr+1	; Y = ZSM bank window address hi byte
			sta tmp + SONGPTR::bank
			stx tmp + SONGPTR::addr
			sty tmp + SONGPTR::addr+1
			; bank in the music data
			ldx RAM_BANK
			phx		; save current BANK to restore later
			sta RAM_BANK
			; copy the loop offset from the header data into main memory
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
			ldx #0
:			lda (data)
			sta zsm_chanmask,x
			jsr nextdata
			inx
			cpx	#3
			bcc :-
			
			; get song playback rate and convert to ticks/frame
			lda (data)
			tax				; X is the low byte argument to setmusicspeed
			jsr nextdata
			lda (data)
			tay				; Y is the hi byte argument to setmusicspeed
			jsr nextdata
			jsr setmusicspeed
			
			; move pointer to the first byte of music data
			ldx #5			; currently 5 bytes of reserved (unused) space
:			jsr nextdata
			dex
			bne :-
			
			; check if there is a loop or not
			lda #$FF
			cmp loop_pointer + SONGPTR::bank
			bne calculate_loop
			; if not, set the loop pointer to point at the beginning of the tune
			; and set the done vector = stopmusic
			lda data
			sta loop_pointer + SONGPTR::addr
			lda data+1
			sta loop_pointer + SONGPTR::addr+1
			lda data + SONGPTR::bank
			sta loop_pointer + SONGPTR::bank
			lda #<stopmusic
			sta ZSM_VECTOR_done
			lda #>stopmusic
			sta ZSM_VECTOR_done + 1
			bra done

calculate_loop:
			; If the song loops, add loop offset to load address
			; and set done vector = continuemusic
			clc
			lda tmp + SONGPTR::addr
			adc loop_pointer + SONGPTR::addr
			sta loop_pointer + SONGPTR::addr
			lda loop_pointer + SONGPTR::addr + 1
			cmp #$20
			bcs die	; invalid loop data >= $2000 
			adc tmp + SONGPTR::addr + 1
			cmp #$c0	; see if adjusted location exceeds bank window
			bcc	calculate_bank	; if not, continue by calculating bank of loop point
			sbc #$20			; if so, wrap the offset address, and bank pointer++
			inc loop_pointer + SONGPTR::bank
calculate_bank:
			sta loop_pointer + SONGPTR::addr + 1
			lda tmp + SONGPTR::bank
			adc loop_pointer + SONGPTR::bank
			bcs	die 	; loop bank points past end of HIRAM
			cmp #$FF	; did we end up with loop bank = FF?
			beq die		; if so, then die (FF is an invalid loop bank)
			sta loop_pointer + SONGPTR::bank
			lda #<loopmusic
			sta ZSM_VECTOR_done
			lda #>loopmusic
			sta ZSM_VECTOR_done + 1
			lda #2
			sta loop_count
			
done:		pla
			sta RAM_BANK	; restore the RAM_BANK to what it was before
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
; note that the default=60Hz for files with no Hz specified. We could add some
; code to just write 0001.00 into memory and exit early, but I've chosen to
; just let the routine run in order to optimize a bit for code size.
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
setplayer:
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
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Halts music playback, clears music channel mask.
; TODO: silence the voices used by the tune.
;

.segment "CODE"
.proc stopmusic: near
			;disable ZSM player
			stz	delay
			;silence the voices used by the YM2151
			ldx #0			; .X = voice index 0..7
			lda #$20		; .A = LR|FB|CON register for voice ($20..$27)
@YMloop:	ror zsm_chanmask
			bcc @nextYM
			YM_BUSY_WAIT
			ldy #08
			sty YM_reg		
			nop
			stx	YM_data		; send KeyUP for voice
			YM_BUSY_WAIT
			sta YM_reg		; select LR|FB|CON register for voice
			nop
			stz YM_data		; set to 0 to disable L and R output
@nextYM:	inx
			inc
			cpx	#8
			bne @YMloop
			stz zsm_chanmask
			
			; set up VERA data0 port to sweep through the PSG volumes.
			VERA_SELECT_PSG
			; VERA_SELECT_PSG macro sets step=0, we need step=4
			; (will fix macro later to allow optional step=x argument)
			lda #$31
			sta VERA_addr_bank	; set step=4
			lda #$c2			; point data0 at volume register of PSG channel 0.
			sta VERA_addr_low

			ldy #0
@PSGloop2:	lda zsm_chanmask+1,y
			ldx #8			; 8 voices per byte of chanmask
@PSGloop1:	ror
			bcc @skipPSGvoice
			stz VERA_data0
			bra @nextPSG
@skipPSGvoice:
			bit VERA_data0	; BIT command doesn't modify A, but reads from mem
							; which will cause VERA to step to next voice w/o
@nextPSG:					; changing anything.
			dex
			bne @PSGloop1
			iny
			cpy	#2
			bcc @PSGloop2
			
			; music channels are now silenced.
			
			jmp clear_song_pointers
.endproc

; ---------------------------------------------------------------------------
; stepmusic: 
;
; Arguments: none
; Returns: none
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
			VERA_SELECT_PSG
			bra nextnote

noop:		rts

delayframe:
			and #$7F		; mask off the delay command flag
			bne :+
			jmp (ZSM_VECTOR_done)
:			sta delay
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
			beq CallHandler
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
CallHandler:
			jsr nextdata
			; PCM commands are 3 bytes.
			lda (data)
			sta CMD_BYTE	; stash the command byte to free up the accumulator
			jsr nextdata
			lda (data)
			tax
			jsr nextdata
			lda (data)
			tay
			jsr nextdata
			; pre-load the stack pointer with return address = nextnote, and jmp to callback
			; callback should end with rts which will return to nextnote
			lda #>(nextnote-1)
			pha
			lda #<(nextnote-1)
			pha
			lda #$FF
			CMD_BYTE = (*-1)
			bmi user_callback
			jmp (ZSM_VECTOR_pcmcall)
user_callback:
			jmp (ZSM_VECTOR_user)

.endproc

.segment "CODE"
.proc loopmusic: near
			lda #0
			RECURSION_STOPPER = (*-1)
			bne die
			inc RECURSION_STOPPER
			lda #ZSM_EOF
			cmp (data)
			bne exit
			lda loop_count
			beq go	; loop_count=0 at time of call = infinite loops
			dec
			sta loop_count
			bne go
			; last loop. Change done vector to stopmusic
			lda #<stopmusic
			sta ZSM_VECTOR_done
			lda #>stopmusic
			sta ZSM_VECTOR_done
go:
			lda	loop_pointer + SONGPTR::addr
			sta	data + SONGPTR::addr
			lda	loop_pointer + SONGPTR::addr+1
			sta	data + SONGPTR::addr+1
			lda loop_pointer + SONGPTR::bank
			sta data + SONGPTR::bank
			lda #1
			sta delay
			jsr stepmusic
			; notification callback goes here
exit:		stz RECURSION_STOPPER
			rts
die:		jmp stopmusic	; if the end of tune is reached twice in the
							; same frame, something is wrong. Just end.
.endproc

;............
; step_word :
; ===========================================================================
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
			bne loop
			lda step
			bne loop
			rts	; somehow, this was called with a steps=0 in memory...
loop_hi:
			dec
			sta step+1
loop:
			jsr stepmusic
			; TODO: handle nonzero CC flag returns from stepmusic
			dec step
			bne loop
			lda step+1
			bne loop_hi
done:		rts
.endproc


; stubs for setloopcount and setcallback. (it's getting late)
.proc setloopcount: near
	rts
.endproc

.proc setcallback: near
	rts
.endproc

;............
; step_byte : (no longer used - the step_word is fast enough. Save some space.)
; ===========================================================================
;
; Arguments: (none)
; Returns: should be similar to step_music - not implemented yet here.
; Affects: A,X
; ---------------------------------------------------------------------------
;
; calls stepmusic however many times specified in ZP step.fracstep
; This one is used if steps is < 255. (255 goes to 16bit version because
; chance of carry from frac causing an overflow in a single byte)

;.proc step_byte: near
;			; early exit if called when delay=0 (music not playing)
;			lda delay
;			beq done
;			clc
;			lda zsm_fracsteps
;			adc fracstep
;			sta fracstep
;			lda zsm_steps
;			adc #0
;			sta step
;			beq done
;next_step:
;			jsr	stepmusic
;			dec step
;			bne next_step
;done:		rts
;.endproc
