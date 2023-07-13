; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"
.include "macros.inc"


.export init_player
.export stepmusic
.export startmusic
.export stopmusic
.export pausemusic
.export unpausemusic
.export playmusic
.export playmusic_IRQ
.export set_music_speed
.export force_loop
.export set_loop
.export disable_loop
.export set_callback
.export clear_callback
.export get_music_speed

; library-internal exports and imports. These probably shouldn't be
; included in the main api .inc files

.import nextdata
.import nextpage
.export data

ZSM_HDR_SIZE	=	16	; does not include PRG header which isn't loaded
ZSM_EOF				=	$80	; (equates to pause cmd with value=0)

.segment "ZEROPAGE"

data:			.tag	SONGPTR
delay:		.res	1
; these next two probably need to use TMP ZP space, not permanent.....
fracstep:	.res	1			; residual steps per frame
step:			.res	2			; integer steps per frame

.segment "BSS"

zsm:						.tag ZSM_HEADER
loop_pointer		:= zsm + ZSM_HEADER::loop
zsm_chanmask		:= zsm + ZSM_HEADER::chanmask
zsm_rate				:= zsm + ZSM_HEADER::tickrate
tmp:						.tag	SONGPTR
loop_count:			.res	1
zsm_fracsteps:	.res	3	; 16.8 fixed point: n steps per 60hz frame
zsm_scale60hz:	.res	1	; flag for whether to use the rate->60hz conversion player

zsm_steps	:= zsm_fracsteps + 1

psg_vol_shadow: .res 16
ym_rl_fb_con_shadow: .res 8
shadow_offset: .res 1
saved_delay: .res 1

; Vector table for various callback pointers
ZSM_VECTOR_TABLE = *
ZSM_VECTOR_pcmcall:		.res 2	; PCM music track handler
ZSM_VECTOR_user:			.res 2	; custom data event handler
ZSM_VECTOR_done:			.res 2	; callback to handle ZSM data EOF
ZSM_VECTOR_notify:		.res 2	; callback when music loops or ends
ZSM_VECTOR_play:			.res 2  ; music playback routine
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
	php
	sei
	lda #<stepmusic
	sta ZSM_VECTOR_play
	lda #>stepmusic
	sta ZSM_VECTOR_play+1
	bra @done
@set_scale:
	php
	sei
	lda #<step_word
	sta ZSM_VECTOR_play
	lda #>step_word
	sta ZSM_VECTOR_play+1
@done:
	plp
.endmacro

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

;..............
; init_player :
;============================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Initializes the memory locations used by ZSM player to a stopped playback
; state, with the data & loop pointers set to an "end-of-data" command frame.
;
; TODO: Support an argument that is used to set zsm_scale60hz flag.
;
.segment "CODE"
.proc init_player: near
			stz delay ; "not playing"
			stz saved_delay
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

;......................
; clear_song_pointers :
;============================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X
; ---------------------------------------------------------------------------
; Utility function to make the player's state reflect a non-tune
; I.e. channel mask is clear, play rate = 60Hz, data pointer points to
; an end-of-data marker, loop pointer also points to the same marker.
;
.segment "CODE"
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
			stz zsm_fracsteps
			stz zsm_rate
			stz zsm_rate+1
			rts
dummy_tune:	.byte ZSM_EOF	; dummy "end of tune" byte - point to this
.endproc

;.................
; check_zsm_file :
;============================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X, active RAM bank
; ---------------------------------------------------------------------------
; Utility function to verify the loaded ZSM header in the state table is
; valid, and if so, update the ZSM file in HiRam with the calculated
; values and set version number to $FF to skip this check on subsequent
; triggers.
;
; expects TMP to be pointing at the load point of the ZSM file.
.segment "CODE"
.proc check_zsm_file: near
			; see whether we support this version
			cmp #ZSM_MINVER
			bcc die
			cmp #ZSM_MAXVER
			beq :+
			bcs die

			; check magic header
:			lda zsm+ZSM_HEADER::magic
			cmp #$7a ; z
			bne die
			lda zsm+ZSM_HEADER::magic+1
			cmp #$6d ; m
			bne die
			; check if the loop offset value >= header size
			; skip calculate_loop and just write $0000:$00 into loop_pointer
			; this seems kludgy and wasteful but is currently necessary
			; TODO - see if there's a more efficient thing to do.
			lda loop_pointer + 2
			bne loop_exists
			lda loop_pointer + 1
			bne loop_exists
			lda loop_pointer
			cmp #$10
			bcs loop_exists
			stz loop_pointer
			bra update_zsm
die:
			sec
			rts
loop_exists:
			jsr calculate_loop
			bcs die
update_zsm:
			; modify the loaded ZSM file header with the calculated loop
			; address for faster triggering upon subsequent starts...

			; first set data PTR to point at the version number.
			lda tmp+SONGPTR::bank
			sta data+SONGPTR::bank
			sta RAM_BANK
			ldy tmp+SONGPTR::addr
			stz data
			lda tmp+SONGPTR::addr+1
			sta data+1
			HIRAM_NEXT
			HIRAM_NEXT ; skip the magic header
			lda #$ff
			sta (data),y
			sta zsm+ZSM_HEADER::version
			HIRAM_NEXT
			lda loop_pointer+SONGPTR::addr
			sta (data),y
			HIRAM_NEXT
			lda loop_pointer+SONGPTR::addr+1
			sta (data),y
			HIRAM_NEXT
			lda loop_pointer+SONGPTR::bank
			sta (data),y
			; Advance data ptr past header
			; This is optimized for code size, not speed...
			HIRAM_SKIP ($10 - 5) ; 5 instances of nextdata out of 16.
			sty data + SONGPTR::addr
			clc
			rts
.endproc


;.............
; startmusic :
;============================================================================
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
; Sets looping behavior:
; If tune has a loop defined, then infinite looping enabled
; Else set loop pointer to start-of-tune and disable looping.
;
; Use loopmusic to override the default behavior
;
.segment "CODE"
.proc startmusic: near
			; ensure music does not attempt to play due to an IRQ
			stz delay
			; store the passed arguments into the ZP data pointer
			; and into a tmp space so it's available later to calculate
			; the loop offset relative to the load point.
			sta data + SONGPTR::bank   ; A = ZSM starting bank
			stz data + SONGPTR::addr   ; page-align the data pointer (X will xfer to Y later)
			sty data + SONGPTR::addr+1 ; Y = ZSM bank window address hi byte
			sta tmp + SONGPTR::bank
			stx tmp + SONGPTR::addr
			sty tmp + SONGPTR::addr+1

			; bank in the music data
			ldy RAM_BANK
			phy		; save current BANK to restore later
			sta RAM_BANK

			txa   ; transfer page offset into Y for use as index.
			tay
			; copy the ZSM header from loaded place in HiRAM into player's memory
			ldx #0
nextbyte:
			lda (data),y
			sta zsm,x
			HIRAM_NEXT
			inx
			cpx #16
			bcc nextbyte
			sty data + SONGPTR::addr ; store page offset in Data pointer.

			; check ZSM version
			; see whether it has been processed before (ver=-1)
			lda zsm+ZSM_HEADER::version
			cmp #$FF
			beq go

			; ZSM is not pre-checked. Check for validity
			jsr check_zsm_file
			bcs die

go:
			ldx zsm_rate
			ldy zsm_rate+1
			jsr set_music_speed

			; check if there is a loop or not
			lda loop_pointer + SONGPTR::bank
			bne do_loop ; loop bank=0 means no loop in the tune.
			; if not, set the loop pointer to point at the beginning of the tune
			; and set the done vector = stopmusic.
			; this is so that if the program wants to force a loop, it will
			; loop the entire song.
			lda data
			sta loop_pointer + SONGPTR::addr
			lda data+1
			sta loop_pointer + SONGPTR::addr+1
			lda data + SONGPTR::bank
			sta loop_pointer + SONGPTR::bank
			jsr disable_loop
			bra success
do_loop:
			lda #0
			jsr force_loop
success:
			lda #1
			sta delay	; start the music
			clc
done:
			pla
			sta RAM_BANK	; restore the RAM_BANK to what it was before
			rts
die:
			stz delay	; ensure the music is not playing
			sec			; return carry flag set to indicate failure
			bra done
.endproc



.proc calculate_loop: near
			; calculate the absolute address of the loop point as follows:
			; addr = (load_point + size) & $1FFF | $a000
			; bank = (load_point + size - $A000) >> 13 + load_bank
			; size is currently stored in loop_pointer, load_point is in tmp.

			; TODO: make this same routine work for PCM offset as well....
			;       .... by using tmp as starting point, and size in .axy
			clc
			lda tmp + SONGPTR::addr ; load_point
			adc loop_pointer + SONGPTR::addr
			sta loop_pointer + SONGPTR::addr
			lda tmp + SONGPTR::addr+1
			adc loop_pointer + SONGPTR::addr+1
			tax		; store a copy in X to preserve 3 MSB for bank calc
			and #$1F
			ora #$a0
			sta loop_pointer + SONGPTR::addr+1
			; loop_pointer now contains addr in SONGPTR::addr fields.
			; Resume calculation for bank....
			; finish the base + size - $a000
			lda loop_pointer + SONGPTR::bank
			adc #0
			bcs die
			sta loop_pointer + SONGPTR::bank
			txa
			sec
			sbc #$a0
			bcs :+
			dec loop_pointer + SONGPTR::bank
			; now perform >>13 by rotating the top 3 bits of middle byte into
			; the bottom 3 bits of the top byte.
:			asl
			rol loop_pointer + SONGPTR::bank
			bcs die
			asl
			rol loop_pointer + SONGPTR::bank
			bcs die
			asl
			rol loop_pointer + SONGPTR::bank
			bcs die
			; do +load_bank portion of second calculation
			lda tmp + SONGPTR::bank
			adc loop_pointer + SONGPTR::bank
			bcs die
			sta loop_pointer + SONGPTR::bank
			clc
			rts
die:
			sec
			rts
.endproc

;..................
; set_music_speed :
; ===========================================================================
; Arguments:
;	X/Y	: Playback speed in Hz. (x=lo, y=hi)
; Returns: (none)
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Converts Hz into ticks/frame, sets the zsm_steps.zsm_fracstep variables
; Chooses the appropriate playback routine as follows:
;   stepmusic: Rate = 60Hz or if using a hi-res timing source
;
;
; note that the default=60Hz for files with no Hz specified. We could add some
; code to just write 0001.00 into memory and exit early, but I've chosen to
; just let the routine run in order to optimize a bit for code size.
;
.proc set_music_speed: near
			; X/Y = tick rate (Hz) - divide by 60 and store to zsm_steps
			; use r0/r1 ZP as temp space, but preserve it
			lda r0L
			pha
			lda r0H
			pha
			lda r1L
			pha

			value := r0
			frac  := r1L
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
			pla
			sta r1L
			pla
			sta r0H
			pla
			sta r0L
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

;............
; stopmusic :
; ===========================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Halts music playback, clears music channel mask, and sets the music pointers
; to dummy_tune by calling clear_song_pointers
;

.segment "CODE"
.proc stopmusic: near
			jsr pausemusic
			; music channels are now silenced.
			stz zsm_chanmask
			stz zsm_chanmask+1
			stz zsm_chanmask+2
			stz saved_delay
			jmp clear_song_pointers
.endproc

;...............
; unpausemusic :
; ===========================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Restores PSG volumes and YM RL_FB_CON from saved state, restores delay
.segment "CODE"
.proc unpausemusic: near
			lda saved_delay
			beq end
			sta delay
			stz saved_delay

			lda zsm_chanmask
			pha
			ldx #0
			lda #$20
YMloop:
			ror zsm_chanmask
			bcc nextYM
			ldy ym_rl_fb_con_shadow,x
			YM_BUSY_WAIT
			sta YM_reg		; select LR|FB|CON register for voice
.repeat 9
			nop
.endrepeat
			sty YM_data		; restore value from before pause
nextYM:		inx
			inc
			cpx	#8
			bne YMloop
			pla
			sta zsm_chanmask


			lda #<VRAM_psg+62	; point data0 at volume register of PSG channel 16
			sta VERA_addr_low

			lda zsm_chanmask+2
			ldx #8			; 8 voices per byte of chanmask
PSGloop2:	rol
			bcc skipPSGvoice2
			ldy psg_vol_shadow+7,x
			sty VERA_data0
			bra nextPSG2
skipPSGvoice2:
			bit VERA_data0	; BIT command doesn't modify A, but reads from mem
							; which will cause VERA to step to next voice w/o
nextPSG2:					; changing anything.
			dex
			bne PSGloop2


			lda zsm_chanmask+1
			ldx #8			; 8 voices per byte of chanmask
PSGloop1:	rol
			bcc skipPSGvoice1
			ldy psg_vol_shadow-1,x
			sty VERA_data0
			bra nextPSG1
skipPSGvoice1:
			bit VERA_data0	; BIT command doesn't modify A, but reads from mem
							; which will cause VERA to step to next voice w/o
nextPSG1:					; changing anything.
			dex
			bne PSGloop1

end:
			rts
.endproc

;.............
; pausemusic :
; ===========================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X,Y
; ---------------------------------------------------------------------------
;
; Pauses music playback by silencing PSG channels while storing their state,
; and silences YM channels by clearing the L/R flags and releasing the note


.segment "CODE"
.proc pausemusic: near
			;disable ZSM player
			lda	delay
			sta saved_delay
			stz delay
			lda zsm_chanmask
			pha ; save the channel mask since it gets destroyed here
			;silence the voices used by the YM2151
			ldx #0			; .X = voice index 0..7
			lda #$20		; .A = LR|FB|CON register for voice ($20..$27)
YMloop:		ror zsm_chanmask
			bcc nextYM
			YM_BUSY_WAIT
			ldy #08
			sty YM_reg
.repeat 9
			nop
.endrepeat
			stx	YM_data		; send KeyUP for voice
			YM_BUSY_WAIT
			sta YM_reg		; select LR|FB|CON register for voice
.repeat 9
			nop
.endrepeat
			stz YM_data		; set to 0 to disable L and R output
nextYM:		inx
			inc
			cpx	#8
			bne YMloop
			pla
			sta zsm_chanmask

			; set up VERA data0 port to sweep through the PSG volumes.
			VERA_SELECT_PSG -3 ; -3 = step amount of -4
			; VERA_SELECT_PSG macro sets step=0, we need step=4
			; (will fix macro later to allow optional step=x argument)
;			lda #$31
;			sta VERA_addr_bank	; set step=4
			lda #<VRAM_psg+62	; point data0 at volume register of PSG channel 16
			sta VERA_addr_low

			; Save the volume shadows in case we're pausing and not stopping
			ldx #16
PSGloop3:
			lda VERA_data0
			sta psg_vol_shadow-1,x
			dex
			bne PSGloop3

			lda #<VRAM_psg+62	; point data0 at volume register of PSG channel 16
			sta VERA_addr_low

			ldy #2
PSGloop2:	lda zsm_chanmask,y
			ldx #8			; 8 voices per byte of chanmask
PSGloop1:	rol
			bcc skipPSGvoice
			stz VERA_data0
			bra nextPSG
skipPSGvoice:
			bit VERA_data0	; BIT command doesn't modify A, but reads from mem
							; which will cause VERA to step to next voice w/o
nextPSG:					; changing anything.
			dex
			bne PSGloop1
			dey
			bne PSGloop2

			rts
.endproc

; ...........
; stepmusic :
; ===========================================================================
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
; is designed to run the music player during an IRQ, use one of the IRQ-safe
; frontend routines provided.
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
			; page-align the data pointer
			ldy data + SONGPTR::addr
			stz data + SONGPTR::addr
			bra nextnote

noop:		rts

delayframe:
			sty data + SONGPTR::addr ; de-page-align the data pointer.
			and #$7F		; mask off the delay command flag
			bne :+
			jmp (ZSM_VECTOR_done)
:			sta delay
			jmp nextdata	; advance data pointer and exit

nextnote:
			lda (data),y			; 5
			bmi delayframe  	;
								; 2
			bit #$40			; 2
			bne YMPCM			;
playPSG:						; 2
			ora #$c0			; faster way to add $C0 to a value known to be < $c0. :)
;			clc					; 2
;			adc #$c0			; 2		; ...to offset it properly into VRAM location
			sta VERA_addr_low	; 4		; VERA data0 now points at selected PSG register
			HIRAM_NEXT		; +X
			lda (data),y			; 5		; get the value for writing into PSG
			sta VERA_data0		; 4		; ... and write it.
			HIRAM_NEXT		; +X
			bra nextnote		; 3

YMPCM:							; 3
			and #$3f			; 2
			beq CallHandler
playYM:							; 2
			tax					; 2		; X now holds number of reg/val pairs to process
nextYM:
			HIRAM_NEXT
			dex
			bmi nextnote	; note: the most YM writes is 63, so this is a safe test
			stz shadow_offset
			phx
			lda (data),y
			tax
			; shadow if appropriate
			cmp #$20
			bcc noYMshadow
			cmp #$28
			bcs noYMshadow
			and #$07
			inc
			sta shadow_offset
noYMshadow:
			HIRAM_NEXT
			lda (data),y
			YM_BUSY_WAIT
			stx YM_reg
.repeat 9
			nop
.endrepeat
			sta YM_data
			ldx shadow_offset
			beq :+
			sta ym_rl_fb_con_shadow-1,x
:
			plx
			bra nextYM		; 3
CallHandler:
			HIRAM_NEXT
			; PCM commands are 3 bytes.
			lda (data),y
			sta CMD_BYTE	; stash the command byte to free up the accumulator
			HIRAM_NEXT
			lda (data),y
			tax
			HIRAM_NEXT
			lda (data),y
			phy
			tay
			; pre-load the stack pointer with return address = callback_done, and jmp
			; to callback which should end with rts.
			lda #>(callback_done-1)
			pha
			lda #<(callback_done-1)
			pha
			lda #$FF
			CMD_BYTE = (*-1)
			bmi user_callback
			jmp (ZSM_VECTOR_pcmcall)
user_callback:
			jmp (ZSM_VECTOR_user)
callback_done:
			ply
			HIRAM_NEXT
			jmp nextnote
.endproc

; ............
; zsmstopper :
; ===========================================================================
; EOF callback routine - this one is called when the music is set not to loop

.segment "CODE"
.proc zsmstopper: near
			jsr stopmusic
			lda #0	; set Z flag=1 , A = remaining loops = 0
			jmp (ZSM_VECTOR_notify)
.endproc

; ...........
; zsmlooper :
; ===========================================================================
; EOF callback routine - it loops the music either an infinite or a set number
; of times. If the final play-through is starting, it changes the EOF callback
; to be zsmstopper instead.

.segment "CODE"
.proc zsmlooper: near
			lda #0
			RECURSION_STOPPER = (*-1)
			bne die
			lda #ZSM_EOF
			cmp (data)
			bne no_callback	; (data) is not pointing at an EOF marker
			; finish playing back the tune after the loop point, up
			; to the next delay command.
			lda	loop_pointer + SONGPTR::addr
			sta	data + SONGPTR::addr
			lda	loop_pointer + SONGPTR::addr+1
			sta	data + SONGPTR::addr+1
			lda loop_pointer + SONGPTR::bank
			sta data + SONGPTR::bank
			lda #1
			sta delay
			inc RECURSION_STOPPER
			jsr stepmusic	; finish playing this frame's music data
			stz RECURSION_STOPPER
			; test for infinite repeat mode
			lda loop_count
			beq do_callback
dec_repeat_count:
			dec
			sta loop_count
			bne continue
last_go:
			; last loop. Change done vector to stopmusic
			php
			sei
			ldx #<zsmstopper
			stx ZSM_VECTOR_done
			ldx #>zsmstopper
			stx ZSM_VECTOR_done+1
			plp
continue:
			inc	;notify using loop_count's pre-decrement value
do_callback:
			ldx #1	; set Z flag = 0
			jmp (ZSM_VECTOR_notify)
no_callback:
			rts
die:	;if the end of tune is reached twice in the same frame, something
 			;is wrong. Just end. Remove "jsr stepmusic"'s return point from the
			;stack so that stopmusic will return to the function that called
			;stepmusic originally when it's finished.
			tsx
			inx
			inx
			txs
			stz RECURSION_STOPPER
			jmp stopmusic

.endproc

;............
; step_word :
; ===========================================================================
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

;================================================================

; simple control routines. (these should probably just be macros?)

; A = number of loops (forces playback into looping mode)
.proc force_loop: near
			sta loop_count
			php
			sei					; just in case the program is using IRQ-driven player
			lda #<zsmlooper
			sta ZSM_VECTOR_done
			lda #>zsmlooper
			sta ZSM_VECTOR_done+1
			plp
			rts
.endproc

; A = number of loops (does not force loopback mode if song has no loop defined)
.proc set_loop: near
			sta loop_count
			rts
.endproc

.proc disable_loop: near
			php
			sei
			ldx #<zsmstopper
			stx ZSM_VECTOR_done
			ldx #>zsmstopper
			stx ZSM_VECTOR_done+1
			plp
			rts
.endproc

.proc set_callback: near
			php
			sei
			stx ZSM_VECTOR_notify
			sty ZSM_VECTOR_notify+1
			plp
			rts
.endproc

.proc clear_callback: near
			php
			sei
			lda #<null_handler
			sta ZSM_VECTOR_notify
			lda #>null_handler
			sta ZSM_VECTOR_notify+1
			plp
			rts
.endproc

.proc get_music_speed: near
			ldx zsm_rate
			ldy zsm_rate+1
			rts
.endproc
