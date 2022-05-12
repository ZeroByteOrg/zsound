; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsm.inc"
.include "macros.inc"
.include "via.inc"


EXPORT_TAGGED "init_player"
EXPORT_TAGGED "stepmusic"
EXPORT_TAGGED "startmusic"
EXPORT_TAGGED "stopmusic"
EXPORT_TAGGED "playmusic"
EXPORT_TAGGED "playmusic_IRQ"
EXPORT_TAGGED "set_music_speed"
EXPORT_TAGGED "force_loop"
EXPORT_TAGGED "set_loop"
EXPORT_TAGGED "disable_loop"
EXPORT_TAGGED "set_callback"
EXPORT_TAGGED "clear_callback"
EXPORT_TAGGED "get_music_speed"

; library-internal exports and imports. These probably shouldn't be
; included in the main api .inc files

IMPORT_TAGGED "nextdata"
IMPORT_TAGGED "ymwrite_viat1"
EXPORT_TAGGED "data"
EXPORT_TAGGED "ym_write"

ZSM_HDR_SIZE	=	16	; does not include PRG header which isn't loaded
ZSM_EOF			=	$80	; (equates to pause cmd with value=0)

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
zsm_rate:		.res	2	; the native tick rate of the ZSM from its header

zsm_steps	:= zsm_fracsteps + 1

; Vector table for various callback pointers
ZSM_VECTOR_TABLE = *
ZSM_VECTOR_pcmcall:		.res 2	; PCM music track handler
ZSM_VECTOR_user:		.res 2	; custom data event handler
ZSM_VECTOR_done:		.res 2	; callback to handle ZSM data EOF
ZSM_VECTOR_notify:		.res 2	; callback when music loops or ends
ZSM_VECTOR_play:		.res 2  ; music playback routine
ZSM_VECTOR_ymwrite:		.res 2	; selectable YM busy-wait-write routine
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
	sei
	lda #<stepmusic
	sta ZSM_VECTOR_play
	lda #>stepmusic
	sta ZSM_VECTOR_play+1
	bra @done
@set_scale:
	sei
	lda #<step_word
	sta ZSM_VECTOR_play
	lda #>step_word
	sta ZSM_VECTOR_play+1
@done:
	cli
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

			; default to handling ZSM playback rate internally
			lda #1
			sta zsm_scale60hz

			; todo: accept timing source via either a define() or as an
			;       argument to init()... for now, just use VIA2_T1

			; make sure IRQ is disabled for VIA2 T1
			lda #$40	; bit mask to clear only the VIA2 IRQ enable
			sta VIA2_ier
			; set VIA2 T1 into one-shot mode / PB7 disabled (T1 bits = 00)
			lda VIA2_acr
			and #$3F	; keep bits 0-5
			sta VIA2_acr

			; initialize VIA2 t1 timer
			lda #144	; see ymwrite_viat1.asm for how this value is chosen.
			sta VIA2_t1cl
			stz VIA2_t1ch

			; set YM_WRITE vector
			lda #<ymwrite_viat1
			sta ZSM_VECTOR_ymwrite
			lda #>ymwrite_viat1
			sta ZSM_VECTOR_ymwrite+1

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
			sta STARTBANK ; self-mod the code below to use the starting bank #.
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
			sta zsm_rate
			tax				; X is the low byte argument to set_music_speed
			jsr nextdata
			lda (data)
			sta zsm_rate+1
			tay				; Y is the hi byte argument to set_music_speed
			jsr nextdata
			jsr set_music_speed

			; move pointer to the first byte of music data
			ldx #5			; currently 5 bytes of reserved (unused) space
:			jsr nextdata
			dex
			bne :-

			; check if there is a loop or not
			lda loop_pointer + SONGPTR::bank
			bne calculate_loop
			lda loop_pointer + SONGPTR::addr+1
			bne calculate_loop
			lda loop_pointer + SONGPTR::addr
			cmp #16
			bcs calculate_loop
			; if not, set the loop pointer to point at the beginning of the tune
			; and set the done vector = stopmusic
			lda data
			sta loop_pointer + SONGPTR::addr
			lda data+1
			sta loop_pointer + SONGPTR::addr+1
			lda data + SONGPTR::bank
			sta loop_pointer + SONGPTR::bank
			jsr disable_loop
			bra done

calculate_loop:
			; If the song loops, convert byte offset into direct memory pointer
			; addr = (load_point + size) & $1FFF | $a000
			; bank = (load_point + size - $A000) >> 13 + load_bank
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
			; Resume calculation for bank by rotating left 3 bits from the
			; value in X into the LSB of loop_ptr+2 after adding the carry
			; from first calc into loop_pointer+2
			lda loop_pointer + SONGPTR::bank
			adc #0
			bcs die
			sta loop_pointer + SONGPTR::bank
			txa
			sec
			sbc #$a0
			asl
			rol loop_pointer + SONGPTR::bank
			bcs die
			asl
			rol loop_pointer + SONGPTR::bank
			bcs die
			asl
			rol loop_pointer + SONGPTR::bank
			bcs die
			; do +load_bank portion of second calculation
			lda #$FF
			STARTBANK := (*-1)
			adc loop_pointer + SONGPTR::bank
			bcs die
			sta loop_pointer + SONGPTR::bank
			lda #0
			jsr force_loop

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
			;disable ZSM player
			stz	delay
			;silence the voices used by the YM2151
			ldx #0			; .X = voice index 0..7
YMloop:		ror zsm_chanmask
			bcc nextYM
			ldy #08
			txa				; send KeyUP for voice
			jsr ym_write
			clc
			adc #$20		; $20+.x = LR|FB|CON register for current voice
			tay
			lda #0
			jsr ym_write

nextYM:		inx
			cpx	#8
			bne YMloop
			stz zsm_chanmask

			; set up VERA data0 port to sweep through the PSG volumes.
			VERA_SELECT_PSG -3 ; -3 = step amount of -4
			; VERA_SELECT_PSG macro sets step=0, we need step=4
			; (will fix macro later to allow optional step=x argument)
;			lda #$31
;			sta VERA_addr_bank	; set step=4
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

			; music channels are now silenced.

			jmp clear_song_pointers
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
			ora #$c0			; faster way to add $C0 to a value known to be < $c0. :)
;			clc					; 2
;			adc #$c0			; 2		; ...to offset it properly into VRAM location
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
			lda (data)
			jsr ym_write
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
			sei
			ldx #<zsmstopper
			stx ZSM_VECTOR_done
			ldx #>zsmstopper
			stx ZSM_VECTOR_done+1
			cli
continue:
			inc	;notify using loop_count's pre-decrement value
do_callback:
			ldx #1	; set Z flag = 0
			jmp (ZSM_VECTOR_notify)
no_callback:
			rts
die:		jmp stopmusic	; if the end of tune is reached twice in the
							; same frame, something is wrong. Just end.
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
			sei					; just in case the program is using IRQ-driven player
			lda #<zsmlooper
			sta ZSM_VECTOR_done
			lda #>zsmlooper
			sta ZSM_VECTOR_done+1
			cli
			rts
.endproc

; A = number of loops (does not force loopback mode if song has no loop defined)
.proc set_loop: near
			sta loop_count
			rts
.endproc

.proc disable_loop: near
			sei
			ldx #<zsmstopper
			stx ZSM_VECTOR_done
			ldx #>zsmstopper
			stx ZSM_VECTOR_done+1
			cli
			rts
.endproc

.proc set_callback: near
			sei
			stx ZSM_VECTOR_notify
			sty ZSM_VECTOR_notify+1
			cli
			rts
.endproc

.proc clear_callback: near
			sei
			lda #<null_handler
			sta ZSM_VECTOR_notify
			lda #>null_handler
			sta ZSM_VECTOR_notify
			cli
			rts
.endproc

.proc get_music_speed: near
			ldx zsm_rate
			ldy zsm_rate+1
			rts
.endproc

.proc ym_write: near
			jmp (ZSM_VECTOR_ymwrite)
.endproc
