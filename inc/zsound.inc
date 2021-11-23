.ifndef REV
	REV = 39
.endif

	; import symbols as symbol + REV  (e.g. helloworld38)
	; alias them with the non-rev symbol (i.e. helloworld -> helloworld38)


	.import	.ident(.sprintf("%s%d","helloworld",REV))
	helloworld	:=	.ident(.sprintf("%s%d","helloworld",REV))

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
; state, with the data pointer pointing at a dummy "end-of-data" command frame.
;
	.import	.ident(.sprintf("%s%d","init_player",REV))
	init_player	:=	.ident(.sprintf("%s%d","init_player",REV))

; ---------------------------------------------------------------------------
; stepmusic: 
;
; Arguments: (none)
; Returns: Carry flag: (currently broken) 0=playing, 1=stopped or looped
;
; Affects: A,X,Y, VERA CTRL and data port 0 address registers
; ---------------------------------------------------------------------------
;
; Advances the music by one tick.
; Music must be initialized by startmusic before this will have any effect.
;
; Call as many times per frame as required by the ZSM's playback rate.
; (usually 60Hz - once per frame)
; THIS ROUTINE IS NOT SAFE to call directly during IRQ, as it clobbers VERA
; registers w/o fixing them (for speed reasons). If your program
; is designed to run the music player during an IRQ, use one of the IRQ-safe 
; wrapper functions that save and restore VERA before calling this
; core routine.
;
	.import	.ident(.sprintf("%s%d","stepmusic",REV))
	stepmusic	:=	.ident(.sprintf("%s%d","stepmusic",REV))

; ---------------------------------------------------------------------------
; playmusic:
;
; Arguments: (none)
; Returns: (currently broken) should return 0=playing, 1=stopped or loop encountered
; Affects: See stepmusic
; ---------------------------------------------------------------------------
;
; Speed-correct frontend for stepmusic. Calls stepmusic however many times
; is determined from the ZSM tick rate header information. This function does not
; smooth time - i.e. it calls stepmusic N times in a row. For more accurate timing
; you should generate your own timing source and call stepmusic directly.
;
; NOTE: playmusic calls stepmusic, WHICH IS NOT IRQ SAFE! Use playmusic_IRQ instead
;       if you wish to run the music update during the IRQ handler, or else your
;		handler should save and restore the VERA ctrl register and the address
;		registers for the data0 data port.
;
	.import	.ident(.sprintf("%s%d","playmusic",REV))
	playmusic	:=	.ident(.sprintf("%s%d","playmusic",REV))

; ---------------------------------------------------------------------------
; playmusic_IRQ:
;
; IRQ-safe version of playmusic, which restores VERA registers.
; It does NOT save or restore the CPU registers. Your IRQ handler should do
; those functions.
; 
	.import	.ident(.sprintf("%s%d","playmusic_IRQ",REV))
	playmusic_IRQ	:=	.ident(.sprintf("%s%d","playmusic_IRQ",REV))

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
; Copies channel mask into main memory, and calls setmusicspeed
; Music will begin playing on the following frame
;
	.import	.ident(.sprintf("%s%d","startmusic",REV))
	startmusic	:=	.ident(.sprintf("%s%d","startmusic",REV))

; ---------------------------------------------------------------------------
; stopmusic: 
;
; Arguments: (none)
; Returns: (none)
; Affects: (none)
; ---------------------------------------------------------------------------
;
; Halts music playback, clears music channel mask.
; TODO: silence the voices used by the tune.
;
	.import	.ident(.sprintf("%s%d","stopmusic",REV))
	stopmusic	:=	.ident(.sprintf("%s%d","stopmusic",REV))

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
	.import	.ident(.sprintf("%s%d","setmusicspeed",REV))
	setmusicspeed	:=	.ident(.sprintf("%s%d","setmusicspeed",REV))

; ---------------------------------------------------------------------------

; to-do: move patchym into a separate .inc file for API

	.import	.ident(.sprintf("%s%d","patchym",REV))
	patchym	:=	.ident(.sprintf("%s%d","patchym",REV))




