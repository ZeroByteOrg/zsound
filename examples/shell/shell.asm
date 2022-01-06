.ifndef REV
	REV=39
.endif

.include "x16.inc"
.include "zsmplayer.inc"

; Background player for BASIC environment. Load a ZSM into HIRAM at
; address $A000, bank 2 and then use the API to start/stop playback.
; e.g.:
; READY
; LOAD "KENSTAGE.ZSM",8,2,$A000
;
; SYS $8000
;

; "API" - these are simple interfaces for SYS calls to use to interact
; with the player. Functions are start, stop, speed, and uninstall.
; start = initialize the player and install IRQ (if needed)
; stop  = stops music and restores the original IRQ vector at $314
; set_music_speed = direct call to zsm player API. Expects X/Y to hold
; 					the playback speed in Hz.
;
; use STARTUP segment to ensure these are found at the memory address where
; the program is loaded.
.segment "STARTUP"
	jmp	start			; SYS call for starting the music
	jmp stop			; SYS call to uninstall the player IRQ
	jmp set_music_speed	; SYS call for adjusting playback speed

; set_music_speed and stopmusic are directly part of the player library and
; do not use any front-end code from this shell


;-----------------------------------------------------------------------
; start: Starts the music in bank $A000, bank 2. Installs IRQ if not already installed.
;
; Arguments: None
; Returns: None
; Affects: A,X,Y
;-----------------------------------------------------------------------
.segment "CODE"
.proc start: near
	jsr	check_irq		; If IRQ is already installed, skip player init and
	beq	start_song		; just start the music playback.
	; initialize player and install IRQ handler to vector at $314
	jsr init_player
	sei					; disable Interrupts while modifying the IRQ vector
	; save current IRQ vector to jmp into it after playmusic call
	lda IRQVec
	sta kernal_irq
	lda IRQVec+1
	sta kernal_irq+1
	; install player's IRQ handler
	lda #<irq_handler
	sta IRQVec
	lda #>irq_handler
	sta IRQVec+1
	cli					; resume Interrupt processing
start_song:
	; pass play address of $A000, bank 2
	lda	#2		; A = bank ID
	ldx #0		; X = <address
	ldy #$A0	; Y = >address
	jsr startmusic
	rts			; return to BASIC
.endproc

;-----------------------------------------------------------------------
; stop: Stops the music and removes IRQ if not already removed.
;
; Arguments: None
; Returns: None
; Affects: A,X,Y (stopmusic affects X and Y)
;-----------------------------------------------------------------------
.proc stop: near
	jsr	stopmusic
	jsr check_irq
	bne	done			; if IRQ not installed, skip IRQ removal
	sei
	lda	kernal_irq
	sta	IRQVec
	lda kernal_irq+1
	sta IRQVec+1
	cli
done:
	rts
.endproc

;-----------------------------------------------------------------------
; irq_handler: Calls the once-per-frame playback routine.
;
; Arguments: None
; Returns: None
; Affects: None
;
; Note that this is not a .proc because kernal_irq symbol needs to be
; exposed to the (un)install/check routines.
;-----------------------------------------------------------------------
irq_handler:
	jsr	playmusic_IRQ	; call the IRQ-safe playback function
kernal_irq := (* + 1)
	jmp	$FFFF			; Installation overwrites this with previous vector
						; value from ($0314)


;-----------------------------------------------------------------------
; check_irq: Checks whether irq_handler is currently installed at $314
;
; Arguments: None
; Returns: Zero flag: 0=installed 1=not installed
; Affects: A
;-----------------------------------------------------------------------
.segment "CODE"
.proc check_irq: near
	lda	#<irq_handler
	cmp IRQVec
	bne	done
	lda #>irq_handler
	cmp IRQVec+1
done:
	rts
.endproc
