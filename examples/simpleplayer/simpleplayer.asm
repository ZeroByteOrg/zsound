; Simple bare-bones ZSM music player program, with "performance" rasterbar.
;
; Plays either BGM38.ZSM or BGM39.ZSM depending on which rev it is built for.
; If the tune ends, the player stops at the end, but does not return to BASIC.
; If the tune loops, then the player loops indefinitely.
;
; The program builds to version r39/real HW specifications by default.
; Use --asm-define REV=38 to build for r38 emulator specifications.
;
; BGM38.ZSM is pitch-corrected for YM2151 clocked at 4MHz (r38)
; BGM39.ZSM uses reference pitch values for ~3.5MHz YM2151 clock.
; 			(Yamaha recommended value, used in r39+, Box16, and real X16 HW)
;
; NOTE: This program COULD just call playmusic directly in the IRQ handler, as
;		it does nothing in the main loop otherwise. However, as an example of using
;		zsmplayer, it is best to go ahead and demonstrate how to properly call
;		playmusic from the main loop, as playmusic clobbers VERA registers and
;		is therefore unsafe to use during an IRQ.

; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"		; Import X16-related symbols
.include "zsound.inc"	; use the zsound library for playback

ZSM_address = $A000		; memory address to load song (should be in HIRAM bank window)
ZSM_bank 	= 2			; defines starting bank in HIRAM


.ifndef X16_VERSION
	.error	"x16.inc must not have been included successfully."
.endif

.segment "ONCE"	; current ca65 linker config file for cx16 requires this segment
				; to set the LOAD address as $801 and to emit the PRG header
				; and BASIC stub for SYS $80D

.segment "RODATA"

; Choose the ZSM whith the proper pitch tuning for the revision being built.
; x16.inc sets X16_VERSION to 39 by default in this project's copy...
.if X16_VERSION = 38
	filename:	.byte "bgm38.zsm"
.else
	filename:	.byte "bgm39.zsm"
.endif

filename_len = (* - filename)

; -----------------------------------------------------------------

.segment "BSS"

kernal_irq:	.res	2	; storage for the previous IRQ RAM vector
semaphore:	.res	1	; byte that signals between the IRQ and main loop

; -----------------------------------------------------------------

.segment "CODE"

irqhandler:
			lda	VERA_isr
			and	#%00000010		; check the LINE IRQ flag
			bne	line_irq
			jmp	(kernal_irq)

line_irq:
			lda	#2
			sta VERA_isr		; ACK to VERA Line IRQ
			stz	semaphore
			ply
			plx
			pla
			rti

; -----------------------------------------------------------------

.segment	"CODE"
			
main:		wai					; save power :)
			lda	semaphore
			bne	main			; raster IRQ sets semaphore = 0. Wait for that to happen
			lda	#1
			sta	semaphore		; set the semaphore so the main loop only plays once per frame
			sta	VERA_dc_video	; hide layers and sprites to create "rasterbar"
			jsr	playmusic
			lda	#$21
			sta	VERA_dc_video	; enable L1 and VGA output to end the "rasterbar"
			bra	main			; program does nothing in the main loop.

; -----------------------------------------------------------------

.segment "STARTUP"

start:
			; initialize the player so that it doesn't do anything
			; during IRQs prior to startmusic: being called.
			jsr init_player

			;  ==== load zsm file into memory ====

			; set BANKRAM to the first bank where song should load
			lda	#ZSM_bank
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

			;  ==== Install IRQ handler to process music once per frame ====
			;
			;  Assumes default configuration:
			;  VERA VSYNC IRQ is the only active IRQ source
			;

			; save the current IRQ vector so player can call it when done
			sei
			lda IRQVec
			sta kernal_irq
			lda IRQVec+1
			sta kernal_irq+1
			; install player as the IRQ handler
			lda #<irqhandler
			sta IRQVec
			lda #>irqhandler
			sta IRQVec+1
			; enable LINE IRQs on line 50
			lda #82
			sta VERA_irqline_l
			lda #3				; 3=VSYNC and LINE, IRQ line_hi bit=0
			sta VERA_ien
			cli

			; Call startmusic:
			; X,Y = address of the ZSM data
			; A   = HIRAM bank of the ZSM data
			lda #ZSM_bank
			ldx #<ZSM_address
			ldy #>ZSM_address
			jsr startmusic

			lda #1
			sta	semaphore		; intilize the IRQ semaphore
			jsr	helloworld
			jmp main
