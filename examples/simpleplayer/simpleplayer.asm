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
; NOTE: This program COULD just call stepmusic directly in the IRQ handler, as
;		it does nothing in the main loop otherwise. However, as an example of using
;		zsmplayer, it is best to go ahead and demonstrate how to properly call
;		stepmusic from the main loop, as stepmusic clobbers VERA registers and
;		is therefore unsafe to use during an IRQ.

; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"		; Import X16-related symbols
.include "zsound.inc"	; use the zsound library for playback
.include "pcmplayer.inc"

ZSM_address = $A000		; memory address to load song (should be in HIRAM bank window)
ZSM_bank 	= 2			; defines starting bank in HIRAM

PCM_address = $B46A+3
PCM_bank	= 2
USEDIGI		= 1


BAR_VISIBLE_MODE	= $31
BAR_HIDDEN_MODE		= $11
RASTER_LINE_TOP		= 0	; first visible row of pixels?


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

diginame:
.if USEDIGI=0
	.byte "stereo22k16.pcm"
	shoryuken = stereo22k16
.endif
.if USEDIGI=1
	.byte "mono8k8.pcm"
	shoryuken = mono8k8
.endif
diginame_len = (* - diginame)

; PCM parameter table to pass to start_digi
stereo22k16:
	.word	PCM_address
	.byte 	PCM_bank
	.byte	<(135628)	; size of digi (in bytes)
	.byte	>(135628)
	.byte	^(135628)
	.byte	$3f			; stereo 16bit
	.byte	(22000/(25000000>>16))+1	; 22khz sample rate
mono8k8:
	.word	PCM_address
	.byte 	PCM_bank
	.byte	<(12330)	; size of digi (in bytes)
	.byte	>(12330)
	.byte	^(12330)
	.byte	$0f			; mono 8bit
	.byte	(8000/(25000000>>16))+1	; 8khz sample rate

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
			lda	#BAR_VISIBLE_MODE
			sta	semaphore		; set the semaphore so the main loop only plays once per frame
			sta	VERA_dc_video	; display L1 for a visible performance meter bar
			jsr	playmusic
			jsr play_pcm
			lda	#BAR_HIDDEN_MODE
			sta	VERA_dc_video	; hide L1 to end the "rasterbar"
			; do a countdown and play "shoryuken" sound when it reaches zero.
			lda countdown
			ora countdown+1
			beq main			; don't play the digi more than once
			lda countdown
			sec
			sbc #1
			sta countdown
			bne notrigger
			lda countdown+1
			beq trigger
			sbc #0
			sta countdown+1
			bne main
trigger:
			stz countdown
			stz countdown+1
			ldx #<shoryuken
			ldy #>shoryuken
			lda #0
			jsr start_digi
			bra	main
notrigger:
			lda countdown+1
			sbc #0
			sta countdown+1
			bra main
countdown:
			.word	$180
			
; -----------------------------------------------------------------

; SWAPLAYER: Switch to using L0 as the active screen. Clear space at VRAM $4000
;				to use as the L1 tilemap

.segment	"CODE"
			
.proc swaplayer: near
			stz VERA_ctrl			; use data port=0 and DCSEL=0
			VERA_SET_ADDR	$4000,1	; vram $4000, stride 1
			lda #32					; blank space for clearing new screen memory
			ldx #0					; be sure X starts at 0
			ldy	#$40				; clear $40 pages of VRAM
nextbyte:	sta	VERA_data0
			dex
			stz	VERA_data0			; set attr byte to 0 for each screen location
			dex
			bne	nextbyte
			dey
			bne	nextbyte			; next page of VRAM
			
			; now copy L1 config to L0
			ldx #6
nextattr:	lda VERA_L1_config,X
			sta VERA_L0_config,X
			dex
			bpl	nextattr
			; set L1 to use VRAM base $4000
			lda #$20
			sta	VERA_L1_mapbase
			; enable L0, disable sprites
			lda	#BAR_HIDDEN_MODE
			sta	VERA_dc_video
			rts
.endproc

; -----------------------------------------------------------------

; DRAWMETER: Draw the performance bar on the screen to L1
;		

.segment	"CODE"
			
.proc drawmeter: near
			
COL = 76
			stz VERA_ctrl	; use data port 0, DCSEL=0
			VERA_SET_ADDR	($4000 + 2*COL), 9	; stride down on screen
			lda #1
			sta VERA_ctrl	; set data port 1
			VERA_SET_ADDR	($4001 + 2*COL), 9	; data1 for attribute bytes
			jsr	drawcolumn
			stz VERA_ctrl	; use data port 0, DCSEL=0
			VERA_SET_ADDR	($4000 + 2*(COL+1)), 9	; stride down on screen
			lda #1
			sta VERA_ctrl	; set data port 1
			VERA_SET_ADDR	($4001 + 2*(COL+1)), 9	; data1 for attribute bytes
			jsr	drawcolumn
			stz VERA_ctrl	; use data port 0, DCSEL=0
			VERA_SET_ADDR	($4000 + 2*(COL+2)), 9	; stride down on screen
			lda #1
			sta VERA_ctrl	; set data port 1
			VERA_SET_ADDR	($4001 + 2*(COL+2)), 9	; data1 for attribute bytes
			jsr	drawcolumn
			stz VERA_ctrl	; use data port 0, DCSEL=0
			VERA_SET_ADDR	($4000 + 2*(COL+3)), 9	; stride down on screen
			lda #1
			sta VERA_ctrl	; set data port 1
			VERA_SET_ADDR	($4001 + 2*(COL+3)), 9	; data1 for attribute bytes
			jsr	drawcolumn
			stz VERA_ctrl	; put data port selection back to data0 for Kernal's benefit
			rts				; CHROUT fails if it's set to data1 (helloworld uses CHROUT)
			
drawcolumn:
			lda #$66		; checkerboard PETSCII character
			ldx #$A2		; red on orange
			ldy #64			; default screen map = 64 rows of VRAM
nextrow:	sta	VERA_data0
			stx VERA_data1
			dey
			bne	nextrow
			; TODO: draw a nice graduated meter to the side of the bar
			rts
.endproc

; -----------------------------------------------------------------

.segment "STARTUP"

start:
			; initialize the player so that it doesn't do anything
			; during IRQs prior to startmusic: being called.
			jsr init_player
			jsr init_pcm

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

			;  ==== load digi file into memory ====

			; set BANKRAM to the first bank where song should load
			lda	#PCM_bank
			sta	RAM_BANK
			; prepare for call to SETNAM Kernal routine
			lda #diginame_len
			ldx #<diginame
			ldy #>diginame
			jsr SETNAM
			; prepare for call to SETLFS Kernal routine
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; 0 = no command
			jsr	SETLFS
			; load digi sound data
			lda	#0		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#<PCM_address
			ldy #>PCM_address
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
			lda #RASTER_LINE_TOP
			sta VERA_irqline_l
			lda #3				; 3=VSYNC and LINE, IRQ line_hi bit=0
			sta VERA_ien
			
			jsr	swaplayer		; get the screen elements ready for the
			jsr	drawmeter		; perf meter bar
			
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
