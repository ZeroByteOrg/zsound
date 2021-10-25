; x16.inc by SlithyMatt - slightly modified for multi-revision support
.include "x16.inc"
.include "zsound.inc"

.segment "CODE"

databank = 2

irq:
			jsr	playmusic
			jmp	(kernal_irq)

kernal_irq:	.word	$ffff

.segment "STARTUP"
			
start:
			jsr helloworld
			jsr init_player

			sei
			
			;  ==== load zsm file into memory ====

			; set BANKRAM to the first bank where song should load
			lda	#databank
			sta	RAM_BANK
			lda #filename_len
			ldx #<filename
			ldy #>filename
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load song to $A000
			lda	#0		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #$a0
			jsr LOAD
			
			
			; save the current IRQ vector so player can call it when done
			lda IRQVec
			sta kernal_irq
			lda IRQVec+1
			sta kernal_irq+1
			; install player as the IRQ handler
			lda #<irq
			sta IRQVec
			lda #>irq
			sta IRQVec+1


			cli
			lda #databank
			ldx #0
			ldy #$a0
			jsr startmusic
forever:	bra forever

.segment	"RODATA"
.if X16_VERSION = 39
filename:	.byte "bgm.zsm2"
.else
filename:	.byte "bgm38.zsm2"
.endif
filename_end:
filename_len = (* - filename)
