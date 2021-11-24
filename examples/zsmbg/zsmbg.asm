.ifndef REV
	REV=38
.endif

.include "x16.inc"
.include "zsound.inc"

; Background player for BASIC environment. This program will load into Golden RAM
; and await SYS calls to perform its functionality. Ideally, there will be an
; accompanying .BAS file which installs a BASIC wedge that adds BASIC commands
; to signal the player.

.segment "STARTUP"
install:
	jmp	init_background_player
start:
	jmp	startmusic
stop:
	jmp stopmusic
setpeed:
	jmp setmusicspeed
uninstall:
	jsr	stopmusic
	sei
	lda	kernal_irq
	sta	IRQVec
	lda kernal_irq+1
	sta IRQVec+1
	stz irq_set
	cli
	rts

kernal_irq:
	.word	$0		; storage for the original IRQ vector.

irq_set:
	.byte	$0		; use this to avoid setting the IRQ handler multiple times
					; and losing the original vector's destination.

irq_handler:
	jsr	playmusic_IRQ
	jmp	(kernal_irq)

init_background_player:
	jsr init_player
	lda irq_set		; check whether the IRQ has been configured already
	bne	done		; if so, don't do it a second time.
	inc
	sta irq_set		; mark IRQ as installed.
	sei
	lda IRQVec
	sta kernal_irq
	lda IRQVec+1
	sta kernal_irq+1
	lda #<irq_handler
	sta IRQVec
	lda #>irq_handler
	sta IRQVec+1
	cli
done:
	rts	; exit to BASIC
