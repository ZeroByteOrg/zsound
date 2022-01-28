.ifndef REV
	REV=39
.endif

.include "x16.inc"
.include "../../inc/zfxplayer.inc"

.segment "ONCE"

.segment "RODATA"
.include "ding.inc"

.segment "STARTUP"
	jsr init		; init ZFX player
	
	; check whether IRQ handler is already installed
	lda IRQVec
	cmp #<irq
	bne install_irq
	lda IRQVec+1
	cmp #>irq
	beq go
install_irq:
	; install IRQ handler to call ZFX player update once per frame
	sei
	lda IRQVec
	sta kernal_irq
	lda IRQVec+1
	sta kernal_irq+1
	lda #<irq
	sta IRQVec
	lda #>irq
	sta IRQVec+1
	cli

go:
	;Play PSG sound on voice 8 (PSG voice 0)
	lda #<ZFX_psgtest
	ldy #>ZFX_psgtest
	ldx #8
	jsr play
	
	; wait 64 frames for the sound to finish playing
	ldx #$40
delay:
	wai
	dex
	bne delay

	;Play a ding on voicee 2 (FM)

	; load YMP_ding patch into first FM voice (ZFX voice 0)
	lda	#0
	ldx #<YMP_ding
	ldy #>YMP_ding
	jsr patchym
	; Play ding sound on voice 0
	lda #<ZFX_ding
	ldy #>ZFX_ding
	ldx #0
	jsr play
	rts
	
.segment "CODE"
irq:
	jsr update
	jmp $ffff	; install_irq overwrites this with the correct address
	kernal_irq = (*-2)
