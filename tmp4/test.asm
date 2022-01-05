.ifndef REV
	REV=39
.endif

.include "x16.inc"
.include "../inc/zfxplayer.inc"

.segment "ONCE"

.segment "RODATA"
.include "ding.inc"

.segment "STARTUP"
	lda	#1
	ldx #<YMP_ym_ding
	ldy #>YMP_ym_ding
	jsr patchym
	lda #2
	ldx #<dingsound1
	ldy #>dingsound1
	jsr patchym
	jsr init
	lda IRQVec
	cmp #<irq
	bne install_irq
	lda IRQVec+1
	cmp #>irq
	beq irq_installed
install_irq:
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
irq_installed:
	lda #<ZFX_psgtest
	ldy #>ZFX_psgtest
	ldx #8
;	jsr play
	lda #<ZFX_ding
	ldy #>ZFX_ding
	ldx #2
	jsr play
	rts
	
.segment "CODE"
irq:
	jsr update
	jmp $ffff
	kernal_irq = (*-2)
