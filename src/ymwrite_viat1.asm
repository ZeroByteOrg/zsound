.include "x16.inc"
.include "macros.inc"
.include "via.inc"

EXPORT_TAGGED "ymwrite_viat1"

; Subroutine that uses VIA2 T1 as a stand-in for the YM busy flag.
; it expects the clock to be set at ~144 CPU cycles in the latch, and
; checks the timer IRQ bit in the VIA's IRQ status flags.

; Note - this routine expects that T1 in VIA2 is already configured
; properly for one-shot mode / phi2-timed, and was triggered immediately
; after the previous write to the YM2151.

; init_player will handle this configuration, and should definitely be
; called prior to calling this routine, or else the system will be in
; an infinite loop waiting for a timer that isn't running.

; Inputs:	.y = YM2151 register to be written
;			.a = value to write into register

; YM busy shuold last approximately 143.03 CPU clocks at 8MHz
; currently ~11 clocks from exit of the busy loop until actual
; write into YM_data - so clock can be safely set to 132.03 -> 133.
.proc ymwrite_viat1: near
:	bit	VIA2_ifr	; 4
	bvc	:-			; 2	; wait for VIA2 t1 timer to expire
	sty	YM_reg		; 4
	nop				; 2
	nop
	nop
	sta	YM_data		; 4
	stz VIA2_t1ch	; 4 ; re-trigger the timer
	rts
.endproc
