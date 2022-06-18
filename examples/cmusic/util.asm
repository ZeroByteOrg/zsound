.include "x16.inc"

.export _vsync
.export _snooze

.bss
lastjiffy:  .res 1

.code
.proc _vsync: near
  jsr RDTIM
  sta lastjiffy
keep_waiting:
  jsr RDTIM
  cmp lastjiffy
  beq keep_waiting
  rts
.endproc

.code
.proc _snooze: near
  phx
  ldx #$30
: nop
  dex
  bne :-
  plx
  rts
.endproc
