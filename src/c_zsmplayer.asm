
; system routines for interacting with the C stack.
.import popa
.import pushax

.import init_player
.import stepmusic
.import set_music_speed
.import playmusic
.import playmusic_IRQ
.import startmusic
.import stopmusic
.import force_loop
.import set_loop
.import disable_loop
.import set_callback
.import clear_callback
.import get_music_speed

.code

.export _init_player
_init_player := init_player

.export _stepmusic
_stepmusic:
  jsr stepmusic
  lda #0
  rol     ; stepmusic should return 0=playing / 1=stopped in C flag.
  eor #1  ; convert this to C true/false style 0=false, else true.
  ldx #0  ; promote to int. (even if return type is char)
  rts

.export _playmusic
_playmusic := playmusic

.export _playmusic_IRQ
_playmusic_IRQ := playmusic_IRQ

.export _startmusic
_startmusic:
  phx
  tax
  jsr popa
  ply
  jsr startmusic
  lda #0
  rol
  eor #1
  ldx #0
  rts

.export _stopmusic
_stopmusic := stopmusic

.export _set_music_speed
_set_music_speed:
  phx
  tax
  ply
  jmp set_music_speed

.export _force_loop
_force_loop := force_loop

.export _set_loop
_set_loop := set_loop

.export _disable_loop
_disable_loop := disable_loop

.export _set_callback
_set_callback:
  ; set wrapper with the desired callback address
  ; and tell zsound to call the wrapper.
  sta callback_vector
  stx callback_vector+1
  ldx #<do_callback
  ldy #>do_callback
  jmp set_callback
do_callback:
  ; prepare the arguments for the callback routine:
  ; passed in: Z: zero=stopped, neq=playing and A: loops_left
  ; void callback(uint8_t playing, uint8_t loops_left )
  beq :+
  ldx #1 ; playing = 1 in C logic (playing)
  bra push_it
: ldx #0 ; playing = 0 in C logic (stopped)
push_it:
  jsr pushax
  jmp $FFFF  ; RTS from the callback will return to zsmplayer.
  callback_vector := (*-2)

.export _clear_callback
_clear_callback := clear_callback

.export _get_music_speed
_get_music_speed:
  jsr get_music_speed
  txa
  phy
  plx
  rts
