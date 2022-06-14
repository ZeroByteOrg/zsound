
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

.export _zsm_init
_zsm_init := init_player

.export _zsm_step
_zsm_step:
  jsr stepmusic
  lda #0
  rol     ; stepmusic should return 0=playing / 1=stopped in C flag.
  eor #1  ; convert this to C true/false style 0=false, else true.
  ldx #0  ; promote to int. (even if return type is char)
  rts

.export _zsm_play
_zsm_play := playmusic

.export _zsm_playIRQ
_zsm_playIRQ := playmusic_IRQ

.export _zsm_startmusic
_zsm_startmusic:
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

.export _zsm_stopmusic
_zsm_stopmusic := stopmusic

.export _zsm_setspeed
_zsm_setspeed:
  phx
  tax
  ply
  jmp set_music_speed

.export _zsm_forceloop
_zsm_forceloop := force_loop

.export _zsm_loop
_zsm_loop := set_loop

.export _zsm_noloop
_zsm_noloop := disable_loop

.export _zsm_setcallback
_zsm_setcallback:
  ; set wrapper with the desired callback address
  ; and tell zsound to call the wrapper.
  sei
  sta callback_vector
  stx callback_vector+1
  cli
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

.export _zsm_clearcallback
_zsm_clearcallback := clear_callback

.export _zsm_getspeed_normal
_zsm_getspeed_normal:
  jsr get_music_speed
  txa
  phy
  plx
  rts
