
; system routines for interacting with the C stack.
.import popa
.import pushax

.import init_pcm
.import start_digi
.import play_pcm
.import stop_pcm
.import set_pcm_volume

.code

.export _pcm_init
_pcm_init := init_pcm

.export _pcm_play
_pcm_play := play_pcm

.export stop_pcm
_pcm_stop := stop_pcm

.export _pcm_trigger_digi:
_pcm_trigger_digi:
  phx
  tax
  jsr popa
  ply
  jsr start_digi  ; returns Carry Set on error / clear on success
  lda #0
  rol
  eor #1  ; reverse logic to C-style 0=false, else true.
  ldx #0
  rts

.export _pcm_set_volume
_pcm_set_volume := set_pcm_volume
