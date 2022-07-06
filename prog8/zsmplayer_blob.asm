.import init_player
.import stepmusic
.import playmusic
.import playmusic_IRQ
.import startmusic
.import stopmusic
.import set_music_speed
.import force_loop
.import set_loop
.import disable_loop
.import set_callback
.import clear_callback
.import get_music_speed

.segment "STARTUP"
zsm_init:
  jmp init_player
zsm_play:
  jmp playmusic
zsm_playIRQ:
  jmp playmusic_IRQ
zsm_start:
  jmp startmusic
zsm_stop:
  jmp stopmusic
zsm_setspeed:
  jmp set_music_speed
zsm_setloop:
  jmp set_loop
zsm_forceloop:
  jmp force_loop
zsm_noloop:
  jmp disable_loop
zsm_setcallback:
  jmp set_callback
zsm_clearcallback:
  jmp clear_callback
zsm_get_music_speed:
  jmp get_music_speed
