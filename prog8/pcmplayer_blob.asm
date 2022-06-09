.import init_pcm
.import start_digi
.import play_pcm
.import stop_pcm
.import set_pcm_volume

.segment "STARTUP"
pcm_init:
  jmp init_pcm

pcm_trigger_digi:
  jmp start_digi

pcm_play:
  jmp play_pcm

pcm_stop:
  jmp stop_pcm

pcm_set_volume:
  jmp set_pcm_volume
