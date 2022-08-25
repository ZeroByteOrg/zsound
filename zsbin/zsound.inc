zsm_init := $9721
zsm_play := zsm_init+3
zsm_playIRQ := zsm_play+3
zsm_start := zsm_playIRQ+3
zsm_stop := zsm_start+3
zsm_setspeed := zsm_stop+3
zsm_setloop := zsm_setspeed+3
zsm_forceloop := zsm_setloop+3
zsm_noloop := zsm_forceloop+3
zsm_setcallback := zsm_noloop+3
zsm_clearcallback := zsm_setcallback+3
zsm_get_music_speed := zsm_clearcallback+3

pcm_init := zsm_get_music_speed+3
pcm_trigger_digi := pcm_init+3
pcm_play := pcm_trigger_digi+3
pcm_stop := pcm_play+3
pcm_set_volume := pcm_stop+3
