; Zsound source and tools available at https://github.com/ZeroByteOrg/zsound

; Zsound JMP Table API addresses.
; Modify the value of ZSOUND_LOAD_ADDR whenever changes to the ZSOUND.BIN requre
; a different load point in memory.

ZSOUND_LOAD_ADDR = $971F

; ============================================ZSM Music Player API========

; Call this once before making any other calls to ZSM player.
ZSM_INIT            = ZSOUND_LOAD_ADDR

; Music advance routine. Call one of these once per frame
ZSM_PLAY            = ZSM_INIT+3  ; NOT IRQ SAFE: (Clobbers VERA/RAM Bank)
ZSM_PLAYIRQ         = ZSM_PLAY+3  ; Safe to call during IRQ

; Playback control functions:
ZSM_START           = ZSM_PLAYIRQ+3    ; A=bank XY=ADDRESS of ZSM in memory
ZSM_STOP            = ZSM_START+3      ; No args.
ZSM_SETSPEED        = ZSM_STOP+3       ; XY = tick rate (hz) of music
ZSM_SETLOOP         = ZSM_SETSPEED+3   ; A=number of loops to play (0=infinite)
ZSM_FORCELOOP       = ZSM_SETLOOP+3    ; A=number of loops to play (0=infinite)
ZSM_NOLOOP          = ZSM_FORCELOOP+3  ; no args.
ZSM_SETCALLBACK     = ZSM_NOLOOP+3     ; XY=address for callback handler
ZSM_CLEARCALLBACK   = ZSM_SETCALLBACK+3    ; no args
ZSM_GET_MUSIC_SPEED = ZSM_CLEARCALLBACK+3  ; no args (returns in XY)

; ============================================ZCM Digital Audio API========

PCM_INIT          = ZSM_GET_MUSIC_SPEED+3
PCM_TRIGGER_DIGI  = PCM_INIT+3
PCM_PLAY          = PCM_TRIGGER_DIGI+3
PCM_STOP          = PCM_PLAY+3
PCM_SET_VOLUME    = PCM_STOP+3
