#ifndef _zsmplayer_h_
#define _zsmplayer_h_

#include <stdint.h>

typedef void(*zsm_callback)(uint8_t, uint8_t);

extern void __fastcall__ zsm_init();
extern void __fastcall__ zsm_play();
extern void __fastcall__ zsm_playIRQ();

extern uint8_t __fastcall__ zsm_step();
extern uint8_t __fastcall__ zsm_startmusic(uint8_t bank, uint16_t addr);
extern void __fastcall__ zsm_stopmusic();
extern void __fastcall__ zsm_setspeed(uint16_t hz);
extern void __fastcall__ zsm_forceloop(uint8_t count);
extern void __fastcall__ zsm_loop(uint8_t count);
extern void __fastcall__ zsm_noloop();
extern void __fastcall__ zsm_setcallback(zsm_callback);
extern void __fastcall__ zsm_clearcallback();
extern uint16_t __fastcall__ zsm_getspeed_normal();

#endif
