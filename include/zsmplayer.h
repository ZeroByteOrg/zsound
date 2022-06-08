#ifndef _zsmplayer_h_
#define _zsmplayer_h_

#include <stdint.h>

typedef void(*zsm_callback)(uint8_t, uint8_t);

extern void __fastcall__ init_player();
extern void __fastcall__ playmusic();
extern void __fastcall__ playmusic_IRQ();

extern uint8_t __fastcall__ stepmusic();
extern uint8_t __fastcall__ startmusic(uint8_t bank, uint16_t addr);
extern void __fastcall__ stopmusic();
extern void __fastcall__ set_music_speed(uint16_t hz);
extern void __fastcall__ force_loop(uint8_t count);
extern void __fastcall__ set_loop(uint8_t count);
extern void __fastcall__ disable_loop();
extern void __fastcall__ set_callback(zsm_callback);
extern void __fastcall__ clear_callback();
extern uint16_t __fastcall__ get_music_speed();

#endif
