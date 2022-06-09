#ifndef _pcmplayer_h_
#define _pcmplayer_h_

#include <stdint.h>

typedef struct digitab_s {
  uint16_t* addr;
  uint8_t   bank;

} digitab;

extern void __fastcall__ pcm_init();
extern void __fastcall__ pcm_trigger_digi(uint8_t bank, uint16_t addr);
extern void __fastcall__ pcm_play();
extern void __fastcall__ pcm_stop();
extern void __fastcall__ pcm_set_volume(uint8_t volume);
