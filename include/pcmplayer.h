#ifndef _pcmplayer_h_
#define _pcmplayer_h_

#include <stdint.h>

typedef struct digitab_s {
  uint8_t* pcmdata;
  uint8_t  bank;
  uint8_t  size[3]; //little-endian 24bit value = n bytes of PCM data
  uint8_t  VERA_cfg;
  uint8_t  VERA_rate;
} digitab;

extern void __fastcall__ pcm_init();
extern uint8_t __fastcall__ pcm_trigger_digi(uint8_t bank, uint16_t addr);
extern void __fastcall__ pcm_play();
extern void __fastcall__ pcm_stop();
extern void __fastcall__ pcm_set_volume(uint8_t volume);

#endif
