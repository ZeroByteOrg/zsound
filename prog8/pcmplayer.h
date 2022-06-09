#ifndef _pcmplayer_h_
#define _pcmplayer_h_

#include <stdint.h>

// incomplete - do not use....
typedef struct digitab_s {
  uint16_t* addr;
  uint8_t   bank;
} digitab;

extern void __fastcall__ pcm_init();
extern void __fastcall__ pcm_trigger_digi(uint8_t bank, uint16_t addr);
extern void __fastcall__ pcm_play();
extern void __fastcall__ pcm_stop();
extern void __fastcall__ pcm_set_volume(uint8_t volume);

// call pcm_play once per frame outside of IRQ (or at least I feel it should be - it takes a long time to run, depending on the bit rate of the PCM stream)
//
// See pcmplayer.inc  for details about the functions' behavior.
#endif
