#include "zsmplayer.h"
#include "pcmplayer.h"

#include <stdlib.h>
#include <stdint.h>
#include <conio.h>

typedef enum {
  CLIP_NULL,
  CLIP_ZSM,
  CLIP_ZCM
} mediatype;

typedef struct item_t {
  uint8_t x;     // screen location
  uint8_t y;
  char* name;      // Display Name for the item
  mediatype type;  // music / digi indicator
  uint8_t bank;    // Location of the resource
  uint16_t addr;   //
  char key;        // key to press to trigger it
} item_t;

// because cbm.h's waitvsync() is broken
void __fastcall__ vsync();

void screen_init();
void draw_resources();
void load_sounds();
void init();
void trigger(char i);
void install_irq();
void remove_irq();
void music_callback(uint8_t playing, uint8_t loops_left);

mediatype active[3];

item_t* resource[32];

struct active_resource {
    char music;
    char digi;
} active_resource = { -1, -1 };

uint16_t kernal_irq;

uint8_t wait = 1;
uint8_t numcalls = 0;

static item_t const null_item = { 0,0,"",CLIP_NULL,1,0xa000,'\xff'};

void screen_init() {
  uint8_t i;
  videomode(VIDEOMODE_80x30);
  clrscr();
  cbm_k_bsout(CH_FONT_UPPER);
  gotoxy (0,0);
  cprintf("media jukebox");
  gotoxy (0,1);
  for (i=0 ; i<80 ; i++) cputc('-');

  gotoxy(0,3);
  cprintf("directory: /");
}

void draw_resources() {
  char i;
  gotoxy(1,30);
  cprintf("resources:");
  for (i=0; i<32 ; i++) {
    if (resource[i]->type == CLIP_NULL) continue;
    gotoxy(resource[i]->x, resource[i]->y);
    cprintf("%c: %-20s %02x %04x",
      resource[i]->key,
      resource[i]->name,
      resource[i]->bank,
      resource[i]->addr
    );
  }
}

uint16_t bload(char* filename, uint16_t address) {
  // loads with current HiRAM bank = the one we want.
  // Even if it's not, it's the one we're gonna get! :)
	cbm_k_setnam(filename);
	cbm_k_setlfs(0,8,2);
	return cbm_k_load(0,address);
}

void load_sounds() {
  uint8_t i;
  digitab* zcm;
  uint8_t bank = 1;
  uint16_t addr = 0xa000;
  char key='1';
  const char* const names[4]= {
    "sonic.zsm",
    "sf2intro.zsm",
    "shoryuken.zcm",
    "pacman.zcm"
  };
  const mediatype types[4]= {
    CLIP_ZSM, CLIP_ZSM, CLIP_ZCM, CLIP_ZCM
  };
  for (i=0 ; i<4 ; i++) {
    RAM_BANK = bank;
    resource[i]=malloc(sizeof(item_t));
    resource[i]->x=0;
    resource[i]->y=10+i;
    resource[i]->name=names[i];
    resource[i]->type=types[i];
    resource[i]->bank=RAM_BANK;
    resource[i]->addr=addr;
    resource[i]->key=key;
    addr = bload(resource[i]->name,addr);
    bank = RAM_BANK; // save ending bank for next item's load point
    if (resource[i]->type == CLIP_ZCM) {
      // switch back to this resource's starting bank and set PCM data pointer.
      RAM_BANK = resource[i]->bank;
      zcm = (digitab*)resource[i]->addr;
      zcm->pcmdata = (uint8_t*)zcm + 8;
      zcm->bank = RAM_BANK;
    }
    ++key;
  }
}

void trigger(char i) {
  uint8_t x = (uint8_t)i;
  if (resource[x]->type == CLIP_ZCM) {
    if (active_resource.digi == i) {
      pcm_stop();
      active_resource.digi = -1;
    }
    else {
      pcm_trigger_digi(resource[x]->bank, resource[x]->addr);
      active_resource.digi = i;
    }
  }
  if (resource[i]->type == CLIP_ZSM) {
    zsm_stopmusic();
    if (active_resource.music == i) {
      active_resource.music = -1;
    }
    else {
      active_resource.music = i;
      zsm_startmusic(resource[x]->bank, resource[x]->addr);
    }
  }
}

void init() {
  uint8_t i;
  zsm_init();
  pcm_init();
  zsm_setcallback(&music_callback);
  screen_init();
  for (i=0 ; i<32 ; i++) resource[i] = &null_item;
}

void install_irq() {
  // maybe we won't need to use IRQs?
}

void music_callback(uint8_t playing, uint8_t loops_left) {
  gotoxy (20,0);
  cprintf ("%02x %02x %u",playing,loops_left,++numcalls);
}


int main() {
  char key, run, i;
  init();
  load_sounds();
  draw_resources();
  while(kbhit()) cgetc(); // consume any buffered keystrokes
  run=1;
  wait=1;
  while(run) {
    vsync();
    pcm_play();
    zsm_play();
    if(kbhit()) {
      key=cgetc();
      gotoxy(78,0);
      cprintf("%02x",key);
    }
    else
      key=-1;
    switch (key) {
      case 'q':
      case 'Q':
        run=0;
        break;
      case '\x03': //escape
        pcm_stop();
        zsm_stopmusic();
        active_resource.music = -1;
        active_resource.digi  = -1;
        break;
      default:
        for(i=0 ; i<32 ; i++) {
          if (key==resource[i]->key)
            trigger(i);
        }
    }

  }
  zsm_stopmusic();
  pcm_stop();
  while(!kbhit()) {}
  return 0;
}
