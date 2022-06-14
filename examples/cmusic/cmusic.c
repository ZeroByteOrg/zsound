// To get "zsmplayer.h" into the include path with cl65:
// cl65 -I ../../include ....

#include "zsmplayer.h"
#include <cbm.h>
#include <conio.h>

#define RAMBANK (*(uint8_t*)0)

// because cbm.h's waitvsync() is broken
void __fastcall__ vsync();

void bload(const char* filename, const uint8_t bank, const uint16_t address)
{
	uint8_t b = RAMBANK;
	RAMBANK = bank;
	cbm_k_setnam(filename);
	cbm_k_setlfs(0,8,2);
	cbm_k_load(0,address);
	RAMBANK = b;
}

int main() {
	char playing;

	bload("bgm.zsm",2,0xa000);
	zsm_init();
	playing = zsm_startmusic(2,0xa000);
	if (!playing) cprintf("Error starting song.\n\r");
	while(playing) {
		vsync();
		zsm_play();
	}
	return 0;
}
