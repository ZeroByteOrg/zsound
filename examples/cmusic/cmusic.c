#include "../../include/zsmplayer.h"

#include <cbm.h>
#include <conio.h>



#define RAMBANK (*(uint8_t*)0)

const char* const fn = "bgm.zsm";

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
	init_player();
	bload(fn,2,0xa000);
	startmusic(2,0xa000);
	while(1) {
		vsync();
		playmusic();
	}
	return 0;
}
