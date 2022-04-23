#!/bin/bash
[ ! -z "$1" ] && [ "$1" -eq 38 ] && REV=38 || REV=39
echo "Building for REV $REV"
cl65 -t cx16 -g -Ln simplepcm.sym --asm-define REV=$REV --asm-include-dir ../../inc -L ../../lib -o SIMPLEPCM.PRG simplepcm.asm zsound.lib
