#!/bin/bash
echo "Building SIMPLEPCM.PRG"
cl65 -t cx16 -g -Ln simplepcm.sym --asm-include-dir ../../inc -L ../../lib -o SIMPLEPCM.PRG simplepcm.asm zsound.lib
