#!/bin/bash
[ ! -z "$1" ] && [ "$1" -eq 39 ] && REV=39 || REV=38
echo "Building for REV $REV"
cl65 -t cx16 -g -Ln zsmbg.sym --asm-define REV=$REV -L ../../lib --asm-include-dir ../../inc -C x16asm.cfg -o ZSMBG.PRG zsmbg.asm zsound.lib

