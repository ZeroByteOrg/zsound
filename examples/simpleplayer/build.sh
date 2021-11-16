#!/bin/bash
[ ! -z "$1" ] && [ "$1" -eq 39 ] && REV=39 || REV=38
echo "Building for REV $REV"
cl65 -t cx16 --asm-define REV=$REV -L ../../lib --asm-include-dir ../../inc -o SIMPLEPLAYER.PRG simpleplayer.asm zsound.lib
