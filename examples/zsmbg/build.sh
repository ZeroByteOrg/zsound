#!/bin/bash
echo "Building ZSMBG.PRG"
cl65 -t cx16 -g -Ln zsmbg.sym -L ../../lib --asm-include-dir ../../inc -C x16asm.cfg -o ZSMBG.PRG zsmbg.asm zsound.lib
