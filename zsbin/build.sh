#!/bin/bash
echo "building ZSOUND.PRG..."
#cl65 -t cx16 -g -Ln zsound.sym -m zsound.map -u __LOADADDR__ -C robots.cfg -o ZSOUND.PRG zsound.asm ../lib/zsound.lib
cl65 -t cx16 -g -Ln zsound.sym -m zsound.map -C robots.cfg -o ZSOUND.PRG zsound.asm ../lib/zsound.lib

echo "building PCMTEST.PRG..."
cl65 -t cx16 -o PCMTEST.PRG pcmtest.asm

echo "building ZSMTEST.PRG..."
cl65 -t cx16 -o ZSMTEST.PRG zsmtest.asm
