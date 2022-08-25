#!/bin/bash
echo "building ZSOUND.PRG..."
cl65 -t cx16 -g -Ln zs.sym -m zs.map -u __LOADADDR__ -C robots.cfg -o ZSOUND.PRG zs.asm ../lib/zsound.lib
