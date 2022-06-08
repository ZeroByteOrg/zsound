#!/bin/bash

echo "Building cmusic example program."
cl65 -t cx16 -g -Ln cmusic.sym -O -o CMUSIC.PRG cmusic.c ../../lib/zsound.lib util.asm
