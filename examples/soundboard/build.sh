#!/bin/bash
echo "Building SOUNDBOARD.PRG"
cl65 -t cx16 -g -Ln soundboard.sym -O -I../../include -L../../lib -o SOUNDBOARD.PRG main.c util.asm zsound.lib
