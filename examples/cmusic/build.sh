#!/bin/bash

echo "Building cmusic example program."
cl65 -t cx16 -I ../../include -L ../../lib -g -Ln cmusic.sym -O -o CMUSIC.PRG cmusic.c zsound.lib util.asm
