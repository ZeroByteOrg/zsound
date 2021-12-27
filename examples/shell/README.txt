This is a simple shell for loading and playing songs in the background in BASIC.

The idea is to extend this with a BASIC wedge that defines BASIC commands which use the
API header as "SYS" calls.

To use this shell, load with ,8,1 so that it loads to $8000.
Then load any ZSM into HiRAM starting on bank 2, $a000
(e.g.: LOAD "KENSTAGE39.ZSM",8,2,$A000 )

ZSM file names specify which emulator revision they are tuned for. ZSM should work
in either rev, but the pitch will be too high or too low if the wrong version of the
emulator is used to play them.

Compiling:
cl65 -t cx16 -C x16asm.cfg --asm-include-dir ../../inc -o SHELL.PRG shell.asm ../../lib/zsound.lib



BASIC shell Idea:
Include a helper program that adds a wedge to add BASIC commands such as MLOAD,MSTART,MSTOP,MSPEED
for instant access to player functionality from BASIC.

Resources to learn how to do the wedge:
https://www.scottjulian.id.au/2020/05/adding-a-basic-wedge/

