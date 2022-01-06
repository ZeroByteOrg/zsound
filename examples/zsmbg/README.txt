This is a simple shell for loading and playing songs in the background in BASIC.

To use this shell, load it with ,8,1 so that it loads to $8000.
LOAD "ZSMBG.PRG",8,1

Then load any ZSM into HiRAM starting on bank 2, $a000
(e.g.: LOAD "KENSTAGE39.ZSM",8,2,$A000 )

The included ZSM file names specify which emulator revision they are tuned for. ZSM will work
in either rev, but the pitch will be too high or too low if the ZSM version and emulator version
are not the same.

Once the player and music file are loaded, you start/stop the music with SYS calls:

Start music:
SYS $8000

Stop music:
SYS $8003

Set music speed: (uses .XY as the new playback speed, which are set by BASIC from $30D and $30E)
POKE $30D,80 : POKE $30E,0 : REM SET SPEED = 80HZ
SYS $8006

As this is a tad cumbersome, there is a simple BASIC program included: SETSPEED.PRG
You can load this program after loading the player and music file, and run it after starting the music.

-------------------------------------------------------------------

Compiling:
Use the included build.sh script:
./build.sh 39

Or on systems without bash, an example compilation is:
cl65 -t cx16 -C x16asm.cfg --asm-include-dir ../../inc -o ZSMBG.PRG zsmbg.asm ../../lib/zsound.lib


