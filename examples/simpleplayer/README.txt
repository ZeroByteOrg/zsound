SIMPLEPLAYER:

This is a simple program that demonstrates basic usage of the zsound library to
play ZSM music on the Commander X16. When run, it loads either BGM38.ZSM or
BGM39.ZSM depending on which emulator revision it was built for.

Building:

x16emu (r38):
cl65 -t cx16 --asm-define REV=38 -L ../../lib --asm-include-dir ../..inc -o SIMPLEPLAYER.PRG simpleplayer.asm zsound39.lib

x16emu (r39), Box16, or real HW:
cl65 -t cx16 -L ../../lib --asm-include-dir ../..inc -o SIMPLEPLAYER.PRG simpleplayer.asm zsound39.lib


Running:

Be sure that the PRG is being loaded and run from the same directory where the ZSM files are located.


