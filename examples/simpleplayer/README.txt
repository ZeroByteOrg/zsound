SIMPLEPLAYER:

This is a simple program that demonstrates basic usage of the zsound library to
play ZSM music on the Commander X16. When run, it loads BGM.ZSM and plays the
music with a CPU load indicator at the top-right corner of the screen.

The code:
The code demonstrates basic use of the zsmplayer module of Zsound.
One thing of note is that the program uses a raster-line IRQ as the timing
source to drive playmusic. This is done in order to place the CPU load indicator
at the top of the screen, and not due to any particular requirements of
the zsmplayer module. As Zsound does not recommend calling playmusic from
within an IRQ, the raster IRQ handler simply clears a semaphore value
which the main loop sees and then proceeds to call playmusic during normal
execution.

Building:
You may use the included build.sh script
./build.sh

Manual build command:
cl65 -t cx16 -L ../../lib --asm-include-dir ../../inc -o SIMPLEPLAYER.PRG simpleplayer.asm zsound.lib

Running:

Be sure that the PRG is being loaded and run from the same directory
where the ZSM file is located.
