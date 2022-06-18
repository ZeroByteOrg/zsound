Work-in-progress audible asset player/soundboard program.

Build using ./build.sh

Windows/Mac users may just issue the cl65 command from build.sh if bash is not
installed on their systems.

********
* NOTE * You will need to run the emulator with -ram 2048 enabled
********

Currently, the asset names are hardwired into the program. Future updates
will turn this into a fully-fledged browser with directory changing support,
etc.

Pressing the number will start/stop that audio asset from playing.
Pressing ESC (STOP) will halt all audible playback.
Pressing Q will exit the program.

Note that cc65 C programs cannot be RUN a second time (or at least I don't
know how to fix what the C constructors/destructors break) so once quit,
you will need to reload the program if you want to run it again.
