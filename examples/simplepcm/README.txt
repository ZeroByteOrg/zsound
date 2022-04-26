PCMPLAYER:

This is a simple program that demonstrates basic usage of the zsound library to
play PCM sounds (digis) on the Commander X16. When run, it loads TEST.ZCM
and plays it once.

ZCM is a new format for Zsound with the digitab parameters included in the
header. Use the raw2zcm tool in zsound/tools to add the header to a raw PCM
file.

------------------------------------------------------------------

Building:
You may use the included build.sh script - specify the emulator REV on the command line.
E.g.:
./build.sh 39

Manual build command:
cl65 -t cx16 --asm-define REV={38|39} -L ../../lib --asm-include-dir ../../inc -o PCMPLAYER.PRG pcmplayer.asm zsound.lib

Running:

Be sure that the PRG is being loaded and run from the same directory where TEST.ZCM files are located.

NOTE: Large digital sound files will exceed the default 512K of memory that the
      emulator uses. Be sure to use -ram 2048 when playing large files!


