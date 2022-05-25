This is a simple demonstration of how to use the ZFX module.

It plays a sound on the VERA PSG, and then a sound on the YM2151

In this example, the FX are embedded directly in the program
and not loaded from any external file.

-------------------------------------------------------------------

Compiling:
Use the included build.sh script:
./build.sh

Or on systems without bash, an example compilation is:
cl65 -t cx16 -o ZFXDEMO.PRG zfxdemo.asm ../../lib/zsound.lib
