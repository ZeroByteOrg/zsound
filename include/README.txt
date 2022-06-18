C headers for using Zsound.

To use zsound modules in cc65:

Ensure that this directory is in the include search path:
-I.../path/to/zsound/include

Ensure that either the lib directory is in the linker library path
or else give the path to zsound.lib in the build command itself.

If using a Makefile with multiple C project files, it will be easiest
to do the former.

Example:

cl65 -t cx16 -I../zsound/include -l../zsound/lib -O -o MYPROG.PRG main.c foo.c bar.c zsound.lib
