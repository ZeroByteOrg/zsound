Hi, Dave. This is what you should need in order to get the advanced
assets working in X16-Robots.

Be sure to see the notes at the end on loading the ZSM / ZCM resources!

This zip includes the following:
ZSOUND.PRG:
  - the actual player binary that the game needs to load into memory.
  - it contains the 2-byte header so loading it with ,8,1 from BASIC
    will put it in memory where it needs to go. ($9721)
  - PETSCII Robots should be able to utilize this for playing music and
    PCM sounds in-game.

ZSMTEST.PRG:
  - The Zsound reference ZSM player example, but built to use ZSOUND.PRG
    instead of having the library built into the player .prg itself.
  - To use it:
    - LOAD "ZSOUND.PRG",8,1
    - LOAD "ZSMTEST.PRG"
    - RUN

  - It will play the file BGM.ZSM (which is the PETSCII Robots title song)
  - Rename any ZSM file as BGM.ZSM to play other songs with this test prg.

PCMTEST.PRG:
  - The Zsound reference PCM player example, built to use ZSOUND.PRG
  - Works the same way as ZSMTEST.PRG but plays the file TEST.ZCM
  - ZCM is a kind of "wav file" for X16.

Included for use in the PETSCII Robots source:

zsound.inc:
  - This is a quick include to define symbols for the jump table API
    at the beginning of ZSOUND.PRG
  - It uses ca65 syntax, but it should be easy enough to convert to
    ACME syntax if it's any different.

Included for reference:

pcmplayer.inc / zsmplayer.inc:
  - These are the standard ca65 includes for using zsound.lib to build
    zsound support directly into assembly programs. They aren't needed
    to use ZSOUND.PRG but since this is where I documented the API calls,
    I decided to include them in this ZIP archive.

    The names are a bit different in these two, but I think it should be
    obvious which ones are which. I'm going to be deprecating the names
    used in these files in favor of the names used in zsound.inc

pcmtest.asm / zsmtest.asm:
  - This is the ca65 source for the two reference players, as adapted to
    using ZSOUND.PRG - refer to them for details in how to use the basic
    player functionality. Most of the code has to do with drawing the CPU
    raster bar.

Notes:
Loading sound resources:

ZSM and ZCM files do not contain the usual 2-byte "prg" header, and MUST
be loaded using the new "headerless load" mode in Kernal versions >= 39.

It's simple to do - perform loads as normal with calls to SETNAM,SETLFS,LOAD
but in order to do headerless mode, the SA (.Y) parameter for the SETLFS
call should be 2.

See the "STARTUP" code in the test players for an example.

I've included the songs and wav files you sent to me so that you can
get them working quickly. I opted for 16khz / 8bit sample quality, having
auditioned various qualities and determined these to be a good tradeoff
in size and still sounding pretty close to the original 48K-16bit versions.

I can easily make them in other quality levels if you would like.
PCM's quality also has a bearing on the CPU load required to move the data
into the FIFO, so the lower quality files also save on CPU resources.


If you have any questions, feel free to reach out to me at any time.
You have my cell phone number as well, and if I happen to not be available
in Discord when you have a question, feel free to txt me if need be.

-- Barry
