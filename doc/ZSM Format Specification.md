## ZSOUND Data formats

Extension|Format
---|---
ZSM | Zsound Streaming Music
ZFX | Zsound sFX
YMP | YM2151 Patch data
ZCM | PCM audio

# ZSM format specification

ZSM is a format standard specifying both a data stream format and a file header and layout format designed to be loaded into and played back from HIRAM. The stream format is suitable for playback from any region of memory, but the playback routine assumes HIRAM is in use. Any ZSM stream of more than 8k in size would cause the library’s playback routine to handle it improperly, assuming a bank wrap at the nearest 8k boundary in memory. This may be changed in the future to allow loading/playing ZSM from main memory if desired.

## ZSM file composition

Offset|Length|Field
--|--|--
0x00|2|PRG HEADER
0x02|16|ZSM HEADER
0x12|?|ZSM STREAM
?|?|(optional) PCM HEADER
?|?|(optional) PCM DATA

### PRG Header

These 2 bytes are technically not part of the ZSM file format. Until recently, the Kernal assumes that all files begin with a 2-byte load-to address "header" and skips them when loading files into memory. If your program loads a file byte by byte instead of using the Kernal LOAD routines, then it should skip these 2 bytes. Kernal LOAD now supports headerless mode, but the BASIC UI does not expose this mode very well as of R40. Once it is possible to perform such loads equally easily from the "command line" or from within programs, the PRG header bytes are likely to be removed.

### ZSM Header

The ZSM header is 16 bytes long.

- All multi-byte values are little endian unless specified otherwise
- All offsets are relative to the beginning of the ZSM header

Offset|Length|Field|Description
---|---|---|---
0x00|3|Loop Point|Offset to the starting point of song loop. 0 = no looping.
0x03|3|PCM offset|Offset to the beginning of the PCM index table (if present). 0 = no PCM data or header is present.
0x06|1|FM channel mask|Bit 0-7 are set if the corresponding OPM channel is used by the music.
0x07|2|PSG channel mask|Bits 0-15 are set if the corresponding PSG channel is used by the music.
0x09|2|Tick Rate|The rate (in Hz) that each song tick should be advanced.
0x0B|5|RESERVED|Reserved for future use

### ZSM music data Stream format

Byte 0|Byte 1 - n|Byte n+1|Byte n+2 - ...|...|End of stream
---|---|---|---|---|---
CMD|DATA (variable)|CMD|DATA|...|0x80

#### CMD (command) byte values

Value|CMD|Argument bytes|Description
--|---|--|--
0x00-0x3F|PSG write|1|CMD is the VERA PSG register offset to be written. The following 1 byte is the value to be written into the PSG register. The register is to be considered as an offset from 0x1F9C0 which is the first PSG register. Thus CMD 0-3 = voice 0, 4-7 = voice 1, etc.
0x40|Extension command|3|The following 3 bytes are used as callback hooks for event streams in the music such as triggering PCM instruments, synchronization messages, etc. Extension commands are 3 bytes in length and of the general format EXTCMD + A + B. EXTCMD byte values 0x00..0x7F are reserved for official ZSM extensions. 0x80..0xFF are user-defined extensions, and will be passed to a user callback along with the A and B bytes. Even if the user-defined EXTCMD does not require 2 bytes, each command MUST be 3 bytes in length. Player implementations may safely ignore user-defined EXT commands.
0x41-0x7F|FM write|N|Bits 0-5 of CMD specify a value N. Following this command are N reg/val pairs to be written into the YM2151. Each reg/val pair is two bytes. Write val into OPM register reg. Do this for N pairs of bytes.
0x80|End of data|0|If the song has a specified a loop, the player should continue processing the data at that point immediately (without delaying to the next frame). ZSM streams MUST terminate with this byte.
0x81-0xFF|Delay|0|The seven least significant bits give the number of ticks to delay before proceeding. Delay of zero would be CMD of 0x80 which is the "end of data" token. All other values are the number of ticks to delay.

**Notes**

1. While this document describes the commands in ranges for ease-of-reading and clarity, it is recommended that you consider the CMD as being a bit-masked value: If MSB is set, the CMD is a delay/EOF command. If clear, then if bit 6 is set, process as N YM writes or an EXTCMD (if N=0). If bits 6 and 7 are clear, the CMD is simply a PSG register offset followed by a single PSG value.

### PCM Header and Sample Data (in ZSM context)

While these are included in the ZSM specification, it is intended that the PCM portion of the engine and format suite should be capable of working with pure-PCM data files and playback schemes. As such, the header and data in a ZSM file will be determined by the PCM engine’s needs.

It is expected that any offset references found in the PCM header should be relative to the location of the start of the PCM header and not to the start of the ZSM file itself. See the section on PCM data format for details.

### PCM Header and Data format

(not yet supported or specified)

While the size and contents of the PCM header have not been determined at this time, this document includes our thinking in order to generate feedback from the community.

At the very least, the PCM header should contain a list of offsets pointing at the starting points of the various digi clips contained in the PCM data block, as well as format specifiers giving the 8/16 bit, mono/stereo, and sample rate parameters for each digi. This is speculative as it will take some experimentation with various sources of music containing PCM digis in order to determine what is a useful, reasonable, and efficient way to store and reference them. Note that this document refers to such clips as "digis" in order to disambiguate the usage of the word "sample" which also means 1 discrete PCM sample value. As this second definition is of great significance while discussing PCM playback, "sample" shall refer to one discrete sample, and not be used to mean "an audio sample" e.g. "drum sample." These will be referred as "digis." (short for digital audio clip)

It is likely that any offset values stored here will be of the same format as those in the ZSM header, but be considered as offsets relative to the start of the PCM header. This is to make it possible to have PCM-only sound files that can be loaded outside the context of ZSM. These would be played using API commands to directly control. There would be no need to encode this information into a ZSM music stream. Thus the ZSM stream’s PCM commands are likely to mimic the various API calls.

### PCM Data

This is likely to be a simple concatenation of all PCM sample data used in the tune. It will be left up to the header to specify any sample rates, bit depths, and stereo/mono formatting requirements. In fact, many sound systems (such as Amiga MOD, Sega arcade PCM, and many others) have varying playback rates, so it is likely for the PCM commands in the ZSM data stream to feature such capabilities.
