ZSound is a work-in-progress towards a fully-featured sound API for the
Commander X16 retro computer system.

The goal is to create well-defined music and sound data formats useful
for playing music and SFX in games or other programs on the X16. Without
such standards, there is a large amount of duplicate effort being required
of the community. This is because each project wishing to use audio must
spend effort to create its own bespoke playback routines, data formats, and
data creation/import tool chains.

Thus, zSound should supply data format specifications, a reference playback
engine and API, and a basic tool set for creation of audio assets, or the
ability to import audio from other existing ecosystems such as VGM, IMF, etc.

Thus far, we have created ZSM, ZFX, ZCM, and YMP formats:
ZSM is a pre-generated stream of raw instructions for the YM2151, VERA PSG,
    and VERA PCM sound devicesintended to be played back as music.
ZFX is a pair of sound effects data formats for FM and PSG sounds.
ZCM is a stream of PCM sample data with a header describing the size/parameters.
YMP is a block of the raw bytes of an FM instrument patch

See the documentation for full descriptions of the data formats.

-------------------------------------------------------------------------------

Current features:

zsound.lib:
	- ZSM player:
		- support for FM and PSG music
		- Variable playback speed
		- Loop/repeat monitoring via callback
		- Loop/repeat behavior control routines
		- Custom synchronization event channel support (untested)
		- PCM track support is not yet available, but is planned

	- PCM player:
		- Playback of any PCM quality, all the way up to 48Khz 16bit stereo
		- Simple one-shot trigger API - pass pointer to digi parameter table
		- more functionality is planned...
			-granular control API to set individual PCM parameters
			-callbacks to allow creating behaviors such as looping
			 digis or chaining clips together

	- ZFX player:
		- Initial implementation. Allows simple one-shot triggering of sfx playback

	- Raw API:
		- load FM patch into a voice of the YM2151

Tools:
	- vgm2zsm:
		- PHP-based script that converts VGM music data into ZSM data
		- Resamples time to 60Hz by default for lower playback overhead
		- Multiple supported chip types.
		- FM chips supported:
		  * YM2151
		  	- Pitch correction for VGMs with 4MHz-clocked YM2151
		  * YM2612 (e.g. Sega Megadrive/Genesis)
		  	- LFO translated to nearest YM2151 equivalent behavior
		  	- 3CH mode not supported
		  	- DAC not supported (work in progress)
		  * YM2203
		  	- dual-chip VGM support
		  	- integrated AY-3 PSG voices supported (see below)
		  	- clock rate support is incomplete - has cmdline switch
		  	  for trying one of two rate multipliers on FM voices
		- PSG chips supported:
		  * AY-3-8910 / YM2149
		  	- noise output frequency is currently hard-wired to a constant value
		  	- noise mode overrides tone mode. concurrent tone+noise not
		  	  currently supported in order to conserve PSG channel consumption
		  	  (plan to do this dynamically in future re-write of import tool)
		  	- ENV generator not supported
		  * SN76489
		  	- Noise channel only supports white noise, as VERA PSG does not
		  	  have the ability to do periodic noise.
		- Planned/most-wanted chips for future support:
		  * Sega PCM (arcade) for supporting one digi channel from Deflemask arcade mode tunes
		  * YM2612 DAC
		  * NES APU
		  * POKEY
		  * OPL2 (AdLib/SoundBlaster)

	- dmp2x16: instrument conversion tool
		- Converts Deflemask FM instruments into various output formats:
		  * BASIC code to POKE the values into any chosen voice
		  * C header format (bytes are in YMP order)
		  * Assembly .byte format
		  * Binary (YMP) format
		- Requires PHP interpreter

  - raw2zcm: Places ZCM header onto a raw PCM file

  - zsmdecode
    - simple tool that decodes a ZSM file into human-readable text

------------------------------------------------------------------

Soon-to-come features:

zsound.lib:
	- Raw API:
		- stopmusic should mute all music-related voices and clear them from the channel mask
		- basic interface for direct communication with the sound HW
		  using commands like play_note(voice,freq,volume) etc

Planned features:

zsound.lib:
	ZSM player:
		- PCM track support

	SFX API:
		- dynamic SFX channel selection
		- non-destructive music channel pre-emption

	Raw API:
		- basic interface for direct communication with the sound HW
		  using commands like play_note(voice,freq,volume) etc
  		- volume controls (per channel and music level)
		- ZSM/ZFX file loading routines

Tool chain:
	- utility for generating ZFX sound effects
	- utility to rip FM instruments from VGM tunes
	- add .OPM file suppot for instrument import tool

================================================================================
Using zsm player:

	include the .inc file in any assembly file(s) which reference zsm player routines
	(e.g. .include "zsmplayer.inc")
	call init_player (no arguments) at least once before using the module
	call startmusic to set the desired ZSM song
	call set_callback if you would like to be notified when the song loops/ends
	call playmusic once per frame (60Hz) (use playmusic_IRQ if calling during IRQ)
	call stopmusic to halt and silence playback
	build project with zsound.lib included as a component - e.g.:
		cl65 -t cx16 -o MYPROG.PRG myprog.asm zsound.lib
		(see documentation for further information regarding lib and inc paths)
