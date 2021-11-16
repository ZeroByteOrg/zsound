ZSound is a work-in-progress towards a fully-featured sound API for the
Commander X16 retro computer system.

The goal is to create well-defined music and sound data formats useful
for playing music and SFX in games or other programs on the X16. Without
such a standard, there is a large amount of duplicate effort being required
of the community. This is because each project wishing to use audio must
spend effort to create its own bespoke playback routines, data formats, and
data creation/import tool chains.

Thus, zSound should supply data format specifications, a reference playback
engine and API, and a basic tool set for creation of audio assets, or the
ability to import audio from other existing ecosystems such as VGM, IMF, etc.


Thus far, we have created ZSM format which is a pre-defined data stream
of raw instructions for the YM2151, VERA PSG, and VERA PCM sound devices.

See ZSM.txt for a description of that data format.
(PCM is the current work-in-progress)

Current features:

zsound.lib:
	- ZSM playback:
		- support for FM and PSG music
		- PCM support is not yet available, but is planned
	
	- Raw API:
		- load FM patch into a voice of the YM2151
	
Tool chain:
	- VGM to ZSM conversion tool
	- DMP instrument conversion tool

------------------------------------------------------------------

Soon-to-come features:

zsound.lib:
	- Raw API:
		- stopmusic should mute all music-related voices and clear them from the channel mask
		- basic interface for direct communication with the sound HW
		  using commands like play_note(voice,freq,volume) etc

	- SFX API:
		- initial implementation
		- streamlined SFX format (ZFX) capable of being played on any free
		  voice channel of the target sound chip

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
	- utility for generating SFX from plaintext script files
	  (custom definition language).
	- utility to rip FM instruments from VGM tunes
	- add .OPM file suppot for instrument import tool

================================================================================
Using zsound.lib:

	include zsound.inc in any assembly file(s) which reference zsound
	call init_player (no arguments) at least once before using the library
	call startmusic to set the desired ZSM song
	call playmusic once per frame (60Hz)
	call stopmusic to halt playback (does not mute or release any voices)
	build project with zsound.lib included as a component - e.g.:
		cl65 -t cx16 -o MYPROG.PRG myprog.asm zsound.lib
		(see documentation for further information regarding lib and inc paths)
