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

ZSM playback:
	- support for FM and PSG music
	- PCM support is not yet available, but is the next feature planned

Planned features:

SFX API:
	- streamlined SFX format (ZFX?) capable of being played on any free
	  voice channel of the target sound chip

	- dynamic channel selection / music pre-emption

Raw API:
	- basic interface to allow direct communication with the sound HW
	  using commands like play_note(voice,freq,volume) etc

Using zsound.lib:

	include zsound.inc in any assembly file(s) which reference zsound
	call init_player (no arguments) at least once before using the library
	call startmusic to set the desired ZSM song
	call playmusic once per frame (60Hz)
	call stopmusic to halt playback (does not mute or release any voices)


