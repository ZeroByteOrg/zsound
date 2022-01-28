.ifndef REV
	REV = 39
.endif

.ifndef __PCMPLAYER_INC__
__PCMPLAYER_INC__ = 1

;Macros to import the zsound version-tagged symbols as non-tagged symbols.
;Zsound contains symbols for r38 and r39 compatible builds, and exports them
;as symbol38 or symbol39. The macros "undo" that tag.
; eg: playmusic39: --> playmusic:

.ifndef __ZSOUND_TAG_SYMBOLS__
__ZSOUND_TAG_SYMBOLS__ = 1
.macro IMPORT_TAGGED symbol
	.import	.ident(.sprintf("%s%d",symbol,REV))
	.ident(.sprintf("%s",symbol))	:=	.ident(.sprintf("%s%d",symbol,REV))
.endmacro

.macro IMPORTZP_TAGGED symbol
	.importzp .ident(.sprintf("%s%d",symbol,REV))
	.ident(.sprintf("%s",symbol))	:=	.ident(.sprintf("%s%d",symbol,REV))
.endmacro
.endif

; DIGITAB is the structure used by the engine to store the parameters of
; a digi (digital audio) clip. This format may change as this library is
; fleshed out, so be sure to check for changes.
.struct DIGITAB
	addr		.addr
	bank		.byte
	size		.word	; 24bit digi size (in bytes)
	sizehi		.byte	; ...
	cfg			.byte	; VERA_audio_ctrl value
	rate		.byte	; VERA_audio_rate
.endstruct


IMPORT_TAGGED "init_pcm"
IMPORT_TAGGED "start_digi"
IMPORT_TAGGED "play_pcm"
IMPORT_TAGGED "stop_pcm"

.endif

;...........
; init_pcm :
; ===========================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A
; ---------------------------------------------------------------------------
; Call this before using any of the other routines.
;
; Initializes the memory locations used by PCMM player to a stopped playback
; state, sets VERA playback rate to 0 (disabled) and clears the PCM FIFO


;...........
; play_pcm :
; ===========================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A,X,Y
; ---------------------------------------------------------------------------
; Call this once per frame to ensure that the PCM samples are being fed
; into VERA's PCM FIFO. While this routine is technically IRQ-safe, it is
; recommended that it not be executed during the VSYNC IRQ, as higher
; quality sample streams can consume up to ~30k CPU cycles to process.
;
; Consistent, jitter-free low-latency playback can be assured using a line IRQ
;
; If using ZSM player module, it is recommended that this call happen after
; the ZSM player's update function (playmusic) so that any PCM events will
; be processed on the same frame.

;...........
; stop_pcm :
; ===========================================================================
; Arguments: (none)
; Returns: (none)
; Affects: A
; ---------------------------------------------------------------------------
; Disables PCM playback in VERA_audio_rate register, clears the FIFO, and
; sets the PCM player module's status to clear.

;.............
; start_digi :
; ===========================================================================
; Arguments: Pointer to digi parameter table in memory.
;	.A  = RAM bank
; 	.XY = Memory address
; Returns: none
; Affects: none
; ---------------------------------------------------------------------------
; Documentation and terminology in Zsound uses the term "digi" to refer to
; a digital audio clip, as the term "sample" also refers to an individual
; PCM sample. Thus "digi" disambiguates between the two.
;
; start_digi expects a pointer to a digi parameter table in memory (low or
; high memory are both acceptable locations. If in Hi memory, the table
; must exist entirely within the same bank.
;
; The table's contents are the same as the DIGITAB struct at the
; beginning of this inc file.
;
; note that start_digi is a one-shot call to trigger a digi. There is
; currently no infrastructure to make callbacks at the end of a digi's
; playback, or any granular controls to modify a playback in progress
; such as volume changes, etc.
;
; In the future, more granular controls are planned, but the exact
; nature and functionality are as yet to be determined.

