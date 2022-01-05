.ifndef __ZFX_INC__
__ZFX_INC__ = 1

.define ZFX_EOF	$80
.define FM_CHANNELS		8
.define PSG_CHANNELS	16

.struct FMPTR
	addr_lo		.byte	FM_CHANNELS
	addr_hi		.byte	FM_CHANNELS
	bank		.byte	FM_CHANNELS
.endstruct

.struct PSGPTR
	addr_lo		.byte	PSG_CHANNELS
	addr_hi		.byte	PSG_CHANNELS
	bank		.byte	PSG_CHANNELS
.endstruct

.struct FMSTATE
	active		.byte	1
	delay		.byte	FM_CHANNELS
	data		.tag	FMPTR
	loop		.tag	FMPTR
	mask		.byte	1
.endstruct

.struct PSGSTATE
	active		.byte	1
	delay		.byte	PSG_CHANNELS
	data		.tag	PSGPTR
	loop		.tag	PSGPTR
	mask		.byte	2
.endstruct

.struct PLAYERSTATE
	active		.byte	1	; # of channels with active FX.
	fm			.tag	FMSTATE
	psg			.tag	PSGSTATE
.endstruct



.define ZFX_NUM_CHANNELS 24

.struct FXPTR
	addr_lo		.byte	ZFX_NUM_CHANNELS
	addr_hi		.byte	ZFX_NUM_CHANNELS
	bank		.byte	ZFX_NUM_CHANNELS
.endstruct

.struct ZFXSTATE
	active		.byte	1	; # of channels with active FX.
	chan_fm		.byte	1	; bitmask for FM channels in use
	chan_psg	.byte	2	; bitmask for PSG channels in use
	delay		.byte	ZFX_NUM_CHANNELS
	data		.tag	FXPTR
	loop		.tag	FXPTR
.endstruct

.endif
