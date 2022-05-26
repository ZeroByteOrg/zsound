#!/usr/bin/env python3

# This script is completely unusable at this point.
# It is a work-in-progress to replace the PHP-based
# vgm2zsm conversion tool. That tool has been pushed
# about as far as any sane person might do, and is not
# easy to make further changes to. This project intends
# to be much more modular to facilitate easily adding
# new chip types for conversion.

# architecture: classes for X16, VGM, ZSM, and translators

# VGM :
#	reads in files, and walks through them, sending
#	time ticks to X16, and chip writes to the various
#	translators. The translators convert the writes into
#	their best-fit YM2151 / VERA equivalents and send
#	those as writes to X16.
#
# ZSM:
#	Keeps track of the header, writes, etc and writes out
#	the final results when done.
#	Has structures to represent the YM2151 and VERA chips.
#	Performs de-duplication, and writes the results to
#	the ZSM file.
#
# Translators:
#	There will be one for each chip present in
#	the VGM file. They will request channels from X16 and
#   map them to the chip's possible channels. Will be
#   created and destroyed by the VGM object.

from dataclasses import dataclass

@dataclass
class zsmheader:
	magic = 'zm'
    loop = 		bankpointer(0,0xff)		# 3 bytes
    pcm =		bankpointer(0,0)		# 3 bytes
    chanmask:	int	= 0					# 3 bytes (24 flags)
    rate:		int	= 60				# 2 bytes (HZ of framerate)
    pad:		int = 5					# number of pad bytes (reserved bytes)

class zsmfile:
	ymChannelUsed = [False] * 8
	psgChannelUsed = [False] * 16
	ymShadow = [-1] * 256
	ymCache = [-1] * 256

	def __init__(self, filename, playrate):
		self.zsm = zsmfile(filename)

@dataclass
class bankpointer:
	addr:	int
	bank:	int


class zsmfile:
	prghdr = 0xa000
	empty = True
	dirty = False
	totalticks = 0

	def __init__(self, filename, playrate):
		self.filename = filename
		self.header = zsmheader()
		self.header.rate = playrate
		ticksperframe = self.header.rate

	def ymwrite(self, reg,val):
		print("YM written");

	def psgwrite(self, reg,val):
		print("PSG written");

	def pcmwrite(self,reg,val):
		print("PCM written");

	def sync():
		psg.sync()
		ym.sync()
		pcm.sync()


	def wait(self,ticks):
		if (self.empty == True):
			return
		self.totalticks += ticks
		self.ticks += ticks
		if (self.ticks / self.ticksperframe >= 1)
			print("ZSM: New frame")
			if(self.sync() == True):

class ym_chip:
	def sync(self):
		return

class psg_chip:
	def sync(self):
		return

class pcm_chip:
	def sync(self):
		return

zsm = zsmfile("bgm.zsm",60)
zsm.info()
