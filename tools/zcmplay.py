#!/usr/bin/env python

# shows info on a ZCM digi file and optionally plays the audio as well.

import sys
import os
import struct
import tempfile
import argparse
import sox


class ZcmFile:
    def __init__(self, filename: str) -> None:
        with open(filename, "rb") as fin:
            addr, bank, self.size, size_hi, vera_cfg, vera_rate = struct.unpack("<HBHBBB", fin.read(8))
            self.sixteen_bit = bool(vera_cfg & 1<<5)
            self.stereo = bool(vera_cfg & 1<<4)
            self.volume = vera_cfg & 0x0f
            self.size += size_hi << 16
            self.pcm_bytes = fin.read()
            self.sample_rate = int(vera_rate * (25e6 / 65536))
            if addr!=0 or bank!=0:
                print("Warning: the first 3 bytes of the zcm file aren't zero! (addr/bank)")
            assert self.size == len(self.pcm_bytes)

    def print_info(self) -> None:
        duration = self.size / self.sample_rate
        if self.sixteen_bit:
            duration /= 2
        if self.stereo:
            duration /= 2
        print("pcm size:", self.size, "bytes")
        print("16 bit" if self.sixteen_bit else "8 bit", end=" ")
        print("stereo" if self.stereo else "mono", end=", ")
        print(self.sample_rate, "hz.")
        print("volume:", self.volume)
        print("duration:", round(duration, 3), "sec.")

    def play(self) -> None:
        print("Playing digi.")
        with tempfile.NamedTemporaryFile(suffix=".raw") as tmpfile:
            tmpfile.write(self.pcm_bytes)
            sox.core.sox(["-t", "raw", "-r", str(self.sample_rate),
                          "-b", "16" if self.sixteen_bit else "8",
                          "-c", "2" if self.stereo else "1",
                          "-e", "signed-integer", tmpfile.name, "-d"])


if __name__=="__main__":
    parser = argparse.ArgumentParser(description="display and/or play zcm digi file")
    parser.add_argument("-i", "--infoonly", action="store_true", help="only show info, don't play sound")
    parser.add_argument("zcmfile")
    args = parser.parse_args()
    zcm = ZcmFile(args.zcmfile)
    zcm.print_info()
    if not args.infoonly:
        zcm.play()
