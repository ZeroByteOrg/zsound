#!/usr/bin/env python

# Shows info on a ZCM digi file and optionally plays the audio as well.
# Requires python 'sox' module to be installed (pip install sox), and the sox command line tool.

import struct
import tempfile
import argparse
import sox


class ZcmHeader():
    # note: this class is shared with other Zcm tools
    MAX_SIZE = 2048 * 1024 - 8192  # 2 mb banked ram minus 1 bank (8kb) for kernal.
    HEADER_FORMAT = "<HBHBBB"

    def __init__(self) -> None:
        self.sample_rate = 0
        self.bits = 0
        self.channels = 0
        self.size = 0
        self.volume = 0x0f
        self.duration = 0.0
        self.header_bytes = b""

    @staticmethod
    def from_config(samplerate: int, bits: int, channels: int, size: int) -> 'ZcmHeader':
        assert bits in (8, 16)
        assert channels in (1, 2)
        assert 1 <= samplerate <= 48828
        assert 0 < size <= ZcmHeader.MAX_SIZE
        header = ZcmHeader()
        vera_rate = header.samplerate_to_vera_rate(samplerate)
        vera_16_bit = 1 << 5 if bits == 16 else 0
        vera_stereo = 1 << 4 if channels == 2 else 0
        header.sample_rate = header.vera_rate_to_samplerate(vera_rate)
        header.bits = bits
        header.channels = channels
        header.size = size
        header.volume = 0x0f
        header.duration = size / channels / header.sample_rate
        if bits == 16:
            header.duration /= 2
        header.header_bytes = struct.pack(ZcmHeader.HEADER_FORMAT, 0, 0, size & 0xffff, size >> 16,
                                          vera_16_bit | vera_stereo | header.volume, vera_rate)
        return header

    @staticmethod
    def from_header(header_bytes: bytes) -> 'ZcmHeader':
        addr, bank, size_lo, size_hi, vera_cfg, vera_rate = struct.unpack(ZcmHeader.HEADER_FORMAT, header_bytes)
        if addr or bank:
            print("Warning: the first 3 bytes of the zcm aren't zero! (addr/bank)")
        header = ZcmHeader()
        header.size = size_lo + (size_hi << 16)
        header.bits = 16 if vera_cfg & (1 << 5) else 8
        header.channels = 2 if vera_cfg & (1 << 4) else 1
        header.volume = vera_cfg & 0x0f
        header.header_bytes = header_bytes
        header.sample_rate = header.vera_rate_to_samplerate(vera_rate)
        header.duration = header.size / header.channels / header.sample_rate
        if header.bits == 16:
            header.duration /= 2
        return header

    @staticmethod
    def samplerate_to_vera_rate(hz: int) -> int:
        return int((hz / (25e6 / 65536)) + 1)

    @staticmethod
    def vera_rate_to_samplerate(vrate: int) -> int:
        return int(vrate * (25e6 / 65536))


def play_zcm_file(filename: str, playback: bool = True) -> None:
    with open(filename, "rb") as fin:
        header = ZcmHeader.from_header(fin.read(8))
        pcm_bytes = fin.read()
        assert header.size == len(pcm_bytes)

    print("pcm size:", header.size, "bytes")
    print("bits:", header.bits)
    print("channels:", header.channels)
    print("sample rate:", header.sample_rate)
    print("volume:", header.volume)
    print("duration:", round(header.duration, 3), "sec.")

    if playback:
        print("Playing digi.")
        with tempfile.NamedTemporaryFile(suffix=".raw") as tmpfile:
            tmpfile.write(pcm_bytes)
            sox.core.sox(["-t", "raw", "-r", str(header.sample_rate),
                          "-b", str(header.bits),
                          "-c", str(header.channels),
                          "-e", "signed-integer", tmpfile.name, "-d"])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="display and/or play zcm digi file")
    parser.add_argument("-i", "--infoonly", action="store_true", help="only show info, don't play sound")
    parser.add_argument("zcmfile")
    args = parser.parse_args()
    play_zcm_file(args.zcmfile, not args.infoonly)
