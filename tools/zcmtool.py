#!/usr/bin/env python

# Tool to convert source audio file into ZCM digi file, or play them, or show info.
# Requires python 'sox' module to be installed (pip install sox), and the sox command line tool.

import os
import struct
import tempfile
import sox
import argparse


class ZcmHeader():
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


def convert_source_to_raw(source: str, outputformat: dict) -> str:
    info = sox.file_info.info(source)
    print(f"Source file: {source}")
    print(f"   {info['channels']} channels, {info['bitdepth']} bits, {int(info['sample_rate'])} hz ({round(info['duration'], 3)} sec.)")
    tfm = sox.Transformer()
    tfm.set_output_format(file_type="raw", encoding="signed-integer" if outputformat["bits"] > 8 else "signed-integer")
    tfm.set_globals(dither=True)
    # tfm.lowpass(frequency = outputformat["rate"]/2)
    tfm.convert(samplerate=outputformat["rate"], n_channels=outputformat["channels"], bitdepth=outputformat["bits"])
    temp_file = tempfile.mktemp(".raw")
    tfm.build_file(source, temp_file)
    return temp_file


def convert_raw_to_zcm(raw_file: str, output_file: str, outputformat: dict) -> None:
    rawdata = open(raw_file, "rb").read()
    size = len(rawdata)
    if size > ZcmHeader.MAX_SIZE:
        rawdata = rawdata[:ZcmHeader.MAX_SIZE]
        size = len(rawdata)
        print(f"Warning: sample data truncated to {size} bytes!")
    header = ZcmHeader.from_config(outputformat["rate"], outputformat["bits"], outputformat["channels"], size)
    with open(output_file, "wb") as out:
        out.write(header.header_bytes)
        out.write(rawdata)
    print(f"Wrote {output_file} {len(header.header_bytes)+len(rawdata)} bytes")
    print(f"   {header.channels} channels, {header.bits} bits, {header.sample_rate} hz ({round(header.duration, 3)} sec.)")
    if header.sample_rate != outputformat["rate"]:
        print("Note: actual sample rate differs from requested sample rate (due to Vera clock).")


def play_audio_file(filename: str, only_info: bool = True) -> None:
    if os.path.splitext(filename)[1].lower() == ".zcm":
        # play ZCM file by reading the raw pcm data
        with open(filename, "rb") as fin:
            header = ZcmHeader.from_header(fin.read(8))
            pcm_bytes = fin.read()
            assert header.size == len(pcm_bytes)
        print(f"Info about zcm file {filename}")
        print(f"pcm size: {header.size} bytes")
        print(f"bits: {header.bits}")
        print(f"channels: {header.channels}")
        print(f"sample rate: {header.sample_rate}")
        print(f"volume: {header.volume}")
        print(f"duration: {round(header.duration, 3)} sec.")
        if not only_info:
            print("Playing audio.")
            with tempfile.NamedTemporaryFile(suffix=".raw") as tmpfile:
                tmpfile.write(pcm_bytes)
                sox.core.sox(["-t", "raw", "-r", str(header.sample_rate),
                              "-b", str(header.bits),
                              "-c", str(header.channels),
                              "-e", "signed-integer", tmpfile.name, "-d"])
    else:
        # let sox handle the playback of the audio file
        info = sox.file_info.info(filename)
        print(f"Info about audio file {filename}")
        print(f"encoding: {info['encoding']}")
        print(f"bits: {info['bitdepth']}")
        print(f"channels: {info['channels']}")
        print(f"sample rate: {info['sample_rate']}")
        print(f"duration: {info['duration']} sec.")
        if not only_info:
            print("Playing audio.")
            sox.core.play([filename])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="zcm digi audio conversion and playback")
    parser.add_argument("-s", "--stereo", action="store_true", help="stereo output, default is mono")
    parser.add_argument("-r", "--rate", type=int, help="output sample rate, default is %(default)s", default=22050)
    parser.add_argument("-8", "--eight", action="store_true", help="8 bits output, default is 16")
    parser.add_argument("-o", "--output", type=str, help="provide your own output file name")
    parser.add_argument("-p", "--play", action="store_true", help="playback the input file")
    parser.add_argument("-i", "--info", action="store_true", help="show info about the input file")
    parser.add_argument("--raw", action="store_true", help="input file is raw pcm, just add zcm header when converting")
    parser.add_argument("inputfile")
    args = parser.parse_args()
    if args.play:
        play_audio_file(args.inputfile, only_info=False)
    elif args.info:
        play_audio_file(args.inputfile, only_info=True)
    else:
        # audio conversion
        outputformat = {
            "channels": 2 if args.stereo else 1,
            "rate": args.rate,
            "bits": 8 if args.eight else 16
        }
        output_file = args.output if args.output else (os.path.splitext(os.path.split(args.inputfile)[1])[0] + ".zcm").upper()
        raw_file = args.inputfile if args.raw else convert_source_to_raw(args.inputfile, outputformat)
        convert_raw_to_zcm(raw_file, output_file, outputformat)
        if not args.raw:
            os.remove(raw_file)
