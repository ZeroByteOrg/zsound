#!/usr/bin/env python

# Convert source audio file into ZCM digi file.
# Requires python 'sox' module to be installed, and the sox command line tool.

import os
import sys
import struct
import tempfile
import sox
import argparse


MAX_SIZE = 2048*1024 - 8192    #  2 mb banked ram minus 1 bank (8kb) for kernal.

def convert_source_to_raw(source: str, outputformat: dict):
    info = sox.file_info.info(source)
    print("Source file: ",source)
    print(f"   {info['channels']} channels, {info['bitdepth']} bits, {int(info['sample_rate'])} hz ({round(info['duration'], 3)} sec.)")
    tfm = sox.Transformer()
    tfm.set_output_format(file_type="raw", encoding = "signed-integer" if outputformat["bits"]>8 else None)
    tfm.set_globals(dither=True)
    # tfm.lowpass(frequency = outputformat["rate"]/2)
    tfm.convert(samplerate = outputformat["rate"], n_channels = outputformat["channels"], bitdepth = outputformat["bits"])
    temp_file = tempfile.mktemp(".raw")
    tfm.build_file(source, temp_file)
    return temp_file

def convert_raw_to_zcm(raw_file: str, output_file: str, outputformat: dict):
    samplerate = outputformat["rate"]
    bits = outputformat["bits"]
    channels = outputformat["channels"]
    vera_rate = int((samplerate/(25000000>>16))+1)
    vera_16_bit = 1<<5 if bits==16 else 0
    vera_stereo = 1<<4 if channels==2 else 0
    rawdata = open(raw_file, "rb").read()
    size = len(rawdata)
    if size > MAX_SIZE:
        rawdata = rawdata[:MAX_SIZE]
        size = len(rawdata)
        print("Warning: sample data truncated to", size, "bytes!")
    header = struct.pack("<HBHBBB", 0, 0, size & 0xffff, size>>16,
                         0x0f | vera_16_bit | vera_stereo, vera_rate)
    with open(output_file, "wb") as out:
        out.write(header)
        out.write(rawdata)
    os.remove(raw_file)
    duration = len(rawdata)/channels/samplerate
    if bits==16:
        duration /= 2
    print("Wrote", output_file, len(header)+len(rawdata), "bytes")
    print(f"   {channels} channels, {bits} bits, {samplerate} hz ({round(duration, 3)} sec.)")

if __name__=="__main__":
    parser = argparse.ArgumentParser(description="convert audio to zcm")
    parser.add_argument("-s", "--stereo", action="store_true", help="stereo output, default is mono")
    parser.add_argument("-r", "--rate", type=int, help="output sample rate, default is %(default)s", default=22050)
    parser.add_argument("-8", "--eight", action="store_true", help="8 bits output, default is 16")
    parser.add_argument("--raw", action="store_true", help="input file is raw pcm already, just add zcm header")
    parser.add_argument("inputfile")
    args = parser.parse_args()
    outputformat = {
        "channels": 2 if args.stereo else 1,
        "rate": args.rate,
        "bits": 8 if args.eight else 16
    }
    output_file = (os.path.splitext(os.path.split(args.inputfile)[1])[0] + ".zcm").upper()
    if args.raw:
        raw_file = args.inputfile
    else:
        raw_file = convert_source_to_raw(args.inputfile, outputformat)
    convert_raw_to_zcm(raw_file, output_file, outputformat)
