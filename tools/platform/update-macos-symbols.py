#!/usr/bin/env python3

import os
import subprocess

OUTPUT = "macos-symbols.txt"
ROOT = "/tmp/libraries"
FIRST_FILE = "/usr/lib/libSystem.B.dylib"

LIBS = set()
SYMS = set()

QUEUE=[FIRST_FILE]

if not os.path.exists(ROOT):
    print("Gebruik dit gereedschapje om de gedeelde DYLD-cache uit te lezen")
    print("   <https://github.com/keith/dyld-shared-cache-extractor>")
    exit(1)

def fixname(lib):
    if not lib.startswith(ROOT):
        return ROOT + lib
    else:
        return lib

def find_lib(lib):
    lib = fixname(lib)

    if lib in LIBS:
        return

    if 'PrivateFramework' in lib:
        return

    LIBS.add(lib)

    print(f"INDEXING {lib}")

    proc = subprocess.Popen(
        ["otool", "-L", lib],
        stdout=subprocess.PIPE,
    )

    # index dependencies
    for line in iter(proc.stdout.readline, b''):
        line = line.decode()
        if line[0] != '\t': # skip irrelevant files
            continue

        line = line.strip()
        dep = line.split(' ')[0]
        dep = fixname(dep)

        if not dep in LIBS and not dep in QUEUE:
            QUEUE.append(dep)
            print(f"   FOUND {dep}")

    # index symbols
    proc = subprocess.Popen(
        ["nm", "-gU", lib],
        stdout=subprocess.PIPE,
    )

    # index dependencies
    for line in iter(proc.stdout.readline, b''):
        line = line.decode().rstrip()
        if line[0] == ' ' or line[0] == '\t':
            continue
        [val, typ, name] = line.split()

        if typ != 'T':
            continue

        name = name[1:]
        if name.startswith("$") or name.startswith("_Z"):
            continue

        SYMS.add(name)

while len(QUEUE) != 0:
    lib = QUEUE.pop(0)
    find_lib(lib)

print(f" WRITING {OUTPUT}")

with open(OUTPUT, "w") as text_file:
    for sym in SYMS:
        text_file.write(sym)
        text_file.write('\n')

print(f" SORTING {OUTPUT}")
subprocess.Popen(
    ["sort", "-o", OUTPUT, OUTPUT],
    stdout=subprocess.PIPE,
)

print(f"    DONE")
