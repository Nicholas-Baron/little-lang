#!/bin/sh

[ $# -ne 1 ] && echo "Usage: $0 <input file>" && exit

[ -f "build/littlec" ] || tools/build.sh || exit

build/littlec $1 | llc-8 - --x86-asm-syntax=intel -O=2 -filetype=obj -o "$1.o"

