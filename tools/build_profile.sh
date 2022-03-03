#!/bin/bash

if [ ! -d build ]; then
	echo "Build the project at least once"
	exit
fi

ninja -C build
./tools/external/ninjatracing build/.ninja_log > trace.json

grep -Eo 'name[^{}]+dur": [0-9]+' trace.json | sed 's/^name": "\([^"]*\)".*: \([0-9]*\)$/\2 : \1/' | sort -rn > build/profile.txt
