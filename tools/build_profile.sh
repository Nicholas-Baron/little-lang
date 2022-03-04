#!/bin/bash

if [ ! -d build ]; then
	echo "Build the project at least once"
	exit
fi

cd build

ninja clean
ninja

[ -f trace.json ] && mv -vb trace.json old_trace.json

../tools/external/ninjatracing .ninja_log > trace.json

[ -f profile.txt ] && mv -vb profile.txt old_profile.txt

grep -Eo 'name[^{}]+dur": [0-9]+' trace.json | sed 's/^name": "\([^"]*\)".*: \([0-9]*\)$/\2 : \1/' | sort -rn > profile.txt
