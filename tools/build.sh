#!/bin/sh

tools/format.sh

OUTPUT_DIR="build"
TEMP_DIR="temp"

[ -d $OUTPUT_DIR ] && rm -Rf $OUTPUT_DIR
[ -d $TEMP_DIR ] && rm -Rf $TEMP_DIR

mkdir -p $OUTPUT_DIR $TEMP_DIR logs
cd $OUTPUT_DIR || exit
clear

(
	pwd
	cmake -G "Unix Makefiles" ..
	make -j$(nproc)
) 2>&1 | tee ../logs/build.txt
