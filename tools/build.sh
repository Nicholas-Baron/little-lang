#!/bin/bash

tools/format.sh

OUTPUT_DIR="build"
TEMP_DIR="temp"

if [ -d $OUTPUT_DIR ]; then rm -Rf $OUTPUT_DIR; fi
if [ -d $TEMP_DIR ]; then rm -Rf $TEMP_DIR; fi

mkdir -p $OUTPUT_DIR $TEMP_DIR logs
cd $OUTPUT_DIR || exit

clear
pwd

(
	cmake -G Ninja ..
	ninja
) 2>&1 | tee ../logs/build.txt
