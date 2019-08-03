#!/bin/bash

OUTPUT_DIR="build"

if [ -d $OUTPUT_DIR ]; then rm -Rf $OUTPUT_DIR/; fi

mkdir -p $OUTPUT_DIR temp
cd $OUTPUT_DIR || exit

pwd

cmake ..
make -j
