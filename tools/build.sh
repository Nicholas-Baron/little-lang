#!/bin/bash

OUTPUT_DIR="build"

if [ -d $OUTPUT_DIR ]; then rm -Rf $OUTPUT_DIR/; fi

mkdir -p $OUTPUT_DIR
cp src/* $OUTPUT_DIR/
cd $OUTPUT_DIR || exit

pwd

make -j
