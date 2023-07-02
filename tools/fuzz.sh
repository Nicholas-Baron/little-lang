#!/bin/sh

# Setup build
mkdir -p build
cd build

cmake -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++ ..
ninja fuzzer

[ -f corpus.txt ] || fd -t f -e lil ../examples/ > corpus.txt

./bin/fuzzer -seed_inputs=@corpus.txt -runs=25 -max_len=1024

mkdir -p crashes
mv -vt crashes crash-*

