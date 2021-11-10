#!/bin/bash


llvm_flags=$(llvm-config --cxxflags --ldflags --libs core native | grep -o '/[^ ]*' | grep 'include')

touch lol.cpp
cpp_lib=$(clang++ -c lol.cpp -v 2>&1 | grep '^[ ]*/.*include' | tr '\n' ',' | tr -d ' ')
rm -f lol.o lol.cpp &

# echo "LLVM: $llvm_flags"
# echo "Clang: $cpp_lib"

tools/external/cinclude2dot --src src/ --include build/temp/,$llvm_flags,$cpp_lib | dot -Tsvg -o includes.svg
chrome includes.svg 2>&1 >/dev/null &
