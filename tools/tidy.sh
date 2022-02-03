#!/bin/sh

flags=$(jq '.[]["command"]' compile_commands.json | sed 's:"/usr/bin/clang++ \(.*\) -o.*:\1:g' | head -n1)

echo "Flags: $flags"
echo "Beginning linting..."
clang-tidy src/*.cpp src/*.hpp -- $flags 2>&1 | tee tidy.txt
