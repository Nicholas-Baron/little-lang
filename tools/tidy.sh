#!/bin/sh

flags=$(jq '.[]["command"]' compile_commands.json | sed 's:"/usr/bin/clang++ \(.*\) -o.*:\1:g' | head -n1)
files=$(git ls-files -- 'src/*.cpp' 'src/*.hpp')

echo "Flags: $flags"
echo "Beginning linting..."
clang-tidy --quiet $files -- $flags 2>&1 | tee tidy.txt

echo "Summary"
grep '[a-z_]+\.(c|h)pp' tidy.txt -Eo | sort | uniq -c | sort -rn | tee -a tidy.txt
