#!/bin/sh

flags=$(jq '.[]["command"]' compile_commands.json | sed 's:"/usr/bin/clang++ \(.*\) -o.*:\1:g' | head -n1)
files=$(git ls-files -- 'src/*.cpp' 'src/*.hpp')
clang-tidy --quiet $files -- $flags 2>&1 > tidy.txt

{
	echo "Summary"
	grep '[A-Za-z_]+\.(c|h)pp:[0-9:]+ warning' tidy.txt -Eo | cut -f1 -d':' \
		| sort | uniq -c | sort -rn

} | tee -a tidy.txt
