#!/bin/sh

flags=$(jq '.[]["command"]' compile_commands.json | sed 's:"/usr/bin/clang++ \(.*\) -o.*:\1:g' | head -n1)
files=$(git ls-files -- 'src/*.cpp' 'src/*.hpp')

temp_dir=$(mktemp -d)

for file in $files; do
	outfile="$temp_dir/$file.txt"
	mkdir -p "$(dirname "$outfile")"
	clang-tidy --quiet "$file" -- $flags > "$outfile" 2>&1 &
done

wait

grep -Exv '[0-9]+ warnings generated.' $(fd -a 'txt$' "$temp_dir") > tidy.txt

rm -r "$temp_dir"

{
	echo "Summary"
	grep '[A-Za-z_]+\.(c|h)pp:[0-9:]+ warning' tidy.txt -Eo | cut -f1 -d':' \
		| sort | uniq -c | sort -rn

} | tee -a tidy.txt
