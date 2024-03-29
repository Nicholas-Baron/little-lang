#!/bin/bash

if [ -d build ]; then
	echo "Refreshing build..."
	pushd build > /dev/null || exit
	ninja
	popd > /dev/null || exit
else
	echo "Setup the build directory first"
	exit
fi

flags=$(jq '.[]["command"]' compile_commands.json | sed 's:"/usr/bin/clang++ \(.*\) -o.*:\1:g' | head -n1)
files=$(find src -type f -name '*.[ch]pp')

temp_dir=$(mktemp -d)

for file in $files; do
	outfile="$temp_dir/$file.txt"
	mkdir -p "$(dirname "$outfile")"

	if [ "$(jobs | wc -l)" -ge "$(nproc)" ]; then
		sleep 5
	fi

	clang-tidy --quiet "$file" -- $flags > "$outfile" 2>&1 &
done

echo "Waiting for all files to finish"
wait

grep -Ehxv '[0-9]+ warnings generated.' $(fd -a 'txt$' "$temp_dir") > tidy.txt

rm -r "$temp_dir"

{
	echo "Summary"
	grep '[A-Za-z_]+\.(c|h)pp:[0-9:]+ warning' tidy.txt -Eo | cut -f1 -d':' \
		| sort | uniq -c | sort -rn

} | tee -a tidy.txt
