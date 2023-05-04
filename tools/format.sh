#!/bin/sh

files=$(find . -type f -name '*.[ch]pp' ! -path './build/*' | tr '\n' ' ')
clang-format $files -i -verbose -style=file -fallback-style=none
cmake-format $(find . -type f -name 'CMakeLists.txt' | tr '\n' ' ') -i -c .cmake-format

for f in $files; do
	if [ -n "$(tail -c 1 "$f")" ]; then
		echo >> "$f"
	fi
done

git diff
