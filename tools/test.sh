#!/bin/sh

COMPILER="build/littlec"

[ -f $COMPILER ] || tools/build.sh || exit

(
	for file in ./test/*; do
		echo "Testing $file..."
		/usr/bin/time -v $COMPILER "$file"
		printf "\n"
	done
) 2>&1 | tee ./logs/testing.txt

