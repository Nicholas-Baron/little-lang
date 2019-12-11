#!/bin/bash

tools/build.sh || exit

for file in ./test/*; do
	echo "Testing $file..."
	/usr/bin/time -v build/littlec "$file"
	printf "\n"
done

