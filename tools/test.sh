#!/bin/bash

tools/build.sh || exit

for file in ./test/*; do
	echo "Testing $file..."
	build/littlec "$file"
done

