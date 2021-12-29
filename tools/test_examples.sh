#!/bin/sh

# Setup the build folder
[ -d build ] || cmake -B build -DCMAKE_BUILD_TYPE=Debug -GNinja . || exit

compiler="$(realpath -m build/bin/littlec)"
examples_dir=$(realpath -e examples)
old_dir=$PWD

# Build the compiler
cd build
[ -x "$compiler" ] || ninja littlec
[ -x "$compiler" ] || exit

# Setup a temp dir
active_dir=$(mktemp -d)
cd $active_dir || exit

# Compile examples that contain the word `main`
for f in $examples_dir/*.lil; do
	if [ ! -f "$f" ]; then
		continue
	elif ! grep -wq main $f; then
		continue
	fi

	echo "Compiling $f ..."
	$compiler $f
	success=$?
	if [ $success -ne 0 ]; then
		echo "Failed to compile $f : exited with $success"
	fi
done

# Cleanup
cd $old_dir
rm -rf $active_dir
