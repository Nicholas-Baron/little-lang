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

	LLVM_PROFILE_FILE=$(basename "$f.profraw")
	echo "Compiling $f (coverage = $LLVM_PROFILE_FILE)..."
	$compiler $f --no-output
	ls
	mv -v default.profraw $LLVM_PROFILE_FILE
	success=$?
	if [ $success -ne 0 ]; then
		echo "Failed to compile $f : exited with $success"
	fi
done

find . -type f -name '*.lil.profraw'
llvm-profdata merge -sparse $(find . -type f -name '*.lil.profraw') -o total.profdata
llvm-cov show $compiler -instr-profile=total.profdata > $old_dir/cov_report.txt

# Cleanup
cd $old_dir
rm -rf $active_dir

