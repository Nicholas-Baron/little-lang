#!/usr/bin/env sh

llvm_doxygen='https://llvm.org/doxygen'
llvm_class_list="$llvm_doxygen/classes.html"

[ $# -eq 1 ] || {
	echo "Usage: $0 <LLVM class to search for>"
	exit 1
}

llvm_url=$(curl -s $llvm_class_list | grep -iF "$1" | grep -Eio 'href="[^"]+"' | head -n1 | sed -E "s@href=\"(.+)\"@$llvm_doxygen/\1@g")

if [ "$llvm_url" != "" ]; then
	echo "Opening $llvm_url ..."
	chrome "$llvm_url" >/dev/null 2>&1 &
fi
