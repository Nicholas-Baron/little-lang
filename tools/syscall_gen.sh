#!/usr/bin/bash

syscall_source='/usr/include/asm/unistd_64.h'

mkdir -p examples/stdlib

{
	echo "# extracted from $syscall_source"
	echo "export {"
	grep '^#define __' $syscall_source \
		| sed -E 's/#define __NR_([a-z0-9_]+) ([0-9]+)/    const \1_num : int = \2/g'
	echo "}"
} > examples/stdlib/syscall_nums.lil
