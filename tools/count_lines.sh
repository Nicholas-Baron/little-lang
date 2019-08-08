#!/bin/bash

list_file_lines () {
	for file in $1/*; do
		if [ -d "$file" ] && [ "$file" != "$1" ]; then
			list_file_lines "$file"
		elif [ -f "$file" ]; then
			count=$(sed '/^\s*$/d' < "$file" | wc -l)
			printf "%d\t%s\n" "$count" "$file"
		fi
	done
}

count=$(
	tabs 4

	printf "\nLine count of the src files:\n"
	list_file_lines src | sort -gr

	if [ -d "temp" ]; then
		printf "\nLine count of the temp files:\n"
		list_file_lines temp | sort -gr
	fi

	printf "\n"
) 

printf "%s\n" "$count"

printf "\nTotal count: "
echo "$count" | grep -oP '^\d+' | awk '{s+=$1} END {print s}'
printf "\n"
