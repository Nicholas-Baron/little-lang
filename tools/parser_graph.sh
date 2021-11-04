#!/usr/bin/env bash

cd build/temp || exit

echo "Flattening the parser graph..."
unflatten -l10 -f -c 5 -o flattened.dot < parser.gv

echo "Drawing parser graph..."
time dot -Tsvg flattened.dot -o parser.svg

chrome parser.svg >/dev/null 2>&1 &
