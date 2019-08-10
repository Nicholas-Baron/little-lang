#!/bin/bash

cd temp || exit

echo "Flattening the parser graph..."
unflatten -o flattened.dot < parser.dot

echo "Drawing parser graph..."
time dot -Tpdf flattened.dot -o parser.pdf

