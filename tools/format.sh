#!/bin/sh

clang-format $(find . -type f -name '*.[ch]pp' ! -path './build/*' | tr '\n' ' ') -i -verbose -style=file -fallback-style=none
cmake-format $(find . -type f -name 'CMakeLists.txt' | tr '\n' ' ') -i -c .cmake-format

git diff
