#!/bin/sh

clang-format $(git ls-files -- *.cpp *.hpp) -i -verbose -style=file -fallback-style=none
cmake-format $(find . -type f -name 'CMakeLists.txt' | tr '\n' ' ') -i -c .cmake-format

git diff
