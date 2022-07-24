#!/bin/sh

clang-format $(git ls-files -- *.cpp *.hpp) -i -verbose -style=file -fallback-style=none
cmake-format $(git ls-files -- CMakeLists.txt */CMakeLists.txt) -i -c .cmake-format

git diff
