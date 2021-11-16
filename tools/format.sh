#!/bin/sh

clang-format $(git ls-files -- *.cpp *.hpp) -i -verbose -style=file -fallback-style=none

