#!/bin/sh

clang-format $(git ls-files -- src/*.cpp src/*.hpp) -i -verbose -style=file -fallback-style=none

