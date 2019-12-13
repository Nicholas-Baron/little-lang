#!/bin/sh

clang-format-8 src/*.cpp -i -verbose -style=file -fallback-style=none
clang-format-8 src/*.hpp -i -verbose -style=file -fallback-style=none

