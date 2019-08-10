#!/bin/bash

tools/external/cinclude2dot --src src/ --include temp/,/usr/lib/llvm-8/include,/usr/include/c++/8 | dot -Tpdf > temp/includes.pdf
