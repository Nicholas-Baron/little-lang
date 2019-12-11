#!/bin/bash

tools/external/cinclude2dot --src src/ --include external/,temp/,/usr/lib/llvm-8/include,/usr/include/c++/8 | unflatten -l5 -f | dot -Tpdf -o temp/includes_src.pdf
tools/external/cinclude2dot --src temp/ --include src/,/usr/lib/llvm-8/include,/usr/include/c++/8,/usr/include | unflatten -l5 -f | dot -Tpdf -o temp/includes_temp.pdf
