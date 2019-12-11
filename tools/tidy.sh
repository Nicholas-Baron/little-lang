#!/bin/sh

tools/format.sh

echo "Beginning linting..."
clang-tidy-8 src/*.cpp -- -I./temp -I./external -I/usr/lib/llvm-8/include -std=c++0x -Wl,--no-keep-files-mapped -Wl,--no-map-whole-files -fPIC -fvisibility-inlines-hidden -Werror=date-time -std=c++11 -Wall -W -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wno-missing-field-initializers -pedantic -Wno-long-long -Wno-maybe-uninitialized -Wdelete-non-virtual-dtor -Wno-comment -ffunction-sections -fdata-sections -O2 -DNDEBUG  -fno-exceptions -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -lLLVM-6.0 -std=c++17 -Wall -Wextra 2>&1 | tee logs/tidy.txt
