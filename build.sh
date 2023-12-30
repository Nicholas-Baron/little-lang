#!/bin/sh

[ ! -d build ] \
	&& cmake -GNinja \
	-B build -S . \
	-DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=cmake/conan_provider.cmake \
	-DCMAKE_BUILD_TYPE=Debug

ninja -C build

