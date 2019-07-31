
OUTPUT="littlec"
LLVM_SETTINGS=$(llvm-config --cxxflags --ldflags --libs core native)
COMPILE_FLAGS="-Wall -Wextra -std=c++17"

all: parser tokens 
	clang++ -o ${OUTPUT} *.cpp ${LLVM_SETTINGS} ${COMPILE_FLAGS}

parser:
	bison -d -o parser.cpp parser.y

tokens: parser
	flex -o tokens.cpp tokens.l parser.hpp
