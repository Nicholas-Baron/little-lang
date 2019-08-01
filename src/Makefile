OUTPUT=littlec
COMPILE_FLAGS=-Wall -std=c++17

all: parser tokens 
	clang++ -o ${OUTPUT} *.cpp $$(llvm-config --cxxflags --ldflags --libs core native) ${COMPILE_FLAGS}

parser:
	bison -d -o parser.cpp parser.y --report=state --graph -t -Wall
	dot -Tpdf parser.dot -o parser.pdf

tokens: parser
	flex -o tokens.cpp tokens.l parser.hpp
