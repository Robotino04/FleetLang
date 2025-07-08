CFLAGS = -O3 -fno-use-cxa-atexit
CXXFLAGS = $(CFLAGS) -std=c++2a
LDFLAGS = -lvulkan


all: example fl_runtime_declarations.bc fl_runtime.so

fl_runtime.o: fl_runtime.cpp Makefile
	clang++ -c $(CXXFLAGS) -o fl_runtime.o fl_runtime.cpp

# for testing only
fl_runtime.so: fl_runtime.o Makefile
	clang++ -shared -o fl_runtime.so fl_runtime.o

fl_runtime_declarations.bc: fl_runtime.h Makefile
	clang++ -c $(CFLAGS) -emit-llvm -o fl_runtime_declarations.bc -x c++ fl_runtime.h -DFL_RUNTIME_DECLARATION_AS_BITCODE


example: example.o fl_runtime.o compute.comp.spv Makefile
	clang++ $(LDFLAGS) -o example example.o fl_runtime.o

example.o: example.cpp Makefile
	clang++ -c $(CXXFLAGS) -o example.o example.cpp

compute.comp.spv: compute.comp Makefile
	glslc compute.comp -o compute.comp.spv

.PHONY: clean run all


clean:
	rm -f example example.o compute.comp.spv fl_runtime.o fl_runtime_declarations.bc fl_runtime.so

run: example
	./example
