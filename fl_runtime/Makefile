CFLAGS = -std=c++2a -O3
LDFLAGS = -lvulkan

all: example fl_runtime.bc

example: example.o fl_runtime.o compute.comp.spv Makefile
	clang++ $(LDFLAGS) -o example example.o fl_runtime.o

example.o: example.cpp Makefile
	clang++ -c $(CFLAGS) -o example.o example.cpp

fl_runtime.o: fl_runtime.cpp Makefile
	clang++ -c $(CFLAGS) -o fl_runtime.o fl_runtime.cpp

fl_runtime.bc: fl_runtime.cpp Makefile
	clang++ -c $(CFLAGS) -emit-llvm -o fl_runtime.bc fl_runtime.cpp

compute.comp.spv: compute.comp Makefile
	glslc compute.comp -o compute.comp.spv

.PHONY: clean run all


clean:
	rm -f example example.o compute.comp.spv fl_runtime.o fl_runtime.bc

run: example
	./example
