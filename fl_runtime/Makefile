CFLAGS = -std=c++2a -O3 -fno-use-cxa-atexit
LDFLAGS = -lvulkan


all: example fl_runtime.bc dso_handle.bc

example: example.o fl_runtime.o compute.comp.spv Makefile
	clang++ $(LDFLAGS) -o example example.o fl_runtime.o

example.o: example.cpp Makefile
	clang++ -c $(CFLAGS) -o example.o example.cpp

fl_runtime.o: fl_runtime.cpp Makefile
	clang++ -c $(CFLAGS) -o fl_runtime.o fl_runtime.cpp

fl_runtime.bc: fl_runtime.cpp Makefile
	clang++ -c $(CFLAGS) -emit-llvm -o fl_runtime.bc fl_runtime.cpp

# https://root-forum.cern.ch/t/cling-execution---dso-handle-missing/13216
# provides a fake __dso_handle for test execution
dso_handle.bc: dso_handle.cpp Makefile
	clang++ -c $(CFLAGS) -emit-llvm -o dso_handle.bc dso_handle.cpp

compute.comp.spv: compute.comp Makefile
	glslc compute.comp -o compute.comp.spv

.PHONY: clean run all


clean:
	rm -f example example.o compute.comp.spv fl_runtime.o fl_runtime.bc dso_handle.bc

run: example
	./example
