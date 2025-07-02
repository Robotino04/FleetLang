CFLAGS = -std=c++2a -O3
LDFLAGS = -lvulkan

template: main.o fl_runtime.o compute.comp.spv Makefile
	clang++ $(LDFLAGS) -o template main.o fl_runtime.o

main.o: main.cpp Makefile
	clang++ -c $(CFLAGS) -o main.o main.cpp

fl_runtime.o: fl_runtime.cpp Makefile
	clang++ -c $(CFLAGS) -o fl_runtime.o fl_runtime.cpp

fl_runtime.bc: fl_runtime.cpp Makefile
	clang++ -c $(CFLAGS) -emit-llvm -o fl_runtime.bc fl_runtime.cpp

compute.comp.spv: compute.comp Makefile
	glslangValidator -V compute.comp -o compute.comp.spv

.PHONY: clean run all

all: template fl_runtime.bc

clean:
	rm -f template main.o compute.comp.spv fl_runtime.o fl_runtime.bc

run: template
	./template
