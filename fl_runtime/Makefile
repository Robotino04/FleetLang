MAKEFLAGS += -r

CFLAGS := \
	-O3 \
	-Wreturn-stack-address \
	-fstack-protector-all \
	-fno-omit-frame-pointer \
	-g

CXXFLAGS := \
	$(CFLAGS) \
	-std=c++2a

CXX := clang++
CC := clang

LDFLAGS := -g
LDFLAGS_RUNTIME := $(LDFLAGS) -lvulkan

EXAMPLE_CXXFLAGS := \
	-fsanitize=undefined \
	-fsanitize=address \
	-fstack-protector-all \
	-fsanitize-address-use-after-return=always


all: example fl_runtime_declarations.bc fl_runtime.so testhook.so

fl_runtime.o: fl_runtime.cpp
	$(CXX) -c $(CXXFLAGS) -o fl_runtime.o fl_runtime.cpp

# for testing only
fl_runtime.so: fl_runtime.o
	$(CXX) $(LDFLAGS) $(CXXFLAGS) -shared -o fl_runtime.so fl_runtime.o

fl_runtime_declarations.bc: fl_runtime.h
	$(CXX) -c $(CXXFLAGS) -emit-llvm -o fl_runtime_declarations.bc -x c++ fl_runtime.h -DFL_RUNTIME_DECLARATION_AS_BITCODE


example: example.o fl_runtime.o compute.comp.spv
	$(CXX) $(LDFLAGS_RUNTIME) $(EXAMPLE_CXXFLAGS) $(CXXFLAGS) -o example example.o fl_runtime.o

example.o: example.cpp
	$(CXX) -c $(EXAMPLE_CXXFLAGS) $(CXXFLAGS) -o example.o example.cpp

compute.comp.spv: compute.comp
	glslc compute.comp -o compute.comp.spv

testhook.so: testhook.c
	$(CC) $(LDFLAGS) $(CFLAGS) -fPIC -shared -o testhook.so testhook.c -ldl

.PHONY: clean run all

clean:
	rm -f example example.o compute.comp.spv fl_runtime.o fl_runtime_declarations.bc fl_runtime.so testhook.so

run: example
	./example
