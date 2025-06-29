CFLAGS = -g -std=c++2a
LDFLAGS = -lvulkan

template: main.o compute.comp.spv Makefile
	clang++ $(LDFLAGS) -o template main.o

main.o: main.cpp Makefile
	clang++ -c $(CFLAGS) -o main.o main.cpp

compute.comp.spv: compute.comp Makefile
	glslangValidator -V compute.comp -o compute.comp.spv

.PHONY: clean run

clean:
	rm -f template main.o compute.comp.spv

run: template
	./template
