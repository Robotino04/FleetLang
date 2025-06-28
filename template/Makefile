template: glad.o main.o
	clang++ -o template -lglfw -lGL main.o glad.o

main.o: main.cpp
	clang++ -c -o main.o -std=c++2a main.cpp

glad.o: glad/src/glad.c glad/include/KHR/khrplatform.h glad/include/glad/glad.h
	clang++ -c -o glad.o -Iglad/include/ glad/src/glad.c

.PHONY: clean run

clean:
	rm template main.o glad.o

run: template
	./template
