#include <stdio.h>
#include <stdint.h>

void eputchar(uint8_t c) {
    fprintf(stderr, "%c", c);
}
