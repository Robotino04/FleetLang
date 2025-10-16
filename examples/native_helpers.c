#include <stdio.h>
#include <stdint.h>
#include "raylib.h"

void eputchar(uint8_t c) {
    fprintf(stderr, "%c", c);
}

// Structures
struct ImageColor {
    float r;
    float g;
    float b;
};

#define IMAGE_WIDTH 1920
#define IMAGE_HEIGHT 1080

struct ImageRow {
    struct ImageColor row[IMAGE_HEIGHT];
};

struct Image800x600 {
    struct ImageRow value[IMAGE_WIDTH];
};


// Initialize raylib window
void helper_initialize_window(void) {
    // SetTraceLogLevel(LOG_ALL);
    InitWindow(IMAGE_WIDTH, IMAGE_HEIGHT, "Raylib Image Viewer");
}

// Deinitialize raylib window
void helper_deinitialize_window(void) {
    CloseWindow();
}

// Submit a frame to display (copy image data and show)
void helper_submit_frame(struct Image800x600 image, float samples) {
    BeginDrawing();
    ClearBackground(BLACK);

    // Draw each pixel
    for (int x = 0; x < IMAGE_WIDTH; x++) {
        for (int y = 0; y < IMAGE_HEIGHT; y++) {
            struct ImageColor c = image.value[x].row[y];
            Color rayColor = {
                (unsigned char)(c.r / samples * 255.0f),
                (unsigned char)(c.g / samples * 255.0f),
                (unsigned char)(c.b / samples * 255.0f),
                255
            };
            DrawPixel(x, IMAGE_HEIGHT - y, rayColor);
        }
    }

    EndDrawing();
}
