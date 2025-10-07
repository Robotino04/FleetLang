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

struct ImageRow {
    struct ImageColor row[600];
};

struct Image800x600 {
    struct ImageRow value[800];
};


// Initialize raylib window
void helper_initialize_window(void) {
    const int screenWidth = 800;
    const int screenHeight = 600;
    //SetTraceLogLevel(LOG_ALL);
    InitWindow(screenWidth, screenHeight, "Raylib Image Viewer");
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
    for (int x = 0; x < 800; x++) {
        for (int y = 0; y < 600; y++) {
            struct ImageColor c = image.value[x].row[y];
            Color rayColor = {
                (unsigned char)(c.r / samples * 255.0f),
                (unsigned char)(c.g / samples * 255.0f),
                (unsigned char)(c.b / samples * 255.0f),
                255
            };
            DrawPixel(x, 600 - y, rayColor);
        }
    }

    EndDrawing();
}
