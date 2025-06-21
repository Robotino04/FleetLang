#include <chrono>
#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <format>

#define GLFW_INCLUDE_NONE

#include <GLFW/glfw3.h>
#include "glad/include/glad/glad.h"

#ifdef __clangd__
    #include <GL/gl.h>
#endif

void onGLFWError(int errorCode, const char* description) {
    std::cerr << "GLFW ERROR " << errorCode << ": " << description << "\n";
    std::exit(1);
}

std::string loadFile(std::string const& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Failed to open file '" << filename << "'\n";
        std::exit(1);
    }
    std::stringstream str;
    str << file.rdbuf();
    return str.str();
}


GLuint loadShader(std::string const& filename) {
    std::string shaderSource = loadFile(filename);
    const char* shaderSource_c = shaderSource.c_str();
    const int shaderLength = shaderSource.length();
    GLuint shader = glCreateShader(GL_COMPUTE_SHADER);
    glShaderSource(shader, 1, &shaderSource_c, &shaderLength);
    glCompileShader(shader);
    GLint compileStatus;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compileStatus);
    if (compileStatus != GL_TRUE) {
        GLint logLength;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &logLength);
        std::vector<char> log_vec(logLength);
        std::cout << log_vec.size() << " " << logLength << "\n";
        glGetShaderInfoLog(shader, log_vec.size(), nullptr, log_vec.data());
        std::cout << "Failed to compile shader:\n" << log_vec.data() << "\n";
        std::exit(1);
    }

    GLuint program = glCreateProgram();
    glAttachShader(program, shader);
    glLinkProgram(program);
    GLint linkStatus;
    glGetProgramiv(program, GL_LINK_STATUS, &linkStatus);
    if (linkStatus != GL_TRUE) {
        GLint logLength;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &logLength);
        std::vector<char> log_vec(logLength);
        std::cout << log_vec.size() << " " << logLength << "\n";
        glGetProgramInfoLog(program, log_vec.size(), nullptr, log_vec.data());
        std::cout << "Failed to link shader program:\n" << log_vec.data() << "\n";
        std::exit(1);
    }

    return program;
}

GLuint createTexture(int width, int height) {
    GLuint texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, width, height, 0, GL_RGBA, GL_FLOAT, nullptr);

    return texture;
}

double scrollX = 0, scrollY = 0;
void scrollCallback(GLFWwindow* window, double dx, double dy) {
    scrollX = dx;
    scrollY = dy;
}

int main() {
    glfwSetErrorCallback(onGLFWError);

    if (glfwInit() != GLFW_TRUE) {
        std::cerr << "Failed to initialize GLFW\n.";
        return 1;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);

    GLFWwindow* window = glfwCreateWindow(800, 600, "Window", nullptr, nullptr);
    if (window == nullptr) {
        std::cerr << "Failed to create window";
        return 1;
    }


    glfwMakeContextCurrent(window);
    if (!gladLoadGL()) {
        std::cerr << "Failed to initialize GLAD\n";
        return 1;
    }

    glfwSwapInterval(0);

    GLuint program = loadShader("./compute.comp");
    glUseProgram(program);
    GLint imageSize_loc = glGetUniformLocation(program, "imageSize");
    GLint topleft_loc = glGetUniformLocation(program, "topleft");
    GLint zoom_loc = glGetUniformLocation(program, "zoom");

    int textureWidth = 800, textureHeight = 600;
    GLuint texture = createTexture(textureWidth, textureHeight);


    GLuint fbo;
    glGenFramebuffers(1, &fbo);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, fbo);

    glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture, 0);

    double zoom = 1;
    double x0 = double(textureWidth) / 2;
    double y0 = double(textureHeight) / 2;

    bool isHeld = false;

    double lastCursorX, lastCursorY;
    double cursorX, cursorY;

    glfwSetScrollCallback(window, scrollCallback);

    auto frameStartTime = std::chrono::high_resolution_clock::now();
    float dt = 1.0f / 60.0f;
    while (!glfwWindowShouldClose(window)) {
        scrollX = 0;
        scrollY = 0;
        glfwPollEvents();

        if (glfwGetKey(window, GLFW_KEY_ESCAPE)) {
            glfwSetWindowShouldClose(window, true);
        }

        glfwGetCursorPos(window, &cursorX, &cursorY);
        if (glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_1) == GLFW_PRESS) {
            if (!isHeld) {
                glfwGetCursorPos(window, &lastCursorX, &lastCursorY);
                isHeld = true;
            }
            else {
                x0 -= lastCursorX - cursorX;
                y0 -= lastCursorY - cursorY;

                lastCursorX = cursorX;
                lastCursorY = cursorY;
            }
        }
        else {
            isHeld = false;
        }

        x0 -= cursorX - double(textureWidth) / 2;
        y0 -= cursorY - double(textureHeight) / 2;
        x0 *= zoom;
        y0 *= zoom;
        zoom += scrollY * zoom * 0.5;
        x0 /= zoom;
        y0 /= zoom;
        x0 += cursorX - double(textureWidth) / 2;
        y0 += cursorY - double(textureHeight) / 2;

        int fb_width, fb_height;
        glfwGetFramebufferSize(window, &fb_width, &fb_height);
        if (fb_width != textureWidth || fb_height != textureHeight) {
            x0 = double(textureWidth) / 2;
            y0 = double(textureHeight) / 2;

            glDeleteTextures(1, &texture);
            textureWidth = fb_width;
            textureHeight = fb_height;

            texture = createTexture(textureWidth, textureHeight);
            glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture, 0);
            std::cout << "Recreated texture\n";
        }

        glClearColor(1, 0, 0, 1);
        glClear(GL_COLOR_BUFFER_BIT);


        glUseProgram(program);

        glBindImageTexture(0, texture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32F);
        glUniform2i(imageSize_loc, textureWidth, textureHeight);
        glUniform2i(topleft_loc, x0, -y0);
        glUniform1f(zoom_loc, zoom);

        glDispatchCompute((textureWidth + 31) / 32, (textureHeight + 31) / 32, 1);
        glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);


        glBlitFramebuffer(0, 0, textureWidth, textureHeight, 0, 0, textureWidth, textureHeight, GL_COLOR_BUFFER_BIT, GL_LINEAR);

        glfwSwapBuffers(window);

        auto frameEndTime = std::chrono::high_resolution_clock::now();
        dt = std::chrono::duration<float>(frameEndTime - frameStartTime).count();
        frameStartTime = frameEndTime;

        std::cout << std::format("dt: {:.2f}ms ({:.1f}F/s)\n", dt * 1000, 1.0f / dt);
    }

    glfwDestroyWindow(window);
    glfwTerminate();


    return 0;
}
