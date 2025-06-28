#include <iostream>
#include <vector>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include "glad/include/glad/glad.h"

const char* shaderSrc = R"(
#version 430
layout(local_size_x = 1) in;

// https://github.com/Darkyenus/glsl4idea/issues/175
#extension GL_EXT_shader_explicit_arithmetic_types         : enable

layout(std430, binding = 0) buffer InputA { int fleet_a[]; };
layout(std430, binding = 1) buffer InputB { int fleet_b[]; };
layout(std430, binding = 2) buffer Output { int fleet_c[]; };

void main() {
    uint fleet_i = gl_GlobalInvocationID.x;
    {
        ((((fleet_c)[fleet_i])) = (((((fleet_a)[fleet_i])) + (((fleet_b)[fleet_i])))));
    }
}
)";

GLuint createComputeProgram() {
    GLuint shader = glCreateShader(GL_COMPUTE_SHADER);
    glShaderSource(shader, 1, &shaderSrc, nullptr);
    glCompileShader(shader);

    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (!status) {
        char log[512];
        glGetShaderInfoLog(shader, 512, nullptr, log);
        std::cerr << "Shader compile error:\n" << log << "\n";
        std::exit(1);
    }

    GLuint program = glCreateProgram();
    glAttachShader(program, shader);
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &status);
    if (!status) {
        char log[512];
        glGetProgramInfoLog(program, 512, nullptr, log);
        std::cerr << "Link error:\n" << log << "\n";
        std::exit(1);
    }

    return program;
}

GLuint createSSBO(const std::vector<int>& data, GLuint binding) {
    GLuint ssbo;
    glGenBuffers(1, &ssbo);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, binding, ssbo);
    glBufferData(GL_SHADER_STORAGE_BUFFER, data.size() * sizeof(int), data.data(), GL_STATIC_DRAW);
    return ssbo;
}

int main(void) {
    glfwInit();
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE); // no visible window
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    GLFWwindow* window = glfwCreateWindow(1, 1, "", nullptr, nullptr);
    glfwMakeContextCurrent(window);
    gladLoadGL();

    std::vector<int> A = {1, 2, 3};
    std::vector<int> B = {4, 5, 6};
    std::vector<int> C(3);

    GLuint ssboA = createSSBO(A, 0);
    GLuint ssboB = createSSBO(B, 1);

    GLuint ssboC;
    glGenBuffers(1, &ssboC);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, ssboC);
    glBufferData(GL_SHADER_STORAGE_BUFFER, C.size() * sizeof(int), nullptr, GL_STATIC_READ);

    GLuint program = createComputeProgram();
    glUseProgram(program);
    glDispatchCompute(GLuint(C.size()), 1, 1);
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);

    glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0, C.size() * sizeof(int), C.data());

    for (int v : C) {
        std::cout << v << " ";
    }
    std::cout << "\n";

    // Cleanup
    glDeleteBuffers(1, &ssboA);
    glDeleteBuffers(1, &ssboB);
    glDeleteBuffers(1, &ssboC);
    glDeleteProgram(program);
    glfwDestroyWindow(window);
    glfwTerminate();
}
