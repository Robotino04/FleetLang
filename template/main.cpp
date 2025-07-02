#include "fl_runtime.h"
#include <fstream>
#include <iostream>
#include <vector>

std::vector<char> readSPV(const std::string& filename) {
    std::ifstream file(filename, std::ios::ate | std::ios::binary);
    if (!file.is_open()) {
        std::cerr << "Fatal : Unable to open shader file '" << filename << "'\n";
        std::exit(-1);
    }
    size_t size = file.tellg();
    std::vector<char> buffer(size);
    file.seekg(0);
    file.read(buffer.data(), size);
    return buffer;
}


int main() {
    fl_runtime_init();

    const std::string filename = "compute.comp.spv";
    std::cout << "Loading ShaderModule \"" << filename << "\"\n";
    auto shaderCode = readSPV(filename);

    constexpr int SIZE = 3;

    // ----- Fleet -----
    int fleet_a[SIZE] = {1, 2, 3};
    int fleet_b[SIZE] = {4, 5, 6};
    int fleet_c[SIZE] = {0};
    // ----- Fleet -----

    std::cout << "Allocating buffers A, B and C on GPU\n";

    fl_runtime_allocate_gpu_backing(&fleet_a, sizeof(fleet_a));
    fl_runtime_allocate_gpu_backing(&fleet_b, sizeof(fleet_b));
    fl_runtime_allocate_gpu_backing(&fleet_c, sizeof(fleet_c));


    std::cout << "Copying Host -> Device\n";
    fl_runtime_copy_to_backing(&fleet_a);
    fl_runtime_copy_to_backing(&fleet_b);

    void* buffers[SIZE] = {&fleet_a, &fleet_b, &fleet_c};
    fl_runtime_bind_buffers(&buffers, SIZE);

    fl_runtime_dispatch_shader(SIZE, reinterpret_cast<uint32_t*>(shaderCode.data()), shaderCode.size());

    std::cout << "Copying Device -> Host\n";
    fl_runtime_copy_from_backing(&fleet_c);

    // ----- Fleet -----
    for (int i = 0; i < SIZE; i++) {
        std::cout << fleet_c[i] << " ";
    }
    std::cout << "\n";
    // ----- Fleet -----

    fl_runtime_free_gpu_backing(&fleet_a);
    fl_runtime_free_gpu_backing(&fleet_b);
    fl_runtime_free_gpu_backing(&fleet_c);

    fl_runtime_deinit();
}
