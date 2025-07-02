#include <cstdint>

extern "C" {
void fl_runtime_init();
void fl_runtime_deinit();

void fl_runtime_allocate_gpu_backing(void* buffer, uint64_t size);
void fl_runtime_free_gpu_backing(void* buffer);

void fl_runtime_copy_to_backing(void* buffer);
void fl_runtime_copy_from_backing(void* buffer);

void fl_runtime_bind_buffers(void* (*buffers)[], uint64_t size);
void fl_runtime_dispatch_shader(uint64_t shader_dispatch_size, uint32_t* shader_code, uint64_t shader_code_size);
}
