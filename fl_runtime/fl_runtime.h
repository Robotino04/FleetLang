#ifdef __cplusplus
    #include <cstdint>

extern "C" {
#else
    #include <stdint.h>
#endif

void fl_runtime_init(void);
void fl_runtime_deinit(void);

void fl_runtime_allocate_gpu_backing(void* buffer, uint64_t size);
void fl_runtime_free_gpu_backing(void* buffer);

void fl_runtime_copy_to_backing(void* buffer);
void fl_runtime_copy_from_backing(void* buffer);

void fl_runtime_bind_buffers(void* (*buffers)[], uint64_t size);
void fl_runtime_dispatch_shader(uint64_t shader_dispatch_size, const uint32_t* shader_code, uint64_t shader_code_size);

#ifdef FL_RUNTIME_DECLARATION_AS_BITCODE
void* dont_delete_my_functions_please[] = {
    (void*)&fl_runtime_init,
    (void*)&fl_runtime_deinit,
    (void*)&fl_runtime_deinit,
    (void*)&fl_runtime_allocate_gpu_backing,
    (void*)&fl_runtime_free_gpu_backing,
    (void*)&fl_runtime_copy_to_backing,
    (void*)&fl_runtime_copy_from_backing,
    (void*)&fl_runtime_bind_buffers,
    (void*)&fl_runtime_dispatch_shader,
};
#endif

#ifdef __cplusplus
}
#endif
