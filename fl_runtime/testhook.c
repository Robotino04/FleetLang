#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef __STDC_IEC_559__
    #error This C compiler doesn't use standard-sized (IEEE754) floats and doubles. Behaviour is unknown.
#endif

typedef int (*main_fn)(int, char**, char**);
typedef int (*libc_start_main_fn)(
    main_fn main, int argc, char** ubp_av, void (*init)(void), void (*fini)(void), void (*rtld_fini)(void), void* stack_end
);

__attribute__((constructor)) static void fleetc_test_entry(void) {
    const char* pipe_path = getenv("FLEETC_TEST_PIPE");
    if (!pipe_path) {
        fprintf(stderr, "FLEETC_TEST_PIPE not set\n");
        exit(2);
    }
    const char* result_type = getenv("FLEETC_TEST_TYPE");
    if (!pipe_path) {
        fprintf(stderr, "FLEETC_TEST_TYPE not set\n");
        exit(2);
    }

    int fd = open(pipe_path, O_WRONLY);
    if (fd < 0) {
        perror("open pipe");
        exit(3);
    }

    // Call fleet_main
    void* fleet_main = dlsym(RTLD_DEFAULT, "fleet_main");
    if (!fleet_main) {
        fprintf(stderr, "fleet_main not found\n");
        close(fd);
        exit(4);
    }
    void (*initialize_fleet)(void) = dlsym(RTLD_DEFAULT, "initialize_fleet");
    if (!initialize_fleet) {
        fprintf(stderr, "initialize_fleet not found\n");
        close(fd);
        exit(5);
    }
    void (*deinitialize_fleet)(void) = dlsym(RTLD_DEFAULT, "deinitialize_fleet");
    if (!deinitialize_fleet) {
        fprintf(stderr, "deinitialize_fleet not found\n");
        close(fd);
        exit(6);
    }

    initialize_fleet();

    int write_status = -1;
    if (strcmp("i8", result_type) == 0) {
        int8_t (*fleet_main_typed)(void) = fleet_main;
        int8_t result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else if (strcmp("i16", result_type) == 0) {
        int16_t (*fleet_main_typed)(void) = fleet_main;
        int16_t result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else if (strcmp("i32", result_type) == 0) {
        int32_t (*fleet_main_typed)(void) = fleet_main;
        int32_t result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else if (strcmp("i64", result_type) == 0) {
        int64_t (*fleet_main_typed)(void) = fleet_main;
        int64_t result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else if (strcmp("f32", result_type) == 0) {
        float (*fleet_main_typed)(void) = fleet_main;
        float result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else if (strcmp("f64", result_type) == 0) {
        double (*fleet_main_typed)(void) = fleet_main;
        double result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else if (strcmp("bool", result_type) == 0) {
        bool (*fleet_main_typed)(void) = fleet_main;
        bool result = fleet_main_typed();
        write_status = write(fd, &result, sizeof(result));
    }
    else {
        fprintf(stderr, "Invalid FLEETC_TEST_TYPE value: %s\n", result_type);
        exit(7);
    }

    deinitialize_fleet();

    if (write_status < 0) {
        fprintf(stderr, "Write failed");
        exit(8);
    }

    close(fd);
    
    fflush(stdout);
    fflush(stderr);
    _exit(0); // Ensure the real main() never runs
}
