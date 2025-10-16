MAKEFLAGS += -r

.PHONY: all clean

CFLAGS := \
	-Wreturn-stack-address \
	-fsanitize=undefined \
	-fsanitize=address \
	-fstack-protector-all \
	-fsanitize-address-use-after-return=always \
	-fno-omit-frame-pointer \
	-g \
	-I../fl_runtime/

CXXFLAGS := $(CFLAGS)

LDFLAGS := \
	-g \
	-rdynamic \
	-lvulkan \
	-lraylib


CXX := clang++
CC := clang

EXAMPLES := raytracing.fl mandelbrot_set.fl hello_world.fl
FL_OBJECTS := $(EXAMPLES:.fl=.o)
C_OBJECTS := $(EXAMPLES:.fl=_c.o)

GENERATED_C := $(EXAMPLES:.fl=.c)
GENERATED_LLVM := $(EXAMPLES:.fl=.opt.ll) $(EXAMPLES:.fl=.unopt.ll)

FL_BINS := $(EXAMPLES:.fl=_out)
C_BINS := $(EXAMPLES:.fl=_c_out)

all: $(FL_BINS) $(C_BINS)

clean:
	rm -f $(FL_OBJECTS) $(GENERATED_C) $(GENERATED_LLVM) $(FL_BINS) $(C_BINS) native_helpers.o


native_helpers.o: native_helpers.c
	$(CC) $(CFLAGS) -c native_helpers.c -o native_helpers.o

%.o: %.fl ../fl_runtime/fl_runtime_declarations.bc
	cargo run -- $< -o $@
	
$(GENERATED_C): %.c: %.o
	# a fake rule to let Make know how to get the .c files
	# FleetC currently produces both for a single invocation 
	touch $@

%_c.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%_out: %.o ../fl_runtime/fl_runtime.o native_helpers.o
	$(CXX) $(LDFLAGS) $(CXXFLAGS) $^ -o $@

../fl_runtime/%:
	$(MAKE) -C $(dir $@) $(notdir $@)
