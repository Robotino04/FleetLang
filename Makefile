.PHONY: run_llvm run_c all clean hook_c hook_llvm

CFLAGS= \
	-Wreturn-stack-address \
	-fsanitize=undefined \
	-fsanitize=address \
	-fstack-protector-all \
	-fsanitize-address-use-after-return=always \
	-fno-omit-frame-pointer

all: out_llvm out_c

clean:
	rm -f out_llvm out_c.o out_c c_out.log llvm_out.log out_c2

out_llvm: output.o fl_runtime/fl_runtime.o
	clang++ $(CFLAGS) output.o fl_runtime/fl_runtime.o -lvulkan -o out_llvm -rdynamic

out_c.o: out.c fl_runtime/fl_runtime.h
	clang $(CFLAGS) -c out.c -o out_c.o -Ifl_runtime/

out_c: out_c.o fl_runtime/fl_runtime.o
	clang++ $(CFLAGS) out_c.o fl_runtime/fl_runtime.o -lvulkan -o out_c -rdynamic

hook_llvm: out_llvm fl_runtime/testhook.so
	set -e; \
	PIPE=$$(mktemp -u); \
	mkfifo $$PIPE; \
	export FLEETC_TEST_PIPE=$$PIPE; \
	export FLEETC_TEST_TYPE=i64; \
	cat $$PIPE | xxd -R always> llvm_out.log & \
	LD_PRELOAD=./fl_runtime/testhook.so ./out_llvm; \
	rm -f $$PIPE; \
	cat llvm_out.log | cat


hook_c: out_c fl_runtime/testhook.so
	set -e; \
	PIPE=$$(mktemp -u); \
	mkfifo $$PIPE; \
	export FLEETC_TEST_PIPE=$$PIPE; \
	export FLEETC_TEST_TYPE=i64; \
	cat $$PIPE | xxd -R always > c_out.log & \
	LD_PRELOAD=./fl_runtime/testhook.so ./out_c; \
	rm -f $$PIPE; \
	cat c_out.log | cat

out_c2.o: out2.c fl_runtime/fl_runtime.h
	clang $(CFLAGS) -c out2.c -o out_c2.o -Ifl_runtime/

out_c2: out_c2.o fl_runtime/fl_runtime.o
	clang++ $(CFLAGS) out_c2.o fl_runtime/fl_runtime.o -lvulkan -o out_c2 -rdynamic

run_llvm: out_llvm
	./out_llvm

run_c: out_c
	./out_c
