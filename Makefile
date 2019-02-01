FILTER=

FAST=--deqp-log-images=disable --deqp-log-shader-sources=disable --deqp-log-flush=disable

export LD_LIBRARY_PATH=../../../../target/release
export DYLD_LIBRARY_PATH=../../../../target/release

build:
	cargo build --release


test: build
ifeq ($(FILTER),)
	cd cts_build/external/openglcts/modules && ./cts-runner --type=es32
else
	cd cts_build/external/openglcts/modules && ./glcts -n $(FILTER)
endif

debug: build
ifeq ($(FILTER),)
	cd cts_build/external/openglcts/modules && lldb -s ../../../../lldb-setup ./cts-runner -- --type=es32
else
	cd cts_build/external/openglcts/modules && lldb -s ../../../../lldb-setup ./glcts -- -n $(FILTER)
endif

perf: build
	cd cts_build/external/openglcts/modules && perf record --call-graph dwarf ./glcts -n $(FILTER) $(FAST)
	perf script -i cts_build/external/openglcts/modules/perf.data | stackcollapse-perf.pl | flamegraph.pl > flame.svg


prepare_cts:
	mkdir -p cts_build
	cd cts_build && cmake ../VK-GL-CTS/ -DDEQP_TARGET=mygl -DGLCTS_GTF_TARGET=gles32 -DCMAKE_BUILD_TYPE=Release
	cd cts_build && cmake --build . -j 12

good: build
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n dEQP-GLES31.functional.compute.basic.*
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n dEQP-GLES31.info.*
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n dEQP-GLES31.functional.shaders.builtin_var.*
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n dEQP-GLES31.functional.shaders.builtin_functions.*.*.*_compute
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n dEQP-GLES3.functional.transform_feedback.*
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n dEQP-GLES3.functional.draw.draw_arrays.*
