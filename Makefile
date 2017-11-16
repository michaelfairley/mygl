FILTER=

build:
	cargo build --release

test: build
ifeq ($(FILTER),)
	cd cts_build/external/openglcts/modules && DYLD_LIBRARY_PATH=../../../../target/release ./cts-runner --type=es32
else
	cd cts_build/external/openglcts/modules && DYLD_LIBRARY_PATH=../../../../target/release ./glcts -n $(FILTER)
endif

debug: build
ifeq ($(FILTER),)
	cd cts_build/external/openglcts/modules && lldb -s ../../../../lldb-setup ./cts-runner -- --type=es32
else
	cd cts_build/external/openglcts/modules && lldb -s ../../../../lldb-setup ./glcts -- -n $(FILTER)
endif

prepare_cts:
	mkdir -p cts_build
	cd cts_build && cmake ../VK-GL-CTS/ -DDEQP_TARGET=mygl -DGLCTS_GTF_TARGET=gles32 -DCMAKE_BUILD_TYPE=Debug
	cd cts_build && cmake --build .

good: build
	cd cts_build/external/openglcts/modules && DYLD_LIBRARY_PATH=../../../../target/release ./glcts -n dEQP-GLES31.functional.compute.basic.*
	cd cts_build/external/openglcts/modules && DYLD_LIBRARY_PATH=../../../../target/release ./glcts -n dEQP-GLES31.info.*
	cd cts_build/external/openglcts/modules && DYLD_LIBRARY_PATH=../../../../target/release ./glcts -n dEQP-GLES31.functional.shaders.builtin_var.*
	cd cts_build/external/openglcts/modules && DYLD_LIBRARY_PATH=../../../../target/release ./glcts -n dEQP-GLES31.functional.shaders.builtin_functions.common.*.*_compute
