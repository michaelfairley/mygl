FILTER=

build:
	cargo build --release

test: build
ifeq ($(FILTER),)
	cd cts_build && DYLD_LIBRARY_PATH=../target/release ./external/openglcts/modules/cts-runner --type=es32
else
	cd cts_build && DYLD_LIBRARY_PATH=../target/release ./external/openglcts/modules/glcts -n $(FILTER)
endif

debug: build
ifeq ($(FILTER),)
	cd cts_build && lldb -s ../lldb-setup ./external/openglcts/modules/cts-runner -- --type=es32
else
	cd cts_build && lldb -s ../lldb-setup ./external/openglcts/modules/glcts -- -n $(FILTER)
endif

prepare_cts:
	mkdir -p cts_build
	! [ ! -L cts_build/gl_cts ] || ln -s ../VK-GL-CTS/external/openglcts cts_build/gl_cts
	cd cts_build && cmake ../VK-GL-CTS/ -DDEQP_TARGET=mygl -DGLCTS_GTF_TARGET=gles32 -DCMAKE_BUILD_TYPE=Debug
	cd cts_build && cmake --build .
