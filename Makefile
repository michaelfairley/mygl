build:
	cargo build

test: build
	cd cts_build && DYLD_LIBRARY_PATH=../target/debug ./external/openglcts/modules/cts-runner --type=es32 --verbose

prepare_cts:
	mkdir -p cts_build
	! [ ! -L cts_build/gl_cts ] || ln -s ../VK-GL-CTS/external/openglcts cts_build/gl_cts
	cd cts_build && cmake ../VK-GL-CTS/ -DDEQP_TARGET=mygl -DGLCTS_GTF_TARGET=gles32 -DCMAKE_BUILD_TYPE=Debug
	cd cts_build && cmake --build .
