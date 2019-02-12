FILTER=dEQP-GLES3.functional.draw.draw_elements.points.single_attribute
FILTER=dEQP-GLES3.functional.draw.draw_arrays.*

FAST=--deqp-log-images=disable --deqp-log-shader-sources=disable --deqp-log-flush=disable

GOOD= \
dEQP-GLES3.functional.prerequisite.* \
dEQP-GLES3.functional.color_clear.* \
dEQP-GLES31.functional.compute.basic.* \
dEQP-GLES31.info.* \
dEQP-GLES31.functional.shaders.builtin_var.* \
dEQP-GLES31.functional.shaders.builtin_functions.*.*.*_compute \
dEQP-GLES3.functional.rasterization.primitives.* \
dEQP-GLES3.functional.rasterization.culling.* \
dEQP-GLES3.functional.draw.draw_arrays.* \
dEQP-GLES3.functional.draw.draw_elements.* \
dEQP-GLES3.functional.draw.draw_arrays_instanced.* \
dEQP-GLES3.functional.draw.draw_elements_instanced.* \
dEQP-GLES3.functional.fragment_ops.depth.* \
dEQP-GLES3.functional.fragment_ops.stencil.* \
dEQP-GLES3.functional.fragment_ops.depth_stencil.* \
dEQP-GLES3.functional.clipping.point.point_* \
dEQP-GLES3.functional.transform_feedback.*

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
	ruby fail_image.rb

tests:
	cargo test

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

good: tests $(GOOD)

$(GOOD): build
	cd cts_build/external/openglcts/modules && ./glcts $(FAST) -n $@ && (! grep 'StatusCode="Fail"' TestResults.qpa > /dev/null)
