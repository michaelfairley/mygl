A software implementation of OpenGL ES 3.2 (and EGL 1.5) written in Rust.

Currently working:
- Compute shaders
- Vertex processing (including transform feedback)
- Rasterization
- Fragment processing (including depth and stencil testing)

Major things still to do:
- Blending
- Non-default framebuffers
- Texture sampling
- Geometry and tesselation shaders
- Reorganize threading model to more accurately reflect the typical GPU/driver divide
