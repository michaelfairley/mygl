#![allow(non_snake_case)]
use std::os::raw::*;
use std::sync::mpsc;
use std::thread;
use std::ptr;
use std::ffi::{CStr};
use std::collections::HashMap;
use std::sync::{Arc,Barrier,RwLock};
use std::cell::{RefCell,Ref,Cell};
use std::mem;
use std::cmp;
use string_cache::DefaultAtom as Atom;

use types::*;
use consts::*;

use egl::{self,Config,Surface};
use glsl;

use draw;

#[cfg(feature = "trace_gl")]
use trace::trace;
#[cfg(feature = "trace_gl")]
trace::init_depth_var!();

// TODO: threadpool
// TODO: split stuff off onto server
// TODO: figure out when to use GL* types and when to use rust types

pub type Rect = (GLint, GLint, GLsizei, GLsizei);
pub type ColorMask = (bool, bool, bool, bool);

const MAX_SHADER_STORAGE_BUFFER_BINDINGS: usize = 4;
const MAX_UNIFORM_BUFFER_BINDINGS: usize = 72;
const MAX_ATOMIC_COUNTER_BUFFER_BINDINGS: usize = 1;
const MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS: usize = 4;
const MAX_IMAGE_UNITS: usize = 8;
const MAX_VERTEX_ATTRIBS: usize = 32; // TODO: try 16 after error handling is better
const MAX_CUBE_MAP_TEXTURE_SIZE: usize = 2048;
const MAX_TEXTURE_SIZE: usize = 2048;
const MAX_3D_TEXTURE_SIZE: usize = 2048;
const MAX_RENDERBUFFER_SIZE: usize = 2048;
const MAX_ARRAY_TEXTURE_LAYERS: usize = 256;



#[derive(Debug)]
pub struct Context {
  pub config: &'static Config,

  pub draw_surface: *mut Surface,
  pub read_surface: *mut Surface,

  tx: mpsc::Sender<Command>,

  pub scissor_test: bool,
  pub scissor: Rect,

  pub color_mask: ColorMask,
  pub depth_mask: bool,
  pub stencil_mask_front: u32,
  pub stencil_mask_back: u32,

  pub stencil_test: bool,
  pub stencil_func_front: (CmpFunc, GLuint, GLuint),
  pub stencil_func_back: (CmpFunc, GLuint, GLuint),
  pub stencil_op_front: (GLenum, GLenum, GLenum),
  pub stencil_op_back: (GLenum, GLenum, GLenum),

  pub depth_test: bool,
  pub depth_func: CmpFunc,

  pub blend: bool,

  pub viewport: Rect,
  pub depth_range: (GLfloat, GLfloat),

  pub clear_color: (GLfloat, GLfloat, GLfloat, GLfloat),
  pub clear_depth: GLfloat,
  pub clear_stencil: GLuint,

  // TODO: move to server?
  pub buffers: HashMap<GLuint, Option<Vec<u8>>>, // TODO: replace with pointer
  pub array_buffer: GLuint,
  pub element_array_buffer: GLuint,
  pub pixel_pack_buffer: GLuint,
  pub pixel_unpack_buffer: GLuint,
  pub uniform_buffer: GLuint,
  pub transform_feedback_buffer: GLuint,
  pub dispatch_indirect_buffer: GLuint,
  pub copy_read_buffer: GLuint,
  pub copy_write_buffer: GLuint,
  pub draw_indirect_buffer: GLuint,
  pub atomic_counter_buffer: GLuint,
  pub shader_storage_buffer: GLuint,
  pub indexed_shader_storage_buffer: [BufferBinding; MAX_SHADER_STORAGE_BUFFER_BINDINGS],
  pub indexed_uniform_buffer: Vec<BufferBinding>,
  pub indexed_atomic_counter_buffer: [BufferBinding; MAX_ATOMIC_COUNTER_BUFFER_BINDINGS],
  pub indexed_transform_feedback_buffer: [BufferBinding; MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS],

  pub vertex_array: GLuint,
  pub vertex_arrays: HashMap<GLuint, Option<VertexArray>>,
  pub current_vertex_attrib: [[GLfloat; 4]; MAX_VERTEX_ATTRIBS],

  pub culling: bool,
  pub cull_face: GLenum,
  pub front_face_cw: bool,

  pub programs: HashMap<GLuint, Program>,
  pub shaders: HashMap<GLuint, Arc<Shader>>,
  pub program: GLuint,

  pub textures: HashMap<GLuint, Option<Arc<Texture>>>,
  pub texture_2d: GLuint,
  pub images: [GLuint; MAX_IMAGE_UNITS],

  pub framebuffers: HashMap<GLuint, Option<Arc<RwLock<Framebuffer>>>>,
  pub draw_framebuffer: GLuint,
  pub read_framebuffer: GLuint,

  pub transform_feedbacks: HashMap<GLuint, Option<TransformFeedback>>,
  pub transform_feedback: GLuint,
  pub transform_feedback_capture: Option<GLenum>,
  pub transform_feedback_paused: bool,

  pub queries: HashMap<GLuint, Option<Query>>,
  pub query: Option<(GLuint, GLenum)>,

  pub line_width: GLfloat,
  pub primitive_restart_fixed_index: bool,
  pub polygon_offset: (GLfloat, GLfloat),
  pub polygon_offset_fill: bool,
  pub rasterizer_discard: bool,
  pub sample_alpha_to_coverage: bool,
  pub sample_coverage_enabled: bool,
  pub sample_coverage: (GLfloat, bool),
  pub sample_mask: bool,
  pub dither: bool,
}

impl Context {
  pub fn new(config: &'static Config) -> Self {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
      let mut server = Server::new(rx);
      server.run();
    });

    let mut context = Self{
      config,
      tx,

      draw_surface: egl::EGL_NO_SURFACE,
      read_surface: egl::EGL_NO_SURFACE,

      scissor_test: false,
      scissor: (0, 0, 0, 0),

      color_mask: (true, true, true, true),
      depth_mask: true,
      stencil_mask_front: 0,
      stencil_mask_back: 0,

      stencil_test: false,
      stencil_func_front: (CmpFunc::Always, 0, GLuint::max_value()),
      stencil_func_back: (CmpFunc::Always, 0, GLuint::max_value()),
      stencil_op_front: (GL_KEEP, GL_KEEP, GL_KEEP),
      stencil_op_back: (GL_KEEP, GL_KEEP, GL_KEEP),

      depth_test: false,
      depth_func: CmpFunc::Less,

      blend: false,

      viewport: (0, 0, 0, 0),
      depth_range: (0.0, 1.0),

      clear_color: (0.0, 0.0, 0.0, 0.0),
      clear_depth: 1.0,
      clear_stencil: 0,

      buffers: HashMap::new(),
      array_buffer: 0,
      element_array_buffer: 0,
      pixel_pack_buffer: 0,
      pixel_unpack_buffer: 0,
      uniform_buffer: 0,
      transform_feedback_buffer: 0,
      dispatch_indirect_buffer: 0,
      copy_read_buffer: 0,
      copy_write_buffer: 0,
      draw_indirect_buffer: 0,
      atomic_counter_buffer: 0,
      shader_storage_buffer: 0,
      indexed_shader_storage_buffer: [BufferBinding::zero(); MAX_SHADER_STORAGE_BUFFER_BINDINGS],
      indexed_uniform_buffer: vec![BufferBinding::zero(); MAX_UNIFORM_BUFFER_BINDINGS],
      indexed_atomic_counter_buffer: [BufferBinding::zero(); MAX_ATOMIC_COUNTER_BUFFER_BINDINGS],
      indexed_transform_feedback_buffer: [BufferBinding::zero(); MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS],

      vertex_array: 0,
      vertex_arrays: HashMap::new(),
      current_vertex_attrib: [[0.0, 0.0, 0.0, 1.0]; MAX_VERTEX_ATTRIBS],

      culling: false,
      cull_face: GL_BACK,
      front_face_cw: false,

      programs: HashMap::new(),
      shaders: HashMap::new(),
      program: 0,

      textures: HashMap::new(),
      texture_2d: 0,
      images: [0; MAX_IMAGE_UNITS],

      framebuffers: HashMap::new(),
      draw_framebuffer: 0,
      read_framebuffer: 0,

      transform_feedbacks: HashMap::new(),
      transform_feedback: 0,
      transform_feedback_capture: None,
      transform_feedback_paused: false,

      queries: HashMap::new(),
      query: None,

      line_width: 1.0,
      primitive_restart_fixed_index: false,
      polygon_offset: (0.0, 0.0),
      polygon_offset_fill: false,
      rasterizer_discard: false,
      sample_alpha_to_coverage: false,
      sample_coverage_enabled: false,
      sample_coverage: (1.0, false),
      sample_mask: false,
      dither: true,
    };

    context.vertex_arrays.insert(0, Some(VertexArray::new()));

    context
  }

  pub fn set_surfaces(&mut self, draw: *mut Surface, read: *mut Surface) {
    self.draw_surface = draw;
    self.read_surface = read;
    self.tx.send(Command::SetSurfaces(unsafe{ draw.as_mut() }, unsafe{ read.as_mut() })).unwrap();
  }

  fn buffer_target(&self, target: GLenum) -> GLuint {
    match target {
      GL_ARRAY_BUFFER => self.array_buffer,
      GL_ELEMENT_ARRAY_BUFFER => self.element_array_buffer,
      GL_PIXEL_PACK_BUFFER => self.pixel_pack_buffer,
      GL_PIXEL_UNPACK_BUFFER => self.pixel_unpack_buffer,
      GL_UNIFORM_BUFFER => self.uniform_buffer,
      GL_TRANSFORM_FEEDBACK_BUFFER => self.transform_feedback_buffer,
      GL_DISPATCH_INDIRECT_BUFFER => self.dispatch_indirect_buffer,
      GL_COPY_READ_BUFFER => self.copy_read_buffer,
      GL_COPY_WRITE_BUFFER => self.copy_write_buffer,
      GL_DRAW_INDIRECT_BUFFER => self.draw_indirect_buffer,
      GL_ATOMIC_COUNTER_BUFFER => self.atomic_counter_buffer,
      GL_SHADER_STORAGE_BUFFER => self.shader_storage_buffer,
      x => unimplemented!("{:x}", x),
    }
  }

  fn buffer_target_mut(&mut self, target: GLenum) -> &mut GLuint {
    match target {
      GL_ARRAY_BUFFER => &mut self.array_buffer,
      GL_ELEMENT_ARRAY_BUFFER => &mut self.element_array_buffer,
      GL_PIXEL_PACK_BUFFER => &mut self.pixel_pack_buffer,
      GL_PIXEL_UNPACK_BUFFER => &mut self.pixel_unpack_buffer,
      GL_UNIFORM_BUFFER => &mut self.uniform_buffer,
      GL_TRANSFORM_FEEDBACK_BUFFER => &mut self.transform_feedback_buffer,
      GL_DISPATCH_INDIRECT_BUFFER => &mut self.dispatch_indirect_buffer,
      GL_COPY_READ_BUFFER => &mut self.copy_read_buffer,
      GL_COPY_WRITE_BUFFER => &mut self.copy_write_buffer,
      GL_DRAW_INDIRECT_BUFFER => &mut self.draw_indirect_buffer,
      GL_ATOMIC_COUNTER_BUFFER => &mut self.atomic_counter_buffer,
      GL_SHADER_STORAGE_BUFFER => &mut self.shader_storage_buffer,
      x => unimplemented!("{:x}", x),
    }
  }

  fn indexed_buffer_target_mut(&mut self, target: GLenum, i: usize) -> &mut BufferBinding {
    match target {
      GL_SHADER_STORAGE_BUFFER => &mut self.indexed_shader_storage_buffer[i],
      GL_UNIFORM_BUFFER => &mut self.indexed_uniform_buffer[i],
      GL_ATOMIC_COUNTER_BUFFER => &mut self.indexed_atomic_counter_buffer[i],
      GL_TRANSFORM_FEEDBACK_BUFFER => &mut self.indexed_transform_feedback_buffer[i],
      x => unimplemented!("{:x}", x),
    }
  }

  fn texture_target(&self, target: GLenum) -> GLuint {
    match target {
      GL_TEXTURE_2D => self.texture_2d,
      x => unimplemented!("{:x}", x),
    }
  }

  fn texture_target_mut(&mut self, target: GLenum) -> &mut GLuint {
    match target {
      GL_TEXTURE_2D => &mut self.texture_2d,
      x => unimplemented!("{:x}", x),
    }
  }

  fn cap_mut(&mut self, cap: GLenum) -> &mut bool {
    match cap {
      GL_SCISSOR_TEST => &mut self.scissor_test,
      GL_PRIMITIVE_RESTART_FIXED_INDEX => &mut self.primitive_restart_fixed_index,
      GL_CULL_FACE => &mut self.culling,
      GL_POLYGON_OFFSET_FILL => &mut self.polygon_offset_fill,
      GL_RASTERIZER_DISCARD => &mut self.rasterizer_discard,
      GL_SAMPLE_ALPHA_TO_COVERAGE => &mut self.sample_alpha_to_coverage,
      GL_SAMPLE_COVERAGE => &mut self.sample_coverage_enabled,
      GL_SAMPLE_MASK => &mut self.sample_mask,
      GL_STENCIL_TEST => &mut self.stencil_test,
      GL_DEPTH_TEST => &mut self.depth_test,
      GL_BLEND => &mut self.blend,
      GL_DITHER => &mut self.dither,
      x => unimplemented!("{:x}", x),
    }
  }
}

pub fn current() -> &'static mut Context {
  unsafe { &mut *::egl::CONTEXT.with(|c| c.get()) }
}

struct Server {
  rx: mpsc::Receiver<Command>,

  draw_surface: Option<&'static mut Surface>,
  read_surface: Option<&'static mut Surface>,
}

impl Server {
  pub fn new(rx: mpsc::Receiver<Command>) -> Self {

    Self{
      rx,

      draw_surface: None,
      read_surface: None,
    }
  }

  fn run(&mut self) {
    for command in self.rx.iter() {
      match command {
        Command::SetSurfaces(draw, read) => {
          self.draw_surface = draw;
          self.read_surface = read;
        },
        Command::Clear(color, depth, stencil, scissor, color_mask, depth_mask, stencil_mask) => {
          let (x, y, width, height) = scissor;

          if let Some(color) = color {
            let draw = self.draw_surface.as_mut().unwrap();

            for y in y..y+height {
              for x in x..x+width {
                draw.set_pixel(x, y, color, color_mask);
              }
            }
          }

          if let Some(depth) = depth {
            if depth_mask {
              let draw = self.draw_surface.as_mut().unwrap();

              for y in y..y+height {
                for x in x..x+width {
                  draw.set_depth(x, y, depth);
                }
              }
            }
          }

          if let Some(stencil) = stencil {
            let draw = self.draw_surface.as_mut().unwrap();
            let stencil_mask = if stencil_mask == 0 { u32::max_value() } else { stencil_mask };

            for y in y..y+height {
              for x in x..x+width {
                draw.set_stencil(x, y, stencil & stencil_mask);
              }
            }
          }
        },
        Command::Finish(tx) => {
          tx.send(()).unwrap();
        },
        Command::Flush(tx) => {
          tx.send(()).unwrap();
        },
      }
    }
  }
}

enum Command {
  SetSurfaces(Option<&'static mut Surface>, Option<&'static mut Surface>),
  Clear(Option<(f32, f32, f32, f32)>, Option<f32>, Option<u32>, Rect, ColorMask, bool, u32),
  Finish(mpsc::Sender<()>),
  Flush(mpsc::Sender<()>),
}

#[derive(Debug)]
pub struct Program{
  pub shaders: Vec<Arc<Shader>>,
  pub ssbos: HashMap<GLuint, glsl::BlockInfo>,
  pub ubos: HashMap<GLuint, glsl::BlockInfo>,
  pub uniform_block_bindings: Vec<GLuint>,
  pub uniforms: Vec<glsl::UniformInfo>,
  pub uniform_values: Vec<glsl::interpret::Value>,
  pub atomic_counters: HashMap<GLuint, glsl::AtomicCounterInfo>,
  pub pending_transform_feedback: Option<(Vec<String>, bool)>,
  pub transform_feedback: Option<(Vec<String>, bool)>,
  pub attrib_locations: [Option<Atom>; MAX_VERTEX_ATTRIBS],
}

impl Program {
  pub fn new() -> Self {
    Self{
      shaders: vec![],
      ssbos: HashMap::new(),
      ubos: HashMap::new(),
      uniform_block_bindings: vec![],
      uniforms: vec![],
      uniform_values: vec![],
      atomic_counters: HashMap::new(),
      pending_transform_feedback: None,
      transform_feedback: None,
      attrib_locations: [None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None],
    }
  }
}

#[derive(Debug)]
pub struct Shader {
  pub type_: GLenum,
  pub source: RefCell<Vec<u8>>,
  pub compiled: RefCell<Option<Arc<glsl::Shader>>>,
  pub info_log: RefCell<String>,
}

impl Shader {
  pub fn new(type_: GLenum) -> Self {
    Self{
      type_,
      source: RefCell::new(vec![]),
      compiled: RefCell::new(None),
      info_log: RefCell::new(String::new()),
    }
  }
}

#[derive(Debug)]
pub struct Texture {
  pub width: usize,
  pub height: usize,
  pub buffer: Vec<u8>,

  pub mag_filter: Cell<GLenum>,
  pub min_filter: Cell<GLenum>,
}

#[derive(Debug)]
pub struct Framebuffer {
  color0: Option<Arc<Texture>>,
}

#[derive(Debug,Copy,Clone)]
pub struct BufferBinding {
  pub buffer: GLuint,
  pub offset: isize,
  pub size: usize,
}

impl BufferBinding{
  fn zero() -> Self {
    Self{
      buffer: 0,
      offset: 0,
      size: 0,
    }
  }
}

#[derive(Debug,Clone)]
pub struct TransformFeedback {
  pub offsets: [isize; MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS],
}

#[derive(Debug)]
pub struct Query {
  pub value: u32,
}

#[derive(Debug)]
pub struct VertexArray {
  pub attribs: [VertexAttrib; MAX_VERTEX_ATTRIBS],
  pub bindings: [VertexBinding; MAX_VERTEX_ATTRIBS],
}

impl VertexArray {
  fn new() -> Self {
    Self{
      attribs: [VertexAttrib::new(); MAX_VERTEX_ATTRIBS],
      bindings: [VertexBinding::new(); MAX_VERTEX_ATTRIBS],
    }
  }
}

#[derive(Debug,Copy,Clone)]
pub struct VertexAttrib {
  pub enabled: bool,
  pub size: GLint,
  pub type_: GLenum,
  pub normalized: bool,
  pub stride: GLsizei,
  pub pointer: *const c_void,
}

impl VertexAttrib {
  fn new() -> Self {
    Self{
      enabled: false,
      size: 4,
      type_: GL_FLOAT,
      normalized: false,
      stride: 0,
      pointer: ptr::null(),
    }
  }
}

#[derive(Debug,Copy,Clone)]
pub struct VertexBinding {
  pub divisor: GLuint,
  pub buffer: GLuint,
  pub stride: GLint,
  pub offset: isize,
}

impl VertexBinding {
  fn new() -> Self {
    Self{
      divisor: 0,
      buffer: 0,
      stride: 0,
      offset: 0,
    }
  }
}

#[derive(Debug,Copy,Clone)]
pub enum CmpFunc {
  LessEqual,
  GreaterEqual,
  Less,
  Greater,
  Equal,
  NotEqual,
  Always,
  Never,
}

impl CmpFunc {
  pub fn cmp_f32(self, old: f32, new: f32) -> bool {
    match self {
      CmpFunc::LessEqual => new <= old,
      CmpFunc::GreaterEqual => new >= old,
      CmpFunc::Less => new < old,
      CmpFunc::Greater => new > old,
      CmpFunc::Equal => new == old,
      CmpFunc::NotEqual => new != old,
      CmpFunc::Always => true,
      CmpFunc::Never => false,
    }
  }

  pub fn cmp_u32(self, old: u32, new: u32) -> bool {
    match self {
      CmpFunc::LessEqual => new <= old,
      CmpFunc::GreaterEqual => new >= old,
      CmpFunc::Less => new < old,
      CmpFunc::Greater => new > old,
      CmpFunc::Equal => new == old,
      CmpFunc::NotEqual => new != old,
      CmpFunc::Always => true,
      CmpFunc::Never => false,
    }
  }
}

impl From<GLenum> for CmpFunc {
  fn from(func: GLenum) -> Self {
    match func {
      GL_LESS => CmpFunc::Less,
      GL_LEQUAL => CmpFunc::LessEqual,
      GL_GREATER => CmpFunc::Greater,
      GL_GEQUAL => CmpFunc::GreaterEqual,
      GL_EQUAL => CmpFunc::Equal,
      GL_NOTEQUAL => CmpFunc::NotEqual,
      GL_ALWAYS => CmpFunc::Always,
      GL_NEVER => CmpFunc::Never,
      x => unimplemented!("{:?}", x),
    }
  }
}


#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
#[allow(unused_variables)]
pub extern "C" fn glActiveShaderProgram(pipeline: GLuint, program: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glActiveTexture(texture: GLenum) -> () {
  assert_eq!(texture, GL_TEXTURE0);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glAttachShader(program: GLuint, shader: GLuint) -> () {
  let current = current();

  let program = current.programs.get_mut(&program).unwrap();
  let shader = current.shaders.get(&shader).unwrap();

  program.shaders.push(Arc::clone(shader));
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBeginQuery(
  target: GLenum,
  id: GLuint,
) -> () {
  let current = current();

  if current.queries.get(&id).unwrap().is_none() {
    current.queries.insert(id, Some(Query{
      value: 0,
    }));
  }

  current.query = Some((id, target));
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBeginTransformFeedback(
  primitiveMode: GLenum,
) -> () {
  let current = current();

  current.transform_feedback_capture = Some(primitiveMode);
  current.transform_feedbacks.get_mut(&current.transform_feedback).unwrap().as_mut().unwrap().offsets = [0; MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS];
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindAttribLocation(program: GLuint, index: GLuint, name: *const GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindBuffer(target: GLenum, buffer: GLuint) -> () {
  let current = current();

  *current.buffer_target_mut(target) = buffer;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindBufferBase(
  target: GLenum,
  index: GLuint,
  buffer: GLuint,
) -> () {
  let current = current();

  let size = if buffer == 0 {
    0
  } else {
    current.buffers[&buffer].as_ref().unwrap().len()
  };

  let binding = BufferBinding{
    buffer: buffer,
    offset: 0,
    size: size,
  };

  *current.buffer_target_mut(target) = buffer;
  *current.indexed_buffer_target_mut(target, index as usize) = binding;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindBufferRange(
  target: GLenum,
  index: GLuint,
  buffer: GLuint,
  offset: GLintptr,
  size: GLsizeiptr,
) -> () {
  let current = current();

  let binding = BufferBinding{
    buffer: buffer,
    offset: offset,
    size: size as usize,
  };

  *current.buffer_target_mut(target) = buffer;
  *current.indexed_buffer_target_mut(target, index as usize) = binding;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindFramebuffer(
  target: GLenum,
  framebuffer: GLuint,
) -> () {
  let current = current();

  if framebuffer != 0 && current.framebuffers[&framebuffer].is_none() {
    current.framebuffers.insert(framebuffer, Some(Arc::new(RwLock::new(Framebuffer{
      color0: None,
    }))));
  }

  match target {
    GL_DRAW_FRAMEBUFFER => current.draw_framebuffer = framebuffer,
    GL_READ_FRAMEBUFFER => current.read_framebuffer = framebuffer,
    GL_FRAMEBUFFER => {
      current.draw_framebuffer = framebuffer;
      current.read_framebuffer = framebuffer;
    },
    x => unimplemented!("{:x}", x),
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindImageTexture(
  unit: GLuint,
  texture: GLuint,
  level: GLint,
  layered: GLboolean,
  layer: GLint,
  _access: GLenum,
  format: GLenum,
) -> () {
  assert_eq!(layered, GL_FALSE);
  assert_eq!(layer, 0);
  assert_eq!(level, 0);
  assert_eq!(format, GL_R32UI);

  let current = current();
  current.images[unit as usize] = texture;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindProgramPipeline(pipeline: GLuint) -> () {
  assert_eq!(pipeline, 0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindRenderbuffer(target: GLenum, renderbuffer: GLuint) -> () {
  assert_eq!(renderbuffer, 0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindSampler(unit: GLuint, sampler: GLuint) -> () {
  assert_eq!(sampler, 0);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindTexture(
  target: GLenum,
  texture: GLuint,
) -> () {
  let current = current();
  *current.texture_target_mut(target) = texture;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindTransformFeedback(
  target: GLenum,
  id: GLuint,
) -> () {
  let current = current();

  assert_eq!(target, GL_TRANSFORM_FEEDBACK);

  if id > 0 {
    if current.transform_feedbacks[&id].is_none() {
      current.transform_feedbacks.insert(id, Some(TransformFeedback{
        offsets: [0; MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS],
      }));
    }
  }

  current.transform_feedback = id;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindVertexArray(
  array: GLuint,
) -> () {
  current().vertex_array = array;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBindVertexBuffer(
  bindingindex: GLuint,
  buffer: GLuint,
  offset: GLintptr,
  stride: GLsizei,
) -> () {
  let current = current();

  let binding = &mut current.vertex_arrays.get_mut(&current.vertex_array).unwrap().as_mut().unwrap().bindings[bindingindex as usize];

  binding.buffer = buffer;
  binding.offset = offset;
  binding.stride = stride;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendBarrier() -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendColor(red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat) -> () {
  assert_eq!(red, 0.0);
  assert_eq!(green, 0.0);
  assert_eq!(blue, 0.0);
  assert_eq!(alpha, 0.0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendEquation(mode: GLenum) -> () {
  assert_eq!(mode, GL_FUNC_ADD);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendEquationSeparate(modeRGB: GLenum, modeAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendEquationSeparatei(buf: GLuint, modeRGB: GLenum, modeAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendEquationi(buf: GLuint, mode: GLenum) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendFunc(sfactor: GLenum, dfactor: GLenum) -> () {
  assert_eq!(sfactor, GL_ONE);
  assert_eq!(dfactor, GL_ZERO);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendFuncSeparate(sfactorRGB: GLenum, dfactorRGB: GLenum, sfactorAlpha: GLenum, dfactorAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendFuncSeparatei(buf: GLuint, srcRGB: GLenum, dstRGB: GLenum, srcAlpha: GLenum, dstAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlendFunci(buf: GLuint, src: GLenum, dst: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBlitFramebuffer(srcX0: GLint, srcY0: GLint, srcX1: GLint, srcY1: GLint, dstX0: GLint, dstY0: GLint, dstX1: GLint, dstY1: GLint, mask: GLbitfield, filter: GLenum) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBufferData(
  target: GLenum,
  size: GLsizeiptr,
  data: *const c_void,
  _usage: GLenum,
) -> () {
  let current = current();

  let vec = if data.is_null() {
    let mut v = Vec::with_capacity(size as usize);
    unsafe{ v.set_len(size as usize); }
    v
  } else {
    let slice = unsafe{ ::std::slice::from_raw_parts(data as *const u8, size as usize) };
    Vec::from(slice)
  };

  let name = current.buffer_target(target);
  current.buffers.insert(name, Some(vec));
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glBufferSubData(target: GLenum, offset: GLintptr, size: GLsizeiptr, data: *const c_void) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCheckFramebufferStatus(
  _target: GLenum,
) -> GLenum {
  // TODO

  GL_FRAMEBUFFER_COMPLETE
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "system" fn glClear(mask: GLbitfield) -> () {
  let current = current();

  let scissor = if current.scissor_test {
    current.scissor
  } else {
    let draw = unsafe{ current.draw_surface.as_ref() };
    let draw = draw.unwrap();

    (0, 0, draw.width, draw.height)
  };

  current.tx.send(Command::Clear(
    if (mask & GL_COLOR_BUFFER_BIT) > 0 { Some(current.clear_color) } else { None },
    if (mask & GL_DEPTH_BUFFER_BIT) > 0 { Some(current.clear_depth) } else { None },
    if (mask & GL_STENCIL_BUFFER_BIT) > 0 { Some(current.clear_stencil) } else { None },
    scissor,
    current.color_mask,
    current.depth_mask,
    current.stencil_mask_front,
  )).unwrap();
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearBufferfi(buffer: GLenum, drawbuffer: GLint, depth: GLfloat, stencil: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearBufferfv(buffer: GLenum, drawbuffer: GLint, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearBufferiv(buffer: GLenum, drawbuffer: GLint, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearBufferuiv(buffer: GLenum, drawbuffer: GLint, value: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearColor(
  red: GLfloat,
  green: GLfloat,
  blue: GLfloat,
  alpha: GLfloat,
) -> () {
  current().clear_color = (red, green, blue, alpha);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearDepthf(
  d: GLfloat,
) -> () {
  current().clear_depth = d;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClearStencil(
  s: GLint,
) -> () {
  current().clear_stencil = s as GLuint;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glClientWaitSync(sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glColorMask(red: GLboolean, green: GLboolean, blue: GLboolean, alpha: GLboolean) -> () {
  current().color_mask = (
    red == GL_TRUE,
    green == GL_TRUE,
    blue == GL_TRUE,
    alpha == GL_TRUE,
  )
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glColorMaski(index: GLuint, r: GLboolean, g: GLboolean, b: GLboolean, a: GLboolean) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCompileShader(shader: GLuint) -> () {
  let current = current();
  let shader = current.shaders.get_mut(&shader).unwrap();

  let result = glsl::compile(&shader.source.borrow(), shader.type_);

  *shader.info_log.borrow_mut() = result.as_ref().err().map(|e| e.clone()).unwrap_or_else(String::new);
  *shader.compiled.borrow_mut() = result.ok().map(Arc::new);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCompressedTexImage2D(target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, border: GLint, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCompressedTexImage3D(target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCompressedTexSubImage2D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCompressedTexSubImage3D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCopyBufferSubData(readTarget: GLenum, writeTarget: GLenum, readOffset: GLintptr, writeOffset: GLintptr, size: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCopyImageSubData(srcName: GLuint, srcTarget: GLenum, srcLevel: GLint, srcX: GLint, srcY: GLint, srcZ: GLint, dstName: GLuint, dstTarget: GLenum, dstLevel: GLint, dstX: GLint, dstY: GLint, dstZ: GLint, srcWidth: GLsizei, srcHeight: GLsizei, srcDepth: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCopyTexImage2D(target: GLenum, level: GLint, internalformat: GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei, border: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCopyTexSubImage2D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCopyTexSubImage3D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCreateProgram() -> GLuint {
  let current = current();

  let name = (1..).find(|n| !current.programs.contains_key(n)).unwrap();
  current.programs.insert(name, Program::new());

  name
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCreateShader(type_: GLenum) -> GLuint {
  let current = current();

  let name = (1..).find(|n| !current.shaders.contains_key(n)).unwrap();
  current.shaders.insert(name, Arc::new(Shader::new(type_)));

  name
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCreateShaderProgramv(type_: GLenum, count: GLsizei, strings: *const *const GLchar) -> GLuint {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glCullFace(
  mode: GLenum,
) -> () {
  current().cull_face = mode;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace(disable(callback)))]
pub extern "C" fn glDebugMessageCallback(callback: GLDEBUGPROC, userParam: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDebugMessageControl(source: GLenum, type_: GLenum, severity: GLenum, count: GLsizei, ids: *const GLuint, enabled: GLboolean) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDebugMessageInsert(source: GLenum, type_: GLenum, id: GLuint, severity: GLenum, length: GLsizei, buf: *const GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteBuffers(n: GLsizei, buffers: *const GLuint) -> () {
  let current = current();

  let buffers = unsafe{ ::std::slice::from_raw_parts(buffers, n as usize) };
  for name in buffers {
    current.buffers.remove(&name);
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteFramebuffers(
  n: GLsizei,
  framebuffers: *const GLuint,
) -> () {
  let current = current();

  let framebuffers = unsafe{ ::std::slice::from_raw_parts(framebuffers, n as usize) };
  for name in framebuffers {
    current.framebuffers.remove(&name);
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteProgram(program: GLuint) -> () {
  let current = current();

  current.programs.remove(&program);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteProgramPipelines(n: GLsizei, pipelines: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteQueries(
  n: GLsizei,
  ids: *const GLuint,
) -> () {
  let current = current();

  let queries = unsafe{ ::std::slice::from_raw_parts(ids, n as usize) };
  for name in queries {
    current.queries.remove(&name);
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteRenderbuffers(n: GLsizei, renderbuffers: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteSamplers(count: GLsizei, samplers: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteShader(shader: GLuint) -> () {
  let current = current();

  current.shaders.remove(&shader);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteSync(sync: GLsync) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteTextures(
  n: GLsizei,
  textures: *const GLuint,
) -> () {
  let current = current();

  let textures = unsafe{ ::std::slice::from_raw_parts(textures, n as usize) };
  for name in textures {
    current.textures.remove(&name);
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteTransformFeedbacks(
  n: GLsizei,
  ids: *const GLuint,
) -> () {
  let current = current();

  let ids = unsafe{ ::std::slice::from_raw_parts(ids, n as usize) };
  for id in ids {
    current.transform_feedbacks.remove(&id);
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDeleteVertexArrays(n: GLsizei, arrays: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDepthFunc(func: GLenum) -> () {
  current().depth_func = CmpFunc::from(func);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDepthMask(
  flag: GLboolean,
) -> () {
  current().depth_mask = flag == GL_TRUE;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDepthRangef(n: GLfloat, f: GLfloat) -> () {
  current().depth_range = (n, f);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDetachShader(program: GLuint, shader: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDisable(cap: GLenum) -> () {
  *current().cap_mut(cap) = false;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDisableVertexAttribArray(index: GLuint) -> () {
  let current = current();

  current.vertex_arrays.get_mut(&current.vertex_array).unwrap().as_mut().unwrap().attribs[index as usize].enabled = false;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDisablei(target: GLenum, index: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDispatchCompute(
  num_groups_x: GLuint,
  num_groups_y: GLuint,
  num_groups_z: GLuint,
) -> () {
  use std::ops::Deref;

  use glsl::interpret::{self,Vars,Value};
  use glsl::{Interface};

  let current = current();
  let program = current.programs.get(&current.program).unwrap();

  let shader = program.shaders.iter().find(|s| s.type_ == GL_COMPUTE_SHADER).unwrap();
  let compiled = Arc::clone(Ref::map(shader.compiled.borrow(), |s| s.as_ref().unwrap()).deref());
  let work_group_size = compiled.work_group_size.unwrap();
  let num_in_work_group = (work_group_size[0] * work_group_size[1] * work_group_size[2]) as usize;

  let mut init_vars = HashMap::new();
  let mut shared = vec![];

  for iface in &compiled.interfaces {
    match iface {
      &Interface::ShaderStorageBlock(ref info)
        | &Interface::UniformBlock(ref info)
        => {
          let binding = match iface {
            &Interface::ShaderStorageBlock(_) => current.indexed_shader_storage_buffer[info.binding as usize],
            &Interface::UniformBlock(_) => current.indexed_uniform_buffer[info.binding as usize],
            _ => unreachable!(),
          };

          let buffer = current.buffers.get_mut(&binding.buffer).unwrap().as_mut().unwrap();
          let buf_ptr = unsafe{ buffer.as_mut_ptr().offset(binding.offset) };

          let size = info.active_variables.last().and_then(|v| {
            if glsl::array_size(&v.array) == 0 {
              Some(((binding.size - v.offset as usize) / glsl::size_of(&v.type_, Some(&compiled.types))) as u32)
            } else { None }
          });

          if let Some(ref var_name) = info.var_name {
            init_vars.insert(var_name.clone(), Value::Buffer(glsl::TypeSpecifierNonArray::Custom(info.name.clone()), buf_ptr, size));
          } else {
            // TODO: unify with Expression::FieldSelection
            for field in &info.active_variables {
              let length = Some(if glsl::array_size(&field.array) > 0 { glsl::array_size(&field.array) } else { size.unwrap() });

              let val = Value::Buffer(field.type_.clone(), unsafe{ buf_ptr.offset(field.offset as isize) }, length);
              init_vars.insert(field.name.clone(), val);
            }
          }
        },
      &Interface::Uniform(ref info) => {
        let index = program.uniforms.iter().position(|i| i.name == info.name).unwrap();
        let val = &program.uniform_values[index];
        let val = match val {
          &Value::UImage2DUnit(unit) => {
            let unit_target = current.images[unit];
            let texture = current.textures.get(&unit_target).expect("No texture for unit");
            let texture = texture.as_ref().expect("No texture in slot");

            Value::UImage2D(Arc::clone(texture))
          },
          x => x.clone(),
        };
        if info.typ == GL_UNSIGNED_INT_ATOMIC_COUNTER { continue; }
        init_vars.insert(info.name.clone(), val);
      },
      &Interface::Shared(ref info) => {
        shared.push(info.clone());
      },
      &Interface::AtomicCounter(ref info) => {
        let binding = current.indexed_atomic_counter_buffer[info.binding as usize];
        let buffer = current.buffers.get_mut(&binding.buffer).unwrap().as_mut().unwrap();
        let buf_ptr = unsafe{ buffer.as_mut_ptr().offset(binding.offset) };

        init_vars.insert(info.name.clone(), Value::Buffer(glsl::TypeSpecifierNonArray::AtomicUint, buf_ptr, Some(info.size as u32)));
      },
      &Interface::Input(_) => {}
      &Interface::Output(_) => {}
    }
  }

  for gx in 0..num_groups_x {
    for gy in 0..num_groups_y {
      for gz in 0..num_groups_z {
        let barrier = Arc::new(Barrier::new(num_in_work_group));
        let mut threads = Vec::with_capacity(num_in_work_group);

        let mut shared_buffers = shared.iter().map(|s| {
          let size = s.size * glsl::size_of(&s.typ, Some(&compiled.types));
          let mut buf = Vec::with_capacity(size);
          unsafe{ buf.set_len(size); }

          buf
        }).collect::<Vec<Vec<u8>>>();

        let mut init_vars = init_vars.clone();
        for (ref info, ref mut buf) in shared.iter().zip(shared_buffers.iter_mut()) {
          init_vars.insert(info.name.clone(), Value::Buffer(info.typ.clone(), buf.as_mut_ptr(), Some(info.size as u32)));
        }

        let shared_buffers = Arc::new(shared_buffers);

        for lx in 0..work_group_size[0] {
          for ly in 0..work_group_size[1] {
            for lz in 0..work_group_size[2] {
              let compiled = Arc::clone(&compiled);
              let barrier = Arc::clone(&barrier);
              let init_vars = init_vars.clone();
              let _shared_buffers = Arc::clone(&shared_buffers);

              let t = thread::spawn(move || {

                // TODO: omg clean up
                let mut vars = Vars::new();
                vars.push();
                vars.insert("__barrier".into(), Value::Barrier(barrier));

                vars.insert("gl_NumWorkGroups".into(), Value::UVec3([num_groups_x,
                                                                          num_groups_y,
                                                                          num_groups_z]));
                vars.insert("gl_WorkGroupSize".into(), Value::UVec3(work_group_size));

                vars.insert("gl_WorkGroupID".into(), Value::UVec3([gx, gy, gz]));
                vars.insert("gl_LocalInvocationID".into(), Value::UVec3([lx, ly, lz]));

                let gid = [
                  gx * work_group_size[0] + lx,
                  gy * work_group_size[1] + ly,
                  gz * work_group_size[2] + lz,
                ];
                vars.insert("gl_GlobalInvocationID".into(), Value::UVec3(gid));

                let lii = lz * work_group_size[0] * work_group_size[1]
                  + ly * work_group_size[0]
                  + lx;
                vars.insert("gl_LocalInvocationIndex".into(), Value::Uint(lii));

                for (ref name, ref data) in &init_vars {
                  vars.insert((*name).clone(), (*data).clone());
                }

                vars.push();

                let main = &compiled.functions[&"main".into()][0];

                interpret::execute(&main.1, &mut vars, &compiled);
              });
              threads.push(t);
            }
          }
        }

        for thread in threads {
          thread.join().unwrap();
        }
      }
    }
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDispatchComputeIndirect(indirect: GLintptr) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawArrays(
  mode: GLenum,
  first: GLint,
  count: GLsizei,
) -> () {
  draw::glDrawArraysOneInstance(current(), mode, first, count, 0, 0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawArraysIndirect(mode: GLenum, indirect: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawArraysInstanced(mode: GLenum, first: GLint, count: GLsizei, instancecount: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawBuffers(n: GLsizei, bufs: *const GLenum) -> () {
  assert_eq!(n, 1);
  assert_eq!(unsafe{ *bufs }, GL_BACK);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawElements(
  mode: GLenum,
  count: GLsizei,
  type_: GLenum,
  indices: *const c_void,
) -> () {
  draw::glDrawElementsOneInstance(current(), mode, count, type_, indices, 0, 0, 0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawElementsBaseVertex(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void, basevertex: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawElementsIndirect(mode: GLenum, type_: GLenum, indirect: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawElementsInstanced(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void, instancecount: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawElementsInstancedBaseVertex(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void, instancecount: GLsizei, basevertex: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawRangeElements(mode: GLenum, start: GLuint, end: GLuint, count: GLsizei, type_: GLenum, indices: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glDrawRangeElementsBaseVertex(mode: GLenum, start: GLuint, end: GLuint, count: GLsizei, type_: GLenum, indices: *const c_void, basevertex: GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glEnable(cap: GLenum) -> () {
  *current().cap_mut(cap) = true;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glEnableVertexAttribArray(
  index: GLuint,
) -> () {
  let current = current();

  current.vertex_arrays.get_mut(&current.vertex_array).unwrap().as_mut().unwrap().attribs[index as usize].enabled = true;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glEnablei(target: GLenum, index: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glEndQuery(
  _target: GLenum,
) -> () {
  let current = current();

  current.query = None;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glEndTransformFeedback(
) -> () {
  let current = current();

  current.transform_feedback_capture = None;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFenceSync(condition: GLenum, flags: GLbitfield) -> GLsync {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFinish() -> () {
  let (tx, rx) = mpsc::channel();

  current().tx.send(Command::Finish(tx)).unwrap();

  rx.recv().unwrap();
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFlush() -> () {
  let (tx, rx) = mpsc::channel();

  current().tx.send(Command::Flush(tx)).unwrap();

  rx.recv().unwrap();
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFlushMappedBufferRange(target: GLenum, offset: GLintptr, length: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFramebufferParameteri(target: GLenum, pname: GLenum, param: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFramebufferRenderbuffer(target: GLenum, attachment: GLenum, renderbuffertarget: GLenum, renderbuffer: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFramebufferTexture(target: GLenum, attachment: GLenum, texture: GLuint, level: GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFramebufferTexture2D(
  target: GLenum,
  attachment: GLenum,
  textarget: GLenum,
  texture: GLuint,
  level: GLint,
) -> () {
  assert_eq!(level, 0);
  assert_eq!(textarget, GL_TEXTURE_2D);
  assert_eq!(attachment, GL_COLOR_ATTACHMENT0);

  let current = current();

  let fbid = match target {
    GL_DRAW_FRAMEBUFFER | GL_FRAMEBUFFER => current.draw_framebuffer,
    GL_READ_FRAMEBUFFER => current.read_framebuffer,
    x => unimplemented!("{:x}", x),
  };

  let mut framebuffer = current.framebuffers.get_mut(&fbid).unwrap().as_ref().unwrap().write().unwrap();
  let texture = &current.textures[&texture].as_ref().unwrap();

  framebuffer.color0 = Some(Arc::clone(texture));
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFramebufferTextureLayer(target: GLenum, attachment: GLenum, texture: GLuint, level: GLint, layer: GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glFrontFace(
  mode: GLenum,
) -> () {
  current().front_face_cw = mode == GL_CW;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenBuffers(n: GLsizei, buffers: *mut GLuint) -> () {
  let current = current();

  let mut search = 1;
  for i in 0..n {
    while current.buffers.contains_key(&search) { search += 1 };
    current.buffers.insert(search, None);
    unsafe{ ptr::write(buffers.offset(i as isize), search); }
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenFramebuffers(
  n: GLsizei,
  framebuffers: *mut GLuint,
) -> () {
  let current = current();

  let mut search = 1;
  for i in 0..n {
    while current.framebuffers.contains_key(&search) { search += 1 };
    current.framebuffers.insert(search, None);
    unsafe{ ptr::write(framebuffers.offset(i as isize), search); }
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenProgramPipelines(n: GLsizei, pipelines: *mut GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenQueries(
  n: GLsizei,
  ids: *mut GLuint,
) -> () {
  let current = current();

  let mut search = 1;
  for i in 0..n {
    while current.queries.contains_key(&search) { search += 1 };
    current.queries.insert(search, None);
    unsafe{ ptr::write(ids.offset(i as isize), search); }
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenRenderbuffers(n: GLsizei, renderbuffers: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenSamplers(count: GLsizei, samplers: *mut GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenTextures(
  n: GLsizei,
  textures: *mut GLuint,
) -> () {
  let current = current();

  let mut search = 1;
  for i in 0..n {
    while current.textures.contains_key(&search) { search += 1 };
    current.textures.insert(search, None );
    unsafe{ ptr::write(textures.offset(i as isize), search); }
  }
}

// TODO: extract this code since most Gens are using it
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenTransformFeedbacks(
  n: GLsizei,
  ids: *mut GLuint,
) -> () {
  let current = current();

  let mut search = 1;
  for i in 0..n {
    while current.transform_feedbacks.contains_key(&search) { search += 1 };
    current.transform_feedbacks.insert(search, None );
    unsafe{ ptr::write(ids.offset(i as isize), search); }
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenVertexArrays(n: GLsizei, arrays: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGenerateMipmap(target: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetActiveAttrib(program: GLuint, index: GLuint, bufSize: GLsizei, length: *mut GLsizei, size: *mut GLint, type_: *mut GLenum, name: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetActiveUniform(program: GLuint, index: GLuint, bufSize: GLsizei, length: *mut GLsizei, size: *mut GLint, type_: *mut GLenum, name: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetActiveUniformBlockName(program: GLuint, uniformBlockIndex: GLuint, bufSize: GLsizei, length: *mut GLsizei, uniformBlockName: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetActiveUniformBlockiv(program: GLuint, uniformBlockIndex: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetActiveUniformsiv(program: GLuint, uniformCount: GLsizei, uniformIndices: *const GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetAttachedShaders(program: GLuint, maxCount: GLsizei, count: *mut GLsizei, shaders: *mut GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetAttribLocation(
  program: GLuint,
  name: *const GLchar,
) -> GLint {
  let current = current();
  let program = current.programs.get(&program).expect("No current program");

  let name = unsafe{ CStr::from_ptr(name) };
  let name = name.to_str().expect("Name couldn't convert to string");

  program.attrib_locations.iter().position(|a| a.as_ref().map_or(false, |a| a == name)).map(|i| i as GLint).unwrap_or(-1)
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetBooleani_v(target: GLenum, index: GLuint, data: *mut GLboolean) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetBooleanv(pname: GLenum, data: *mut GLboolean) -> () {
  let result = match pname {
    GL_TRANSFORM_FEEDBACK_ACTIVE => false,
    x => unimplemented!("{:x}", x),
  };

  let result = if result { GL_TRUE } else { GL_FALSE };

  unsafe{ ptr::write(data, result) };
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetBufferParameteri64v(target: GLenum, pname: GLenum, params: *mut GLint64) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetBufferParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetBufferPointerv(target: GLenum, pname: GLenum, params: *const *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetDebugMessageLog(count: GLuint, bufSize: GLsizei, sources: *mut GLenum, types: *mut GLenum, ids: *mut GLuint, severities: *mut GLenum, lengths: *mut GLsizei, messageLog: *mut GLchar) -> GLuint {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetError() -> GLenum {
  GL_NONE
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetFloatv(
  pname: GLenum,
  data: *mut GLfloat,
) -> () {
  let result = match pname {
    GL_ALIASED_POINT_SIZE_RANGE => {
      unsafe{ ptr::write(data, 1.0) };
      unsafe{ ptr::write(data.add(1), 1.0) };
      return
    }
    x => unimplemented!("{:x}", x),
  };

  unsafe{ ptr::write(data, result) };
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetFragDataLocation(program: GLuint, name: *const GLchar) -> GLint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetFramebufferAttachmentParameteriv(target: GLenum, attachment: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetFramebufferParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetGraphicsResetStatus() -> GLenum {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetInteger64i_v(target: GLenum, index: GLuint, data: *mut GLint64) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetInteger64v(pname: GLenum, data: *mut GLint64) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetIntegeri_v(target: GLenum, index: GLuint, data: *mut GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetIntegerv(pname: GLenum, data: *mut GLint) -> () {
  let result = match pname {
    GL_NUM_EXTENSIONS => 0,
    GL_MAX_VERTEX_ATTRIBS => MAX_VERTEX_ATTRIBS as GLint,
    GL_MAX_SAMPLE_MASK_WORDS => 0, // TODO
    GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS => 0, // TODO
    GL_MAX_UNIFORM_BUFFER_BINDINGS => 0, // TODO
    GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS => 0, // TODO
    GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS => 0, // TODO
    GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS => 0, // TODO
    GL_MAX_IMAGE_UNITS => 0, // TODO
    GL_MAX_TEXTURE_IMAGE_UNITS => 0, // TODO
    GL_MAX_TEXTURE_SIZE => MAX_TEXTURE_SIZE as GLint,
    GL_MAX_CUBE_MAP_TEXTURE_SIZE => MAX_CUBE_MAP_TEXTURE_SIZE as GLint,
    GL_MAX_RENDERBUFFER_SIZE => MAX_RENDERBUFFER_SIZE as GLint,
    GL_MAX_ARRAY_TEXTURE_LAYERS => MAX_ARRAY_TEXTURE_LAYERS as GLint,
    GL_MAX_3D_TEXTURE_SIZE => MAX_3D_TEXTURE_SIZE as GLint,
    GL_SUBPIXEL_BITS => 4, // TODO: figure out what this really means
    x => unimplemented!("{:x}", x),
  };

  unsafe{ ptr::write(data, result) };
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetInternalformativ(target: GLenum, internalformat: GLenum, pname: GLenum, bufSize: GLsizei, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetMultisamplefv(pname: GLenum, index: GLuint, val: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetObjectLabel(identifier: GLenum, name: GLuint, bufSize: GLsizei, length: *mut GLsizei, label: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetObjectPtrLabel(ptr: *const c_void, bufSize: GLsizei, length: *mut GLsizei, label: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetPointerv(pname: GLenum, params: *const *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramBinary(program: GLuint, bufSize: GLsizei, length: *mut GLsizei, binaryFormat: *mut GLenum, binary: *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramInfoLog(program: GLuint, bufSize: GLsizei, length: *mut GLsizei, infoLog: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramInterfaceiv(program: GLuint, programInterface: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramPipelineInfoLog(pipeline: GLuint, bufSize: GLsizei, length: *mut GLsizei, infoLog: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramPipelineiv(pipeline: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramResourceIndex(
  program: GLuint,
  programInterface: GLenum,
  name: *const GLchar,
) -> GLuint {
  let current = current();

  let program = current.programs.get(&program).unwrap();

  let name = unsafe{ CStr::from_ptr(name) };
  let name = name.to_str().unwrap();
  let name = Atom::from(name);

  match programInterface {
    GL_SHADER_STORAGE_BLOCK => {
      *program.ssbos.iter().find(|&(_, ref info)| {
        info.name == name
      }).unwrap().0
    },
    GL_UNIFORM_BLOCK => {
      *program.ubos.iter().find(|&(_, ref info)| {
        info.name == name
      }).unwrap().0
    },
    GL_BUFFER_VARIABLE => {
      for (_, ref info) in &program.ssbos {
        for var in &info.active_variables {
          if info.var_name.is_some() {
            if name == format!("{}.{}", info.name, var.name) { return var.index as GLuint };
          } else {
            if name == format!("{}", var.name) { return var.index as GLuint };
            if name == format!("{}[0]", var.name) { return var.index as GLuint };
          }
        }
      }
      unimplemented!()
    },
    GL_UNIFORM => {
      for (_, ref info) in &program.ubos {
        for var in &info.active_variables {
          if name == format!("{}.{}", info.name, var.name) { return var.index as GLuint };
        }
      }
      for info in &program.uniforms {
        if name == info.name { return info.index as GLuint };
      }
      unimplemented!()
    },
    x => unimplemented!("{:x}", x),
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramResourceLocation(program: GLuint, programInterface: GLenum, name: *const GLchar) -> GLint {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramResourceName(
  program: GLuint,
  programInterface: GLenum,
  index: GLuint,
  bufSize: GLsizei,
  length: *mut GLsizei,
  name: *mut GLchar,
) -> () {
  let current = current();
  let program = current.programs.get(&program).unwrap();

  let n = match programInterface {
    GL_SHADER_STORAGE_BLOCK => { program.ssbos.get(&index).map(|s| &s.name).unwrap() },
    GL_UNIFORM_BLOCK => { program.ubos.get(&index).map(|s| &s.name).unwrap() },
    GL_BUFFER_VARIABLE => { &program.ssbos.values().flat_map(|ssbo| ssbo.active_variables.iter()).find(|a| a.index == index as i32).unwrap().name },
    GL_UNIFORM => { &program.ubos.values().flat_map(|ubo| ubo.active_variables.iter()).find(|a| a.index == index as i32).unwrap().name },
    x => unimplemented!("{:x}", x),
  };

  unsafe{ write_string(n, name, bufSize, length) };
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramResourceiv(
  program: GLuint,
  programInterface:GLenum,
  index: GLuint,
  propCount: GLsizei,
  props: *const GLenum,
  bufSize: GLsizei,
  length: *mut GLsizei,
  params: *mut GLint,
) -> () {
  struct Pusher {
    ptr: *mut GLint,
    pub current_offset: isize,
    max: isize,
  };

  impl Pusher {
    fn push(&mut self, val: GLint) {
      if self.current_offset < self.max as isize {
        unsafe{ *self.ptr.offset(self.current_offset) = val; }
        self.current_offset += 1;
      }
    }
  }

  let mut pusher = Pusher{ ptr: params, current_offset: 0, max: bufSize as isize };

  let current = current();
  let program = current.programs.get(&program).unwrap();

  let props = unsafe{ ::std::slice::from_raw_parts(props, propCount as usize) };

  {
    let mut push = |val: GLint| pusher.push(val);
    match programInterface {
      GL_SHADER_STORAGE_BLOCK | GL_UNIFORM_BLOCK => {
        let info = match programInterface {
          GL_SHADER_STORAGE_BLOCK => program.ssbos.get(&index).unwrap(),
          GL_UNIFORM_BLOCK => program.ubos.get(&index).unwrap(),
          x => unimplemented!("{:x}", x),
        };

        for &prop in props {
          match prop {
            GL_BUFFER_BINDING => push(info.binding as GLint),
            GL_BUFFER_DATA_SIZE => push(info.size as GLint),
            GL_NUM_ACTIVE_VARIABLES => push(info.active_variables.len() as GLint),
            GL_ACTIVE_VARIABLES => info.active_variables.iter().for_each(|v| push(v.index)),
            GL_NAME_LENGTH => push(info.name.len() as GLint),
            x => unimplemented!("{:x}", x),
          };
        }
      },
      GL_BUFFER_VARIABLE => {
        let var = program.ssbos.values().flat_map(|ssbo| ssbo.active_variables.iter()).find(|a| a.index == index as i32).unwrap();
        for &prop in props {
          match prop {
            GL_BLOCK_INDEX => push(*program.ssbos.iter().find(|&(_, ref ssbo)| ssbo.active_variables.contains(var)).unwrap().0 as i32),
            GL_TYPE => push(glsl::gl_type(&var.type_) as i32),
            GL_ARRAY_SIZE => push(glsl::array_size(&var.array) as i32),
            GL_OFFSET => push(var.offset as i32),
            GL_ARRAY_STRIDE => push(glsl::stride_of(&var.type_, None) as i32),
            GL_MATRIX_STRIDE => push(0), // TODO
            GL_IS_ROW_MAJOR => push(0), // TODO
            GL_TOP_LEVEL_ARRAY_SIZE => push(1), // TODO
            GL_TOP_LEVEL_ARRAY_STRIDE => push(0), // TODO
            GL_NAME_LENGTH => push(var.name.len() as i32),
            x => unimplemented!("{:x}", x),
          };
        }
      },
      GL_UNIFORM => {
        if let Some(var) = program.ubos.values().flat_map(|ubo| ubo.active_variables.iter()).find(|a| a.index == index as i32) {
          for &prop in props {
            match prop {
              GL_BLOCK_INDEX => push(*program.ubos.iter().find(|&(_, ref ssbo)| ssbo.active_variables.contains(var)).unwrap().0 as i32),
              GL_TYPE => push(glsl::gl_type(&var.type_) as i32),
              GL_ARRAY_SIZE => push(glsl::array_size(&var.array) as i32),
              GL_OFFSET => push(var.offset as i32),
              GL_ARRAY_STRIDE => push(glsl::stride_of(&var.type_, None) as i32),
              GL_MATRIX_STRIDE => push(0), // TODO
              GL_IS_ROW_MAJOR => push(0), // TODO
              GL_TOP_LEVEL_ARRAY_SIZE => push(1), // TODO
              GL_TOP_LEVEL_ARRAY_STRIDE => push(0), // TODO
              GL_NAME_LENGTH => push(var.name.len() as i32),
              GL_ATOMIC_COUNTER_BUFFER_INDEX => push(0), // TODO
              x => unimplemented!("{:x}", x),
            };
          }
        } else if let Some(var) = program.uniforms.iter().find(|i| i.index == index as i32) {
          for &prop in props {
            match prop {
              GL_ATOMIC_COUNTER_BUFFER_INDEX => push(*program.atomic_counters.iter().find(|&(_, ac)| var.name == ac.name).unwrap().0 as i32),
              GL_OFFSET => push(0), // TODO
              x => unimplemented!("{:x}", x),
            };
          }
        } else { unreachable!() }
      },
      GL_ATOMIC_COUNTER_BUFFER => {
        // TODO
        let info = program.atomic_counters.get(&index).unwrap();
        for &prop in props {
          match prop {
            GL_BUFFER_DATA_SIZE => push((mem::size_of::<u32>() * info.size) as i32),
            x => unimplemented!("{:x}", x),
          };
        }
      },
      x => unimplemented!("{:x}", x),
    }
  }

  if !length.is_null() {
    unsafe{ *length = pusher.current_offset as i32; }
  }

}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetProgramiv(
  program: GLuint,
  pname: GLenum,
  params: *mut GLint,
) -> () {
  let current = current();
  let program = current.programs.get(&program).unwrap();

  let value = match pname {
    GL_LINK_STATUS => GL_TRUE as GLint, // TODO
    GL_INFO_LOG_LENGTH => 0, // TODO
    GL_TRANSFORM_FEEDBACK_VARYINGS => program.transform_feedback.as_ref().map_or(0, |t| t.0.len()) as GLint,
    GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH => program.transform_feedback.as_ref().and_then(|t| t.0.iter().map(|n| n.len() + 1).min()).unwrap_or(0) as GLint,
    x => unimplemented!("{:x}", x),
  };

  unsafe{ ptr::write(params, value) };
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetQueryObjectuiv(
  id: GLuint,
  pname: GLenum,
  params: *mut GLuint,
) -> () {
  let current = current();
  let query = current.queries.get(&id).unwrap().as_ref().unwrap();

  let value = match pname {
    GL_QUERY_RESULT_AVAILABLE => GL_TRUE as GLuint, // TODO
    GL_QUERY_RESULT => query.value,
    x => unimplemented!("{:x}", x),
  };

  unsafe{ *params = value; }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetQueryiv(_target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  assert_eq!(pname, GL_CURRENT_QUERY);
  unsafe{ ptr::write(params, 0) };
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetRenderbufferParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetSamplerParameterIiv(sampler: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetSamplerParameterIuiv(sampler: GLuint, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetSamplerParameterfv(sampler: GLuint, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetSamplerParameteriv(sampler: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetShaderInfoLog(
  shader: GLuint,
  bufSize: GLsizei,
  length: *mut GLsizei,
  infoLog: *mut GLchar
) -> () {
  let current = current();
  let shader = current.shaders.get(&shader).unwrap();

  let log = shader.info_log.borrow();

  unsafe{ write_string(&log, infoLog, bufSize, length) };
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetShaderPrecisionFormat(shadertype: GLenum, precisiontype: GLenum, range: *mut GLint, precision: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetShaderSource(shader: GLuint, bufSize: GLsizei, length: *mut GLsizei, source: *mut GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetShaderiv(shader: GLuint, pname: GLenum, params: *mut GLint) -> () {
  let current = current();
  let shader = current.shaders.get(&shader).unwrap();
  let value = match pname {
    GL_COMPILE_STATUS => if shader.compiled.borrow().is_some() { GL_TRUE as GLint } else { GL_FALSE as GLint },
    GL_INFO_LOG_LENGTH => shader.info_log.borrow().len() as GLint,
    x => unimplemented!("{:x}", x),
  };

  unsafe{ ptr::write(params, value) };
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetString(
  name: GLenum,
) -> *const GLubyte {
  match name {
    GL_VENDOR => b"Michael Fairley\0" as *const u8,
    GL_RENDERER => b"Mygl\0" as *const u8,
    GL_VERSION => b"OpenGL ES 3.2\0" as *const u8,
    GL_SHADING_LANGUAGE_VERSION => b"OpenGL ES GLSL ES 3.20\0" as *const u8,
    x => unimplemented!("{:x}", x)
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetStringi(name: GLenum, index: GLuint) -> *const GLubyte {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetSynciv(sync: GLsync, pname: GLenum, bufSize: GLsizei, length: *mut GLsizei, values: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTexLevelParameterfv(target: GLenum, level: GLint, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTexLevelParameteriv(target: GLenum, level: GLint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTexParameterIiv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTexParameterIuiv(target: GLenum, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTexParameterfv(target: GLenum, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTexParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetTransformFeedbackVarying(
  program: GLuint,
  index: GLuint,
  bufSize: GLsizei,
  length: *mut GLsizei,
  size: *mut GLsizei,
  type_: *mut GLenum,
  name: *mut GLchar,
) -> () {
  let current = current();
  let program = current.programs.get(&program).unwrap();

  let n = &program.transform_feedback.as_ref().unwrap().0[index as usize];

  unsafe{ write_string(&n, name, bufSize, length) };

  let vert_shader = program.shaders.iter().find(|s| s.type_ == GL_VERTEX_SHADER).unwrap();
  let vert_compiled = Arc::clone(&*Ref::map(vert_shader.compiled.borrow(), |s| s.as_ref().unwrap()));

  let (name, _array_indices) = parse_variable_name(n);

  let var_type = vert_compiled.interfaces.iter().filter_map(|i| if let &glsl::Interface::Output(ref v) = i {
    if name == &v.name {
      Some(v.type_.clone())
    } else { None }
  } else { None }).next().expect(&format!("Didn't find {} as an out variable for glGetTransformFeedbackVarying", n));


  unsafe{
    *type_ = glsl::gl_type(&var_type);
    *size = glsl::size_of(&var_type, None) as GLint;
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetUniformBlockIndex(program: GLuint, uniformBlockName: *const GLchar) -> GLuint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetUniformIndices(program: GLuint, uniformCount: GLsizei, uniformNames: *const *const GLchar, uniformIndices: *mut GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetUniformLocation(
  program: GLuint,
  name: *const GLchar,
) -> GLint {
  let current = current();
  let program = current.programs.get(&program).unwrap();

  let name = unsafe{ CStr::from_ptr(name) };
  let name = name.to_str().unwrap();
  let name = Atom::from(name);

  program.uniforms.iter().enumerate().find(|&(_i, info)| {
    info.name == name
  }).unwrap().0 as GLint
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetUniformfv(program: GLuint, location: GLint, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetUniformiv(program: GLuint, location: GLint, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetUniformuiv(program: GLuint, location: GLint, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetVertexAttribIiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetVertexAttribIuiv(index: GLuint, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetVertexAttribPointerv(index: GLuint, pname: GLenum, pointer: *const *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetVertexAttribfv(index: GLuint, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetVertexAttribiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetnUniformfv(program: GLuint, location: GLint, bufSize: GLsizei, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetnUniformiv(program: GLuint, location: GLint, bufSize: GLsizei, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glGetnUniformuiv(program: GLuint, location: GLint, bufSize: GLsizei, params: *mut GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glHint(
  _target: GLenum,
  _mode: GLenum,
) -> () {
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glInvalidateFramebuffer(target: GLenum, numAttachments: GLsizei, attachments: *const GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glInvalidateSubFramebuffer(target: GLenum, numAttachments: GLsizei, attachments: *const GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsBuffer(buffer: GLuint) -> GLboolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsEnabled(cap: GLenum) -> GLboolean {
  if *current().cap_mut(cap) { GL_TRUE } else { GL_FALSE }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsEnabledi(target: GLenum, index: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsFramebuffer(framebuffer: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsProgram(program: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsProgramPipeline(pipeline: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsQuery(id: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsRenderbuffer(renderbuffer: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsSampler(sampler: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsShader(shader: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsSync(sync: GLsync) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsTexture(texture: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsTransformFeedback(id: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glIsVertexArray(array: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glLineWidth(width: GLfloat) -> () {
  current().line_width = width;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glLinkProgram(
  program: GLuint,
) -> () {
  let current = current();

  let program = current.programs.get_mut(&program).unwrap();

  let mut ssbos = HashMap::new();
  let mut ubos = HashMap::new();
  let mut uniforms = vec![];
  let mut uniform_values = vec![];
  let mut atomic_counters = HashMap::new();

  let mut next_ssbo_variable_index = 0;
  let mut next_uniform_index = 0;

  for shader in &program.shaders {
    for iface in &shader.compiled.borrow().as_ref().unwrap().interfaces {
      match iface {
        &glsl::Interface::ShaderStorageBlock(ref info) => {
          let i = ssbos.len() + 1;
          let mut info = info.clone();
          for ref mut var in info.active_variables.iter_mut() {
            var.index = next_ssbo_variable_index;
            next_ssbo_variable_index += 1;
          }
          ssbos.insert(i as GLuint, info);
        },
        &glsl::Interface::UniformBlock(ref info) => {
          let i = ubos.len() + 1;
          let mut info = info.clone();
          for ref mut var in info.active_variables.iter_mut() {
            var.index = next_uniform_index;
            next_uniform_index += 1;
          }
          ubos.insert(i as GLuint, info);
        },
        &glsl::Interface::Uniform(ref info) => {
          let mut info = info.clone();
          info.index = next_uniform_index;
          next_uniform_index += 1;
          uniforms.push(info.clone());

          let val = match info.typ {
            GL_UNSIGNED_INT => glsl::interpret::Value::Uint(0),
            GL_UNSIGNED_INT_VEC2 => glsl::interpret::Value::UVec2([0, 0]),
            GL_UNSIGNED_INT_VEC3 => glsl::interpret::Value::UVec3([0, 0, 0]),
            GL_UNSIGNED_INT_VEC4 => glsl::interpret::Value::UVec4([0, 0, 0, 0]),
            GL_INT => glsl::interpret::Value::Int(0),
            GL_INT_VEC2 => glsl::interpret::Value::IVec2([0, 0]),
            GL_INT_VEC3 => glsl::interpret::Value::IVec3([0, 0, 0]),
            GL_INT_VEC4 => glsl::interpret::Value::IVec4([0, 0, 0, 0]),
            GL_FLOAT => glsl::interpret::Value::Float(0.0),
            GL_FLOAT_VEC2 => glsl::interpret::Value::Vec2([0.0, 0.0]),
            GL_FLOAT_VEC3 => glsl::interpret::Value::Vec3([0.0, 0.0, 0.0]),
            GL_FLOAT_VEC4 => glsl::interpret::Value::Vec4([0.0, 0.0, 0.0, 0.0]),
            GL_BOOL => glsl::interpret::Value::Bool(0),
            GL_BOOL_VEC2 => glsl::interpret::Value::BVec2([0, 0]),
            GL_BOOL_VEC3 => glsl::interpret::Value::BVec3([0, 0, 0]),
            GL_BOOL_VEC4 => glsl::interpret::Value::BVec4([0, 0, 0, 0]),
            GL_UNSIGNED_INT_IMAGE_2D => glsl::interpret::Value::UImage2DUnit(info.binding),
            GL_UNSIGNED_INT_ATOMIC_COUNTER => glsl::interpret::Value::Void,
            x => unimplemented!("{:x}", x),
          };
          uniform_values.push(val);
        },
        &glsl::Interface::Shared(_) => {},
        &glsl::Interface::AtomicCounter(ref info) => {
          let i = atomic_counters.len() + 1;
          let mut info = info.clone();
          atomic_counters.insert(i as GLuint, info);
        },
        &glsl::Interface::Input(_) => {},
        &glsl::Interface::Output(_) => {},
      }
    }
  }

  let mut attrib_locations = [None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None];
  if let Some(vert) = program.shaders.iter().find(|s| s.type_ == GL_VERTEX_SHADER) {
    // TODO: assign the ones with explicit locations first
    for iface in &vert.compiled.borrow().as_ref().unwrap().interfaces {
      if let &glsl::Interface::Input(ref var) = iface {
        let next_unused = attrib_locations.iter().position(|a| a.is_none()).unwrap();
        attrib_locations[next_unused] = Some(var.name.clone());
      }
    }
  }

  program.ssbos = ssbos;
  program.uniform_block_bindings = vec![0; ubos.len()];
  program.ubos = ubos;
  program.uniforms = uniforms;
  program.uniform_values = uniform_values;
  program.atomic_counters = atomic_counters;
  program.transform_feedback = program.pending_transform_feedback.clone();
  program.attrib_locations = attrib_locations;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glMapBufferRange(target: GLenum, offset: GLintptr, _length: GLsizeiptr, _access: GLbitfield) -> *mut c_void {
  let current = current();

  let target_name = current.buffer_target(target);
  let mut buffer = current.buffers.get_mut(&target_name);
  let buffer = buffer.as_mut().unwrap().as_mut().unwrap();
  let range = &mut buffer[(offset as usize)..];
  range.as_mut_ptr() as _
}


#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glMemoryBarrier(
  _barriers: GLbitfield,
) -> () {
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glMemoryBarrierByRegion(barriers: GLbitfield) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glMinSampleShading(value: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glObjectLabel(identifier: GLenum, name: GLuint, length: GLsizei, label: *const GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glObjectPtrLabel(ptr: *const c_void, length: GLsizei, label: *const GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPatchParameteri(pname: GLenum, value: GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPauseTransformFeedback(
) -> () {
  let current = current();
  current.transform_feedback_paused = true;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPixelStorei(pname: GLenum, param: GLint) -> () {
  // unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPolygonOffset(factor: GLfloat, units: GLfloat) -> () {
  current().polygon_offset = (factor, units);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPopDebugGroup() -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPrimitiveBoundingBox(minX: GLfloat, minY: GLfloat, minZ: GLfloat, minW: GLfloat, maxX: GLfloat, maxY: GLfloat, maxZ: GLfloat, maxW: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramBinary(program: GLuint, binaryFormat: GLenum, binary: *const c_void, length: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramParameteri(program: GLuint, pname: GLenum, value: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform1f(program: GLuint, location: GLint, v0: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform1fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform1i(program: GLuint, location: GLint, v0: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform1iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform1ui(program: GLuint, location: GLint, v0: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform1uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform2f(program: GLuint, location: GLint, v0: GLfloat, v1: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform2fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform2i(program: GLuint, location: GLint, v0: GLint, v1: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform2iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform2ui(program: GLuint, location: GLint, v0: GLuint, v1: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform2uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform3f(program: GLuint, location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform3fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform3i(program: GLuint, location: GLint, v0: GLint, v1: GLint, v2: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform3iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform3ui(program: GLuint, location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform3uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform4f(program: GLuint, location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat, v3: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform4fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform4i(program: GLuint, location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform4iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform4ui(program: GLuint, location: GLint, v0: GLuint, v1: GLuint, v2: GLuint, v3: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniform4uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix2fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix2x3fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix2x4fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix3fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix3x2fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix3x4fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix4fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix4x2fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glProgramUniformMatrix4x3fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glPushDebugGroup(source: GLenum, id: GLuint, length: GLsizei, message: *const GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glReadBuffer(
  src: GLenum,
) -> () {
  let current = current();

  if current.read_framebuffer == 0 {
    assert_eq!(src, GL_BACK);
  } else {
    assert_eq!(src, GL_COLOR_ATTACHMENT0);
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glReadPixels(
  x: GLint,
  y: GLint,
  width: GLsizei,
  height: GLsizei,
  format: GLenum,
  type_: GLenum,
  pixels: *mut c_void,
) -> () {
  // TODO: this needs a ton of work. It is super hardcoded right now

  let current = current();

  glFinish();

  match (format, type_) {
    (GL_RGBA, GL_UNSIGNED_BYTE) => {
      let read = unsafe{ current.read_surface.as_ref() };
      let read = read.as_ref().unwrap();

      assert_eq!(current.read_framebuffer, 0);
      let mut pos = pixels as *mut u8;

      for y in y..(y+height) {
        for x in x..(x+width) {

          let (red, green, blue, alpha) = read.get_pixel(x, y);

          // TODO: find a way to not roundtrip 8 bit formats through floats
          let red = (red * 0xFF as f32) as u8;
          let green = (green * 0xFF as f32) as u8;
          let blue = (blue * 0xFF as f32) as u8;
          let alpha = (alpha * 0xFF as f32) as u8;

          unsafe {
            ptr::write(pos.offset(0), red);
            ptr::write(pos.offset(1), green);
            ptr::write(pos.offset(2), blue);
            ptr::write(pos.offset(3), alpha);

            pos = pos.offset(4);
          }
        }
      }
    },
    (GL_RGBA_INTEGER, GL_UNSIGNED_INT) => {
      assert!(current.read_framebuffer != 0);
      let mut pos = pixels as *mut u32;
      let framebuffer = current.framebuffers.get(&current.read_framebuffer).unwrap().as_ref().unwrap().read().unwrap();
      let texture = framebuffer.color0.as_ref().unwrap();
      let tex_pos = texture.buffer.as_ptr() as *const u32;

      for y in y..(y+height) {
        for x in x..(x+width) {
          unsafe {
            let pixel = y * texture.width as i32 + x;

            let red = *tex_pos.offset(pixel as isize);

            *pos.offset(0) = red;
            *pos.offset(1) = 0;
            *pos.offset(2) = 0;
            *pos.offset(3) = 0;

            pos = pos.offset(4);
          }
        }
      }
    },
    (f, t) => unimplemented!("{:x} {:x}", f, t),
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glReadnPixels(x: GLint, y: GLint, width: GLsizei, height: GLsizei, format: GLenum, type_: GLenum, bufSize: GLsizei, data: *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glReleaseShaderCompiler() -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glRenderbufferStorage(target: GLenum, internalformat: GLenum, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glRenderbufferStorageMultisample(target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glResumeTransformFeedback(
) -> () {
  let current = current();
  current.transform_feedback_paused = false;
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSampleCoverage(value: GLfloat, invert: GLboolean) -> () {
  current().sample_coverage = (value, invert == GL_TRUE);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSampleMaski(maskNumber: GLuint, mask: GLbitfield) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSamplerParameterIiv(sampler: GLuint, pname: GLenum, param: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSamplerParameterIuiv(sampler: GLuint, pname: GLenum, param: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSamplerParameterf(sampler: GLuint, pname: GLenum, param: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSamplerParameterfv(sampler: GLuint, pname: GLenum, param: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSamplerParameteri(sampler: GLuint, pname: GLenum, param: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glSamplerParameteriv(sampler: GLuint, pname: GLenum, param: *const GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glScissor(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  let current = current();

  let draw = unsafe{ current.draw_surface.as_ref() };
  let draw = draw.unwrap();

  let width = width.min(x + width);
  let height = height.min(y + height);
  let x = x.min(draw.width).max(0);
  let y = y.min(draw.height).max(0);
  let width = width.min(draw.width - x);
  let height = height.min(draw.height - y);

  current.scissor = (x, y, width, height);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glShaderBinary(count: GLsizei, shaders: *const GLuint, binaryformat: GLenum, binary: *const c_void, length: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glShaderSource(shader: GLuint, count: GLsizei, string: *const *const GLchar, length: *const GLint) -> () {
  let current = current();

  let shader = current.shaders.get_mut(&shader).unwrap();

  let mut source = shader.source.borrow_mut();
  *source = vec![];

  for i in 0..count {
    let string = unsafe{ *string.offset(i as isize) };
    let length = if length.is_null() { -1 } else { unsafe{ *length.offset(i as isize) }};

    let bytes = if length < 0 {
      let cstr = unsafe{ CStr::from_ptr(string) };
      cstr.to_bytes()
    } else {
      unsafe{ ::std::slice::from_raw_parts(string as *const u8, length as usize) }
    };

    source.extend_from_slice(bytes);
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glStencilFunc(
  func: GLenum,
  ref_: GLint,
  mask: GLuint,
) -> () {
  glStencilFuncSeparate(GL_FRONT_AND_BACK, func, ref_, mask);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glStencilFuncSeparate(
  face: GLenum,
  func: GLenum,
  ref_: GLint,
  mask: GLuint,
) -> () {
  let current = current();

  let ref_ = cmp::max(ref_, 0);

  let func = (CmpFunc::from(func), ref_ as GLuint, mask);

  if face == GL_FRONT_AND_BACK || face == GL_FRONT {
    current.stencil_func_front = func;
  }
  if face == GL_FRONT_AND_BACK || face == GL_BACK {
    current.stencil_func_back = func;
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glStencilMask(mask: GLuint) -> () {
  assert_eq!(mask, 0xFFFF_FFFF);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glStencilMaskSeparate(face: GLenum, mask: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glStencilOp(
  fail: GLenum,
  zfail: GLenum,
  zpass: GLenum,
) -> () {
  glStencilOpSeparate(GL_FRONT_AND_BACK, fail, zfail, zpass);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glStencilOpSeparate(
  face: GLenum,
  sfail: GLenum,
  dpfail: GLenum,
  dppass: GLenum,
) -> () {
  let current = current();

  let op = (sfail, dpfail, dppass);

  if face == GL_FRONT_AND_BACK || face == GL_FRONT {
    current.stencil_op_front = op;
  }
  if face == GL_FRONT_AND_BACK || face == GL_BACK {
    current.stencil_op_back = op;
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexBuffer(target: GLenum, internalformat: GLenum, buffer: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexBufferRange(target: GLenum, internalformat: GLenum, buffer: GLuint, offset: GLintptr, size: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexImage2D(target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, border: GLint, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexImage3D(target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexParameterIiv(target: GLenum, pname: GLenum, params: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexParameterIuiv(target: GLenum, pname: GLenum, params: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexParameterf(target: GLenum, pname: GLenum, param: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexParameterfv(target: GLenum, pname: GLenum, params: *const GLfloat) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexParameteri(
  target: GLenum,
  pname: GLenum,
  param: GLint,
) -> () {
  let current = current();

  let texture = current.textures.get(&current.texture_target(target)).unwrap().as_ref().unwrap();

  match pname {
    GL_TEXTURE_MAG_FILTER => texture.mag_filter.set(param as GLenum),
    GL_TEXTURE_MIN_FILTER => texture.min_filter.set(param as GLenum),
    x => unimplemented!("{:x}", x),
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexParameteriv(target: GLenum, pname: GLenum, params: *const GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexStorage2D(
  target: GLenum,
  levels: GLsizei,
  internalformat: GLenum,
  width: GLsizei,
  height: GLsizei,
) -> () {
  let current = current();

  assert_eq!(levels, 1);

  let width = width as usize;
  let height = height as usize;
  let size = width * height;
  let byte_size = size * format_size_of(internalformat);

  let buffer = vec![0u8; byte_size];

  let texture = Texture{
    width: width,
    height: height,
    buffer: buffer,

    mag_filter: Cell::new(GL_LINEAR),
    min_filter: Cell::new(GL_NEAREST_MIPMAP_LINEAR),
  };

  let index = current.texture_target(target);
  current.textures.insert(index, Some(Arc::new(texture)));
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexStorage2DMultisample(target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, fixedsamplelocations: GLboolean) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexStorage3D(target: GLenum, levels: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexStorage3DMultisample(target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei, fixedsamplelocations: GLboolean) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexSubImage2D(
  target: GLenum,
  level: GLint,
  xoffset: GLint,
  yoffset: GLint,
  width: GLsizei,
  height: GLsizei,
  format: GLenum,
  type_: GLenum,
  pixels: *const c_void,
) -> () {
  let current = current();

  let index = current.texture_target(target);
  let texture = current.textures.get(&index).unwrap().as_ref().unwrap();

  assert_eq!(format, GL_RED_INTEGER);
  assert_eq!(type_, GL_UNSIGNED_INT);
  assert_eq!(level, 0);

  let width = width as usize;
  let height = height as usize;
  let xoffset = xoffset as usize;
  let yoffset = yoffset as usize;

  let src = pixels as *const u8;
  let dst = texture.buffer.as_ptr() as *mut u8;

  for i in 0..(width*height) {
    let x = i % width + xoffset;
    let y = i / width + yoffset;

    let target_index = y * texture.width + x;

    unsafe{ ptr::copy(src.offset(i as isize * 4),
                      dst.offset(target_index as isize * 4),
                      4); }
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTexSubImage3D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glTransformFeedbackVaryings(
  program: GLuint,
  count: GLsizei,
  varyings: *const *const GLchar,
  bufferMode: GLenum,
) -> () {
  let current = current();
  let program = current.programs.get_mut(&program).unwrap();

  let names = unsafe {
    ::std::slice::from_raw_parts(varyings, count as usize).iter()
      .map(|&v| CStr::from_ptr(v))
      .map(|c| c.to_str().unwrap().to_string())
      .collect::<Vec<_>>()
  };

  let seperate = match bufferMode {
    GL_INTERLEAVED_ATTRIBS => false,
    GL_SEPARATE_ATTRIBS => true,
    x => unimplemented!("{:x}", x),
  };

  program.pending_transform_feedback = Some((names, seperate));
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform1f(
  location: GLint,
  v0: GLfloat,
) -> () {
  let current = current();
  let program = current.programs.get_mut(&current.program).unwrap();

  program.uniform_values[location as usize] = glsl::interpret::Value::Float(v0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform1fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform1i(location: GLint, v0: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform1iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform1ui(
  location: GLint,
  v0: GLuint,
) -> () {
  let current = current();
  let program = current.programs.get_mut(&current.program).unwrap();

  program.uniform_values[location as usize] = glsl::interpret::Value::Uint(v0);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform1uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform2f(location: GLint, v0: GLfloat, v1: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform2fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform2i(location: GLint, v0: GLint, v1: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform2iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform2ui(location: GLint, v0: GLuint, v1: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform2uiv(
  location: GLint,
  count: GLsizei,
  value: *const GLuint
) -> () {
  assert_eq!(count, 1);

  let current = current();
  let program = current.programs.get_mut(&current.program).unwrap();

  let value = value as *const [GLuint; 2];
  let value = unsafe{ *value };

  program.uniform_values[location as usize] = glsl::interpret::Value::UVec2(value);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform3f(location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform3fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform3i(location: GLint, v0: GLint, v1: GLint, v2: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform3iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform3ui(location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform3uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform4f(
  location: GLint,
  v0: GLfloat,
  v1: GLfloat,
  v2: GLfloat,
  v3: GLfloat,
) -> () {

  let current = current();
  let program = current.programs.get_mut(&current.program).unwrap();

  let value = [v0, v1, v2, v3];

  program.uniform_values[location as usize] = glsl::interpret::Value::Vec4(value);

}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform4fv(
  location: GLint,
  count: GLsizei,
  value: *const GLfloat,
) -> () {
  assert_eq!(count, 1);

  let current = current();
  let program = current.programs.get_mut(&current.program).unwrap();

  let value = value as *const [GLfloat; 4];
  let value = unsafe{ *value };

  program.uniform_values[location as usize] = glsl::interpret::Value::Vec4(value);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform4i(location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform4iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform4ui(location: GLint, v0: GLuint, v1: GLuint, v2: GLuint, v3: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniform4uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformBlockBinding(
  program: GLuint,
  uniformBlockIndex: GLuint,
  uniformBlockBinding: GLuint
) -> () {
  let current = current();
  let program = current.programs.get_mut(&program).unwrap();

  program.uniform_block_bindings[uniformBlockIndex as usize - 1] = uniformBlockBinding;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix2fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix2x3fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix2x4fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix3fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix3x2fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix3x4fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix4fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix4x2fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUniformMatrix4x3fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUnmapBuffer(_target: GLenum) -> GLboolean {
  GL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUseProgram(program: GLuint) -> () {
  current().program = program;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glUseProgramStages(pipeline: GLuint, stages: GLbitfield, program: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glValidateProgram(program: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glValidateProgramPipeline(pipeline: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib1f(index: GLuint, x: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib1fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib2f(
  index: GLuint,
  x: GLfloat,
  y: GLfloat,
) -> () {
  glVertexAttrib4f(index, x, y, 0.0, 1.0)
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib2fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib3f(index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib3fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib4f(
  index: GLuint,
  x: GLfloat,
  y: GLfloat,
  z: GLfloat,
  w: GLfloat,
) -> () {
  let current = current();

  current.current_vertex_attrib[index as usize] = [x, y, z, w];
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttrib4fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribBinding(attribindex: GLuint, bindingindex: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribDivisor(
  index: GLuint,
  divisor: GLuint,
) -> () {
  let current = current();

  let binding = &mut current.vertex_arrays.get_mut(&current.vertex_array).unwrap().as_mut().unwrap().bindings[index as usize];

  binding.divisor = divisor;
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribFormat(attribindex: GLuint, size: GLint, type_: GLenum, normalized: GLboolean, relativeoffset: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribI4i(
  index: GLuint,
  x: GLint,
  y: GLint,
  z: GLint,
  w: GLint,
) -> () {
  glVertexAttrib4f(index, x as f32, y as f32, z as f32, w as f32)
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribI4iv(index: GLuint, v: *const GLint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribI4ui(
  index: GLuint,
  x: GLuint,
  y: GLuint,
  z: GLuint,
  w: GLuint,
) -> () {
  glVertexAttrib4f(index, x as f32, y as f32, z as f32, w as f32)
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribI4uiv(index: GLuint, v: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribIFormat(attribindex: GLuint, size: GLint, type_: GLenum, relativeoffset: GLuint) -> () {
  unimplemented!()
}

fn glBindVertexBufferFromVertexAttribPointer(
  index: GLuint,
  size: GLint,
  type_: GLenum,
  stride: GLsizei,
  pointer: *const c_void,
) {
  let current = current();

  let buffer = current.array_buffer;

  let offset = if buffer == 0 {
    0
  } else {
    pointer as isize
  };

  let effective_stride = if stride == 0 {
    size_of(type_) as i32 * size
  } else { stride };

  glBindVertexBuffer(index, buffer, offset, effective_stride);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribIPointer(
  index: GLuint,
  size: GLint,
  type_: GLenum,
  stride: GLsizei,
  pointer: *const c_void,
) -> () {
  let current = current();

  let attrib = &mut current.vertex_arrays.get_mut(&current.vertex_array).unwrap().as_mut().unwrap().attribs[index as usize];

  attrib.size = size;
  attrib.type_ = type_;
  attrib.normalized = false;
  attrib.stride = stride;
  attrib.pointer = pointer;

  glBindVertexBufferFromVertexAttribPointer(index, size, type_, stride, pointer);
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexAttribPointer(
  index: GLuint,
  size: GLint,
  type_: GLenum,
  normalized: GLboolean,
  stride: GLsizei,
  pointer: *const c_void,
) -> () {
  let current = current();

  let attrib = &mut current.vertex_arrays.get_mut(&current.vertex_array).unwrap().as_mut().unwrap().attribs[index as usize];

  attrib.size = size;
  attrib.type_ = type_;
  attrib.normalized = normalized == GL_TRUE;
  attrib.stride = stride;
  attrib.pointer = pointer;

  glBindVertexBufferFromVertexAttribPointer(index, size, type_, stride, pointer);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glVertexBindingDivisor(bindingindex: GLuint, divisor: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glViewport(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  current().viewport = (x, y, width, height);
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_gl", trace)]
pub extern "C" fn glWaitSync(sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> () {
  unimplemented!()
}


unsafe fn write_string(src: &str,
                       dest: *mut GLchar,
                       buf_size: GLsizei,
                       length: *mut GLsizei,
) {

  let num_to_write = (src.len() as GLsizei).min(buf_size - 1).max(0);

  if buf_size > 0 {
    ptr::copy(src.as_ptr() as *const i8, dest, num_to_write as usize);
    *dest.offset(num_to_write as isize + 1) = 0;
  }

  if !length.is_null() {
    *length = num_to_write;
  }
}

pub fn size_of(typ: GLenum) -> usize {
  match typ {
    GL_UNSIGNED_INT => mem::size_of::<GLuint>(),
    GL_UNSIGNED_INT_VEC2 => mem::size_of::<GLuint>() * 2,
    GL_UNSIGNED_INT_VEC3 => mem::size_of::<GLuint>() * 3,
    GL_UNSIGNED_INT_VEC4 => mem::size_of::<GLuint>() * 4,
    GL_FLOAT => mem::size_of::<GLfloat>(),
    GL_FLOAT_VEC2 => mem::size_of::<GLfloat>() * 2,
    GL_FLOAT_VEC3 => mem::size_of::<GLfloat>() * 3,
    GL_FLOAT_VEC4 => mem::size_of::<GLfloat>() * 4,
    x => unimplemented!("{:x}", x),
  }
}

pub fn format_size_of(typ: GLenum) -> usize {
  match typ {
    GL_R32UI => 32/8,
    x => unimplemented!("{:x}", x),
  }
}

pub fn parse_variable_name(
  name: &str
) -> (&str, Vec<u32>) {
  let mut split = name.split('[');

  let base = split.next().unwrap();

  let mut indices = vec![];
  for part in split {
    let trimmed = part.trim_end_matches(']');
    if trimmed.len() != part.len() - 1 { return (name, vec![]); }

    if let Ok(i) = trimmed.parse::<u32>() {
      indices.push(i);
    } else {
      return (name, vec![]);
    }
  }

  (base, indices)
}

#[inline]
pub(crate) fn fixed_to_float(fixed: GLfixed) -> GLfloat {
  fixed as f32 / 65536.0
}
