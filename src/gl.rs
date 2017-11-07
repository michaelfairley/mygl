 #![allow(non_snake_case)]
use std::os::raw::*;
use std::sync::mpsc;
use std::thread;
use std::ptr;
use std::ffi::{CStr};
use std::collections::HashMap;
use std::sync::Arc;
use std::cell::{RefCell,Ref};
use std::mem;

use egl::{self,Config,Surface};
use glsl;

type Rect = (GLint, GLint, GLsizei, GLsizei);
pub type ColorMask = (bool, bool, bool, bool);

const MAX_SHADER_STORAGE_BUFFER_BINDINGS: usize = 4;

#[derive(Debug)]
pub struct Context {
  pub config: &'static Config,

  pub draw_surface: *mut Surface,
  pub read_surface: *mut Surface,

  tx: mpsc::Sender<Command>,

  scissor_test: bool,
  scissor: Rect,

  color_mask: ColorMask,

  stencil_test: bool,

  depth_test: bool,

  blend: bool,

  viewport: Rect,

  clear_color: (GLfloat, GLfloat, GLfloat, GLfloat),

  // TODO: move to server?
  buffers: HashMap<GLuint, Option<Vec<u8>>>, // TODO: replace with pointer
  array_buffer: GLuint,
  element_array_buffer: GLuint,
  pixel_pack_buffer: GLuint,
  pixel_unpack_buffer: GLuint,
  uniform_buffer: GLuint,
  transform_feedback_buffer: GLuint,
  dispatch_indirect_buffer: GLuint,
  copy_read_buffer: GLuint,
  copy_write_buffer: GLuint,
  draw_indirect_buffer: GLuint,
  atomic_counter_buffer: GLuint,
  shader_storage_buffer: GLuint,
  indexed_shader_storage_buffer: [GLuint; MAX_SHADER_STORAGE_BUFFER_BINDINGS],

  vertex_array: GLuint,

  culling: bool,
  cull_face: GLenum,
  front_face: GLenum,

  programs: HashMap<GLuint, Program>,
  shaders: HashMap<GLuint, Arc<Shader>>,
  program: GLuint,


  depth_range: (GLfloat, GLfloat),
  line_width: GLfloat,
  primitive_restart_fixed_index: bool,
  polygon_offset: (GLfloat, GLfloat),
  polygon_offset_fill: bool,
  rasterizer_discard: bool,
  sample_alpha_to_coverage: bool,
  sample_coverage_enabled: bool,
  sample_coverage: (GLfloat, bool),
  sample_mask: bool,
  dither: bool,
}

impl Context {
  pub fn new(config: &'static Config) -> Self {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
      let mut server = Server::new(rx);
      server.run();
    });

    Self{
      config,
      tx,

      draw_surface: egl::EGL_NO_SURFACE,
      read_surface: egl::EGL_NO_SURFACE,

      scissor_test: false,
      scissor: (0, 0, 0, 0),

      color_mask: (true, true, true, true),

      stencil_test: false,

      depth_test: false,

      blend: false,

      viewport: (0, 0, 0, 0),

      clear_color: (0.0, 0.0, 0.0, 0.0),

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
      indexed_shader_storage_buffer: [0; MAX_SHADER_STORAGE_BUFFER_BINDINGS],

      vertex_array: 0,

      culling: false,
      cull_face: GL_BACK,
      front_face: GL_CCW,

      programs: HashMap::new(),
      shaders: HashMap::new(),
      program: 0,

      depth_range: (0.0, 1.0),
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
    }
  }

  pub fn set_surfaces(&mut self, draw: *mut Surface, read: *mut Surface) {
    self.draw_surface = draw;
    self.read_surface = read;
    self.tx.send(Command::SetSurfaces(unsafe{ draw.as_mut() }, unsafe{ read.as_mut() })).unwrap();
  }

  fn buffer_target(&self, target: GLenum) -> &GLuint {
    match target {
      GL_ARRAY_BUFFER => &self.array_buffer,
      GL_ELEMENT_ARRAY_BUFFER => &self.element_array_buffer,
      GL_PIXEL_PACK_BUFFER => &self.pixel_pack_buffer,
      GL_PIXEL_UNPACK_BUFFER => &self.pixel_unpack_buffer,
      GL_UNIFORM_BUFFER => &self.uniform_buffer,
      GL_TRANSFORM_FEEDBACK_BUFFER => &self.transform_feedback_buffer,
      GL_DISPATCH_INDIRECT_BUFFER => &self.dispatch_indirect_buffer,
      GL_COPY_READ_BUFFER => &self.copy_read_buffer,
      GL_COPY_WRITE_BUFFER => &self.copy_write_buffer,
      GL_DRAW_INDIRECT_BUFFER => &self.draw_indirect_buffer,
      GL_ATOMIC_COUNTER_BUFFER => &self.atomic_counter_buffer,
      GL_SHADER_STORAGE_BUFFER => &self.shader_storage_buffer,
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

  fn indexed_buffer_target_mut(&mut self, target: GLenum, i: usize) -> &mut GLuint {
    match target {
      GL_SHADER_STORAGE_BUFFER => &mut self.indexed_shader_storage_buffer[i],
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
        Command::Clear(color, depth, stencil, scissor, color_mask) => {
          if depth.is_some() { unimplemented!() }
          if stencil.is_some() { unimplemented!() }

          if let Some(color) = color {
            let draw = self.draw_surface.as_mut().unwrap();

            let (x, y, width, height) = scissor;

            for y in y..y+height {
              for x in x..x+width {
                draw.set_pixel(x, y, color, color_mask);
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
  Clear(Option<(f32, f32, f32, f32)>, Option<f32>, Option<f32>, Rect, ColorMask),
  Finish(mpsc::Sender<()>),
  Flush(mpsc::Sender<()>),
}

#[derive(Debug)]
pub struct Program{
  shaders: Vec<Arc<Shader>>,
  ssbos: HashMap<GLuint, glsl::ShaderStorageBlockInfo>,
}

impl Program {
  pub fn new() -> Self {
    Self{
      shaders: vec![],
      ssbos: HashMap::new(),
    }
  }
}

#[derive(Debug)]
pub struct Shader{
  type_: GLenum,
  source: RefCell<Vec<u8>>,
  compiled: RefCell<Option<glsl::Shader>>,
  info_log: RefCell<String>,
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


// Common types from OpenGL 1.1
pub type GLenum = c_uint;
pub type GLboolean = c_uchar;
pub type GLbitfield = c_uint;
pub type GLvoid = c_void;
pub type GLbyte = c_char;
pub type GLshort = c_short;
pub type GLint = c_int;
pub type GLclampx = c_int;
pub type GLubyte = c_uchar;
pub type GLushort = c_ushort;
pub type GLuint = c_uint;
pub type GLsizei = c_int;
pub type GLfloat = c_float;
pub type GLclampf = c_float;
pub type GLdouble = c_double;
pub type GLclampd = c_double;
pub type GLeglImageOES = *const c_void;
pub type GLchar = c_char;
pub type GLcharARB = c_char;

#[cfg(target_os = "macos")]
pub type GLhandleARB = *const c_void;
#[cfg(not(target_os = "macos"))]
pub type GLhandleARB = c_uint;

pub type GLhalfARB = c_ushort;
pub type GLhalf = c_ushort;

// Must be 32 bits
pub type GLfixed = GLint;

pub type GLintptr = isize;
pub type GLsizeiptr = isize;
pub type GLint64 = i64;
pub type GLuint64 = u64;
pub type GLintptrARB = isize;
pub type GLsizeiptrARB = isize;
pub type GLint64EXT = i64;
pub type GLuint64EXT = u64;

pub enum __GLsync {}
pub type GLsync = *const __GLsync;

pub type GLDEBUGPROC = extern "system" fn(source: GLenum,
                                          gltype: GLenum,
                                          id: GLuint,
                                          severity: GLenum,
                                          length: GLsizei,
                                          message: *const GLchar,
                                          userParam: *mut c_void);
pub type GLDEBUGPROCARB = extern "system" fn(source: GLenum,
                                             gltype: GLenum,
                                             id: GLuint,
                                             severity: GLenum,
                                             length: GLsizei,
                                             message: *const GLchar,
                                             userParam: *mut c_void);
pub type GLDEBUGPROCKHR = extern "system" fn(source: GLenum,
                                             gltype: GLenum,
                                             id: GLuint,
                                             severity: GLenum,
                                             length: GLsizei,
                                             message: *const GLchar,
                                             userParam: *mut c_void);



#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92D9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_ATTRIBUTES: GLenum = 0x8B89;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_ATTRIBUTE_MAX_LENGTH: GLenum = 0x8B8A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_PROGRAM: GLenum = 0x8259;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_RESOURCES: GLenum = 0x92F5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_TEXTURE: GLenum = 0x84E0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_UNIFORMS: GLenum = 0x8B86;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_UNIFORM_BLOCKS: GLenum = 0x8A36;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH: GLenum = 0x8A35;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_UNIFORM_MAX_LENGTH: GLenum = 0x8B87;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ACTIVE_VARIABLES: GLenum = 0x9305;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALIASED_LINE_WIDTH_RANGE: GLenum = 0x846E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALIASED_POINT_SIZE_RANGE: GLenum = 0x846D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALL_BARRIER_BITS: GLenum = 0xFFFFFFFF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALL_SHADER_BITS: GLenum = 0xFFFFFFFF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALPHA: GLenum = 0x1906;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALPHA_BITS: GLenum = 0x0D55;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALREADY_SIGNALED: GLenum = 0x911A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ALWAYS: GLenum = 0x0207;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ANY_SAMPLES_PASSED: GLenum = 0x8C2F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ANY_SAMPLES_PASSED_CONSERVATIVE: GLenum = 0x8D6A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ARRAY_BUFFER: GLenum = 0x8892;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ARRAY_BUFFER_BINDING: GLenum = 0x8894;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ARRAY_SIZE: GLenum = 0x92FB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ARRAY_STRIDE: GLenum = 0x92FE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATOMIC_COUNTER_BARRIER_BIT: GLenum = 0x00001000;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATOMIC_COUNTER_BUFFER: GLenum = 0x92C0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATOMIC_COUNTER_BUFFER_BINDING: GLenum = 0x92C1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATOMIC_COUNTER_BUFFER_INDEX: GLenum = 0x9301;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATOMIC_COUNTER_BUFFER_SIZE: GLenum = 0x92C3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATOMIC_COUNTER_BUFFER_START: GLenum = 0x92C2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ATTACHED_SHADERS: GLenum = 0x8B85;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BACK: GLenum = 0x0405;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND: GLenum = 0x0BE2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_COLOR: GLenum = 0x8005;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_DST_ALPHA: GLenum = 0x80CA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_DST_RGB: GLenum = 0x80C8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_EQUATION: GLenum = 0x8009;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_EQUATION_ALPHA: GLenum = 0x883D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_EQUATION_RGB: GLenum = 0x8009;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_SRC_ALPHA: GLenum = 0x80CB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLEND_SRC_RGB: GLenum = 0x80C9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLOCK_INDEX: GLenum = 0x92FD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLUE: GLenum = 0x1905;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BLUE_BITS: GLenum = 0x0D54;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BOOL: GLenum = 0x8B56;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BOOL_VEC2: GLenum = 0x8B57;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BOOL_VEC3: GLenum = 0x8B58;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BOOL_VEC4: GLenum = 0x8B59;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER: GLenum = 0x82E0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_ACCESS_FLAGS: GLenum = 0x911F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_BINDING: GLenum = 0x9302;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_DATA_SIZE: GLenum = 0x9303;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_MAPPED: GLenum = 0x88BC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_MAP_LENGTH: GLenum = 0x9120;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_MAP_OFFSET: GLenum = 0x9121;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_MAP_POINTER: GLenum = 0x88BD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_SIZE: GLenum = 0x8764;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_UPDATE_BARRIER_BIT: GLenum = 0x00000200;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_USAGE: GLenum = 0x8765;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BUFFER_VARIABLE: GLenum = 0x92E5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_BYTE: GLenum = 0x1400;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CCW: GLenum = 0x0901;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CLAMP_TO_BORDER: GLenum = 0x812D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CLAMP_TO_EDGE: GLenum = 0x812F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR: GLenum = 0x1800;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLORBURN: GLenum = 0x929A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLORDODGE: GLenum = 0x9299;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT0: GLenum = 0x8CE0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT1: GLenum = 0x8CE1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT10: GLenum = 0x8CEA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT11: GLenum = 0x8CEB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT12: GLenum = 0x8CEC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT13: GLenum = 0x8CED;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT14: GLenum = 0x8CEE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT15: GLenum = 0x8CEF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT16: GLenum = 0x8CF0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT17: GLenum = 0x8CF1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT18: GLenum = 0x8CF2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT19: GLenum = 0x8CF3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT2: GLenum = 0x8CE2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT20: GLenum = 0x8CF4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT21: GLenum = 0x8CF5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT22: GLenum = 0x8CF6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT23: GLenum = 0x8CF7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT24: GLenum = 0x8CF8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT25: GLenum = 0x8CF9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT26: GLenum = 0x8CFA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT27: GLenum = 0x8CFB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT28: GLenum = 0x8CFC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT29: GLenum = 0x8CFD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT3: GLenum = 0x8CE3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT30: GLenum = 0x8CFE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT31: GLenum = 0x8CFF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT4: GLenum = 0x8CE4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT5: GLenum = 0x8CE5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT6: GLenum = 0x8CE6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT7: GLenum = 0x8CE7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT8: GLenum = 0x8CE8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_ATTACHMENT9: GLenum = 0x8CE9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_BUFFER_BIT: GLenum = 0x00004000;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_CLEAR_VALUE: GLenum = 0x0C22;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COLOR_WRITEMASK: GLenum = 0x0C23;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMMAND_BARRIER_BIT: GLenum = 0x00000040;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPARE_REF_TO_TEXTURE: GLenum = 0x884E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPILE_STATUS: GLenum = 0x8B81;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_R11_EAC: GLenum = 0x9270;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RG11_EAC: GLenum = 0x9272;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGB8_ETC2: GLenum = 0x9274;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2: GLenum = 0x9276;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA8_ETC2_EAC: GLenum = 0x9278;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_10x10: GLenum = 0x93BB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_10x5: GLenum = 0x93B8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_10x6: GLenum = 0x93B9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_10x8: GLenum = 0x93BA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_12x10: GLenum = 0x93BC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_12x12: GLenum = 0x93BD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_4x4: GLenum = 0x93B0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_5x4: GLenum = 0x93B1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_5x5: GLenum = 0x93B2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_6x5: GLenum = 0x93B3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_6x6: GLenum = 0x93B4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_8x5: GLenum = 0x93B5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_8x6: GLenum = 0x93B6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_RGBA_ASTC_8x8: GLenum = 0x93B7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SIGNED_R11_EAC: GLenum = 0x9271;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SIGNED_RG11_EAC: GLenum = 0x9273;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10: GLenum = 0x93DB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5: GLenum = 0x93D8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6: GLenum = 0x93D9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8: GLenum = 0x93DA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10: GLenum = 0x93DC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12: GLenum = 0x93DD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4: GLenum = 0x93D0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4: GLenum = 0x93D1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5: GLenum = 0x93D2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5: GLenum = 0x93D3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6: GLenum = 0x93D4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5: GLenum = 0x93D5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6: GLenum = 0x93D6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8: GLenum = 0x93D7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC: GLenum = 0x9279;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_ETC2: GLenum = 0x9275;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2: GLenum = 0x9277;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPRESSED_TEXTURE_FORMATS: GLenum = 0x86A3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPUTE_SHADER: GLenum = 0x91B9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPUTE_SHADER_BIT: GLenum = 0x00000020;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COMPUTE_WORK_GROUP_SIZE: GLenum = 0x8267;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONDITION_SATISFIED: GLenum = 0x911C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONSTANT_ALPHA: GLenum = 0x8003;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONSTANT_COLOR: GLenum = 0x8001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONTEXT_FLAGS: GLenum = 0x821E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONTEXT_FLAG_DEBUG_BIT: GLenum = 0x00000002;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT: GLenum = 0x00000004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CONTEXT_LOST: GLenum = 0x0507;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COPY_READ_BUFFER: GLenum = 0x8F36;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COPY_READ_BUFFER_BINDING: GLenum = 0x8F36;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COPY_WRITE_BUFFER: GLenum = 0x8F37;
#[allow(dead_code, non_upper_case_globals)] pub const GL_COPY_WRITE_BUFFER_BINDING: GLenum = 0x8F37;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CULL_FACE: GLenum = 0x0B44;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CULL_FACE_MODE: GLenum = 0x0B45;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CURRENT_PROGRAM: GLenum = 0x8B8D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CURRENT_QUERY: GLenum = 0x8865;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CURRENT_VERTEX_ATTRIB: GLenum = 0x8626;
#[allow(dead_code, non_upper_case_globals)] pub const GL_CW: GLenum = 0x0900;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DARKEN: GLenum = 0x9297;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_CALLBACK_FUNCTION: GLenum = 0x8244;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_CALLBACK_USER_PARAM: GLenum = 0x8245;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_GROUP_STACK_DEPTH: GLenum = 0x826D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_LOGGED_MESSAGES: GLenum = 0x9145;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH: GLenum = 0x8243;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_OUTPUT: GLenum = 0x92E0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_OUTPUT_SYNCHRONOUS: GLenum = 0x8242;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SEVERITY_HIGH: GLenum = 0x9146;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SEVERITY_LOW: GLenum = 0x9148;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SEVERITY_MEDIUM: GLenum = 0x9147;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SEVERITY_NOTIFICATION: GLenum = 0x826B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SOURCE_API: GLenum = 0x8246;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SOURCE_APPLICATION: GLenum = 0x824A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SOURCE_OTHER: GLenum = 0x824B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SOURCE_SHADER_COMPILER: GLenum = 0x8248;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SOURCE_THIRD_PARTY: GLenum = 0x8249;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_SOURCE_WINDOW_SYSTEM: GLenum = 0x8247;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: GLenum = 0x824D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_ERROR: GLenum = 0x824C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_MARKER: GLenum = 0x8268;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_OTHER: GLenum = 0x8251;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_PERFORMANCE: GLenum = 0x8250;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_POP_GROUP: GLenum = 0x826A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_PORTABILITY: GLenum = 0x824F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_PUSH_GROUP: GLenum = 0x8269;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR: GLenum = 0x824E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DECR: GLenum = 0x1E03;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DECR_WRAP: GLenum = 0x8508;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DELETE_STATUS: GLenum = 0x8B80;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH: GLenum = 0x1801;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH24_STENCIL8: GLenum = 0x88F0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH32F_STENCIL8: GLenum = 0x8CAD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_ATTACHMENT: GLenum = 0x8D00;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_BITS: GLenum = 0x0D56;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_BUFFER_BIT: GLenum = 0x00000100;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_CLEAR_VALUE: GLenum = 0x0B73;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_COMPONENT: GLenum = 0x1902;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_COMPONENT16: GLenum = 0x81A5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_COMPONENT24: GLenum = 0x81A6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_COMPONENT32F: GLenum = 0x8CAC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_FUNC: GLenum = 0x0B74;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_RANGE: GLenum = 0x0B70;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_STENCIL: GLenum = 0x84F9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_STENCIL_ATTACHMENT: GLenum = 0x821A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_STENCIL_TEXTURE_MODE: GLenum = 0x90EA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_TEST: GLenum = 0x0B71;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DEPTH_WRITEMASK: GLenum = 0x0B72;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DIFFERENCE: GLenum = 0x929E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DISPATCH_INDIRECT_BUFFER: GLenum = 0x90EE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DISPATCH_INDIRECT_BUFFER_BINDING: GLenum = 0x90EF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DITHER: GLenum = 0x0BD0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DONT_CARE: GLenum = 0x1100;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER0: GLenum = 0x8825;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER1: GLenum = 0x8826;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER10: GLenum = 0x882F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER11: GLenum = 0x8830;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER12: GLenum = 0x8831;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER13: GLenum = 0x8832;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER14: GLenum = 0x8833;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER15: GLenum = 0x8834;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER2: GLenum = 0x8827;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER3: GLenum = 0x8828;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER4: GLenum = 0x8829;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER5: GLenum = 0x882A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER6: GLenum = 0x882B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER7: GLenum = 0x882C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER8: GLenum = 0x882D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_BUFFER9: GLenum = 0x882E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_FRAMEBUFFER: GLenum = 0x8CA9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_FRAMEBUFFER_BINDING: GLenum = 0x8CA6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_INDIRECT_BUFFER: GLenum = 0x8F3F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DRAW_INDIRECT_BUFFER_BINDING: GLenum = 0x8F43;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DST_ALPHA: GLenum = 0x0304;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DST_COLOR: GLenum = 0x0306;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DYNAMIC_COPY: GLenum = 0x88EA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DYNAMIC_DRAW: GLenum = 0x88E8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_DYNAMIC_READ: GLenum = 0x88E9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ELEMENT_ARRAY_BARRIER_BIT: GLenum = 0x00000002;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ELEMENT_ARRAY_BUFFER: GLenum = 0x8893;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ELEMENT_ARRAY_BUFFER_BINDING: GLenum = 0x8895;
#[allow(dead_code, non_upper_case_globals)] pub const GL_EQUAL: GLenum = 0x0202;
#[allow(dead_code, non_upper_case_globals)] pub const GL_EXCLUSION: GLenum = 0x92A0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_EXTENSIONS: GLenum = 0x1F03;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FALSE: GLboolean = 0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FASTEST: GLenum = 0x1101;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FIRST_VERTEX_CONVENTION: GLenum = 0x8E4D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FIXED: GLenum = 0x140C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT: GLenum = 0x1406;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_32_UNSIGNED_INT_24_8_REV: GLenum = 0x8DAD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT2: GLenum = 0x8B5A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT2x3: GLenum = 0x8B65;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT2x4: GLenum = 0x8B66;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT3: GLenum = 0x8B5B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT3x2: GLenum = 0x8B67;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT3x4: GLenum = 0x8B68;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT4: GLenum = 0x8B5C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT4x2: GLenum = 0x8B69;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_MAT4x3: GLenum = 0x8B6A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_VEC2: GLenum = 0x8B50;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_VEC3: GLenum = 0x8B51;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FLOAT_VEC4: GLenum = 0x8B52;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRACTIONAL_EVEN: GLenum = 0x8E7C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRACTIONAL_ODD: GLenum = 0x8E7B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAGMENT_INTERPOLATION_OFFSET_BITS: GLenum = 0x8E5D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAGMENT_SHADER: GLenum = 0x8B30;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAGMENT_SHADER_BIT: GLenum = 0x00000002;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAGMENT_SHADER_DERIVATIVE_HINT: GLenum = 0x8B8B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER: GLenum = 0x8D40;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE: GLenum = 0x8215;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE: GLenum = 0x8214;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING: GLenum = 0x8210;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE: GLenum = 0x8211;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE: GLenum = 0x8216;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE: GLenum = 0x8213;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_LAYERED: GLenum = 0x8DA7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME: GLenum = 0x8CD1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE: GLenum = 0x8CD0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE: GLenum = 0x8212;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE: GLenum = 0x8217;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE: GLenum = 0x8CD3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER: GLenum = 0x8CD4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL: GLenum = 0x8CD2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_BARRIER_BIT: GLenum = 0x00000400;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_BINDING: GLenum = 0x8CA6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_COMPLETE: GLenum = 0x8CD5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_DEFAULT: GLenum = 0x8218;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS: GLenum = 0x9314;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_DEFAULT_HEIGHT: GLenum = 0x9311;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_DEFAULT_LAYERS: GLenum = 0x9312;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_DEFAULT_SAMPLES: GLenum = 0x9313;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_DEFAULT_WIDTH: GLenum = 0x9310;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: GLenum = 0x8CD6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS: GLenum = 0x8CD9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS: GLenum = 0x8DA8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: GLenum = 0x8CD7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: GLenum = 0x8D56;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_UNDEFINED: GLenum = 0x8219;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRAMEBUFFER_UNSUPPORTED: GLenum = 0x8CDD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRONT: GLenum = 0x0404;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRONT_AND_BACK: GLenum = 0x0408;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FRONT_FACE: GLenum = 0x0B46;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FUNC_ADD: GLenum = 0x8006;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FUNC_REVERSE_SUBTRACT: GLenum = 0x800B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_FUNC_SUBTRACT: GLenum = 0x800A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GENERATE_MIPMAP_HINT: GLenum = 0x8192;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEOMETRY_INPUT_TYPE: GLenum = 0x8917;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEOMETRY_OUTPUT_TYPE: GLenum = 0x8918;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEOMETRY_SHADER: GLenum = 0x8DD9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEOMETRY_SHADER_BIT: GLenum = 0x00000004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEOMETRY_SHADER_INVOCATIONS: GLenum = 0x887F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEOMETRY_VERTICES_OUT: GLenum = 0x8916;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GEQUAL: GLenum = 0x0206;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GREATER: GLenum = 0x0204;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GREEN: GLenum = 0x1904;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GREEN_BITS: GLenum = 0x0D53;
#[allow(dead_code, non_upper_case_globals)] pub const GL_GUILTY_CONTEXT_RESET: GLenum = 0x8253;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HALF_FLOAT: GLenum = 0x140B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HARDLIGHT: GLenum = 0x929B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HIGH_FLOAT: GLenum = 0x8DF2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HIGH_INT: GLenum = 0x8DF5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HSL_COLOR: GLenum = 0x92AF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HSL_HUE: GLenum = 0x92AD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HSL_LUMINOSITY: GLenum = 0x92B0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_HSL_SATURATION: GLenum = 0x92AE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_2D: GLenum = 0x904D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_2D_ARRAY: GLenum = 0x9053;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_3D: GLenum = 0x904E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BINDING_ACCESS: GLenum = 0x8F3E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BINDING_FORMAT: GLenum = 0x906E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BINDING_LAYER: GLenum = 0x8F3D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BINDING_LAYERED: GLenum = 0x8F3C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BINDING_LEVEL: GLenum = 0x8F3B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BINDING_NAME: GLenum = 0x8F3A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_BUFFER: GLenum = 0x9051;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_CUBE: GLenum = 0x9050;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_CUBE_MAP_ARRAY: GLenum = 0x9054;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS: GLenum = 0x90C9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE: GLenum = 0x90C8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMAGE_FORMAT_COMPATIBILITY_TYPE: GLenum = 0x90C7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMPLEMENTATION_COLOR_READ_FORMAT: GLenum = 0x8B9B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IMPLEMENTATION_COLOR_READ_TYPE: GLenum = 0x8B9A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INCR: GLenum = 0x1E02;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INCR_WRAP: GLenum = 0x8507;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INFO_LOG_LENGTH: GLenum = 0x8B84;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INNOCENT_CONTEXT_RESET: GLenum = 0x8254;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT: GLenum = 0x1404;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INTERLEAVED_ATTRIBS: GLenum = 0x8C8C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_2_10_10_10_REV: GLenum = 0x8D9F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_IMAGE_2D: GLenum = 0x9058;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_IMAGE_2D_ARRAY: GLenum = 0x905E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_IMAGE_3D: GLenum = 0x9059;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_IMAGE_BUFFER: GLenum = 0x905C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_IMAGE_CUBE: GLenum = 0x905B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_IMAGE_CUBE_MAP_ARRAY: GLenum = 0x905F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_2D: GLenum = 0x8DCA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_2D_ARRAY: GLenum = 0x8DCF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_2D_MULTISAMPLE: GLenum = 0x9109;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLenum = 0x910C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_3D: GLenum = 0x8DCB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_BUFFER: GLenum = 0x8DD0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_CUBE: GLenum = 0x8DCC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_SAMPLER_CUBE_MAP_ARRAY: GLenum = 0x900E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_VEC2: GLenum = 0x8B53;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_VEC3: GLenum = 0x8B54;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INT_VEC4: GLenum = 0x8B55;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INVALID_ENUM: GLenum = 0x0500;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INVALID_FRAMEBUFFER_OPERATION: GLenum = 0x0506;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INVALID_INDEX: GLuint = 0xFFFFFFFF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INVALID_OPERATION: GLenum = 0x0502;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INVALID_VALUE: GLenum = 0x0501;
#[allow(dead_code, non_upper_case_globals)] pub const GL_INVERT: GLenum = 0x150A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ISOLINES: GLenum = 0x8E7A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IS_PER_PATCH: GLenum = 0x92E7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_IS_ROW_MAJOR: GLenum = 0x9300;
#[allow(dead_code, non_upper_case_globals)] pub const GL_KEEP: GLenum = 0x1E00;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LAST_VERTEX_CONVENTION: GLenum = 0x8E4E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LAYER_PROVOKING_VERTEX: GLenum = 0x825E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LEQUAL: GLenum = 0x0203;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LESS: GLenum = 0x0201;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LIGHTEN: GLenum = 0x9298;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINEAR: GLenum = 0x2601;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINEAR_MIPMAP_LINEAR: GLenum = 0x2703;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINEAR_MIPMAP_NEAREST: GLenum = 0x2701;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINES: GLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINES_ADJACENCY: GLenum = 0x000A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINE_LOOP: GLenum = 0x0002;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINE_STRIP: GLenum = 0x0003;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINE_STRIP_ADJACENCY: GLenum = 0x000B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINE_WIDTH: GLenum = 0x0B21;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LINK_STATUS: GLenum = 0x8B82;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LOCATION: GLenum = 0x930E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LOSE_CONTEXT_ON_RESET: GLenum = 0x8252;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LOW_FLOAT: GLenum = 0x8DF0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LOW_INT: GLenum = 0x8DF3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LUMINANCE: GLenum = 0x1909;
#[allow(dead_code, non_upper_case_globals)] pub const GL_LUMINANCE_ALPHA: GLenum = 0x190A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAJOR_VERSION: GLenum = 0x821B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAP_FLUSH_EXPLICIT_BIT: GLenum = 0x0010;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAP_INVALIDATE_BUFFER_BIT: GLenum = 0x0008;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAP_INVALIDATE_RANGE_BIT: GLenum = 0x0004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAP_READ_BIT: GLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAP_UNSYNCHRONIZED_BIT: GLenum = 0x0020;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAP_WRITE_BIT: GLenum = 0x0002;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MATRIX_STRIDE: GLenum = 0x92FF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX: GLenum = 0x8008;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_3D_TEXTURE_SIZE: GLenum = 0x8073;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_ARRAY_TEXTURE_LAYERS: GLenum = 0x88FF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS: GLenum = 0x92DC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE: GLenum = 0x92D8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COLOR_ATTACHMENTS: GLenum = 0x8CDF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COLOR_TEXTURE_SAMPLES: GLenum = 0x910E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_ATOMIC_COUNTERS: GLenum = 0x92D7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92D1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS: GLenum = 0x8266;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS: GLenum = 0x8A33;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS: GLenum = 0x8A32;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_IMAGE_UNIFORMS: GLenum = 0x90CF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES: GLenum = 0x8F39;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS: GLenum = 0x90DC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS: GLenum = 0x8E1E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS: GLenum = 0x8E1F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS: GLenum = 0x8B4D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_UNIFORM_BLOCKS: GLenum = 0x8A2E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS: GLenum = 0x8A31;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_ATOMIC_COUNTERS: GLenum = 0x8265;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS: GLenum = 0x8264;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_IMAGE_UNIFORMS: GLenum = 0x91BD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS: GLenum = 0x90DB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_SHARED_MEMORY_SIZE: GLenum = 0x8262;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS: GLenum = 0x91BC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_UNIFORM_BLOCKS: GLenum = 0x91BB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_UNIFORM_COMPONENTS: GLenum = 0x8263;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_WORK_GROUP_COUNT: GLenum = 0x91BE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS: GLenum = 0x90EB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_COMPUTE_WORK_GROUP_SIZE: GLenum = 0x91BF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_CUBE_MAP_TEXTURE_SIZE: GLenum = 0x851C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_DEBUG_GROUP_STACK_DEPTH: GLenum = 0x826C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_DEBUG_LOGGED_MESSAGES: GLenum = 0x9144;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_DEBUG_MESSAGE_LENGTH: GLenum = 0x9143;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_DEPTH_TEXTURE_SAMPLES: GLenum = 0x910F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_DRAW_BUFFERS: GLenum = 0x8824;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_ELEMENTS_INDICES: GLenum = 0x80E9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_ELEMENTS_VERTICES: GLenum = 0x80E8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_ELEMENT_INDEX: GLenum = 0x8D6B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_ATOMIC_COUNTERS: GLenum = 0x92D6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92D0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_IMAGE_UNIFORMS: GLenum = 0x90CE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_INPUT_COMPONENTS: GLenum = 0x9125;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_INTERPOLATION_OFFSET: GLenum = 0x8E5C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS: GLenum = 0x90DA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_UNIFORM_BLOCKS: GLenum = 0x8A2D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_UNIFORM_COMPONENTS: GLenum = 0x8B49;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAGMENT_UNIFORM_VECTORS: GLenum = 0x8DFD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAMEBUFFER_HEIGHT: GLenum = 0x9316;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAMEBUFFER_LAYERS: GLenum = 0x9317;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAMEBUFFER_SAMPLES: GLenum = 0x9318;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_FRAMEBUFFER_WIDTH: GLenum = 0x9315;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_ATOMIC_COUNTERS: GLenum = 0x92D5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92CF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_IMAGE_UNIFORMS: GLenum = 0x90CD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_INPUT_COMPONENTS: GLenum = 0x9123;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_OUTPUT_COMPONENTS: GLenum = 0x9124;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_OUTPUT_VERTICES: GLenum = 0x8DE0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_SHADER_INVOCATIONS: GLenum = 0x8E5A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS: GLenum = 0x90D7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS: GLenum = 0x8C29;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS: GLenum = 0x8DE1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_UNIFORM_BLOCKS: GLenum = 0x8A2C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_GEOMETRY_UNIFORM_COMPONENTS: GLenum = 0x8DDF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_IMAGE_UNITS: GLenum = 0x8F38;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_INTEGER_SAMPLES: GLenum = 0x9110;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_LABEL_LENGTH: GLenum = 0x82E8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_NAME_LENGTH: GLenum = 0x92F6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_NUM_ACTIVE_VARIABLES: GLenum = 0x92F7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_PATCH_VERTICES: GLenum = 0x8E7D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_PROGRAM_TEXEL_OFFSET: GLenum = 0x8905;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET: GLenum = 0x8E5F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_RENDERBUFFER_SIZE: GLenum = 0x84E8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_SAMPLES: GLenum = 0x8D57;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_SAMPLE_MASK_WORDS: GLenum = 0x8E59;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_SERVER_WAIT_TIMEOUT: GLenum = 0x9111;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_SHADER_STORAGE_BLOCK_SIZE: GLenum = 0x90DE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS: GLenum = 0x90DD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS: GLenum = 0x92D3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92CD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS: GLenum = 0x90CB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_INPUT_COMPONENTS: GLenum = 0x886C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS: GLenum = 0x8E83;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS: GLenum = 0x90D8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS: GLenum = 0x8E81;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS: GLenum = 0x8E85;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS: GLenum = 0x8E89;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS: GLenum = 0x8E7F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS: GLenum = 0x92D4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92CE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS: GLenum = 0x90CC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS: GLenum = 0x886D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS: GLenum = 0x8E86;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS: GLenum = 0x90D9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS: GLenum = 0x8E82;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS: GLenum = 0x8E8A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS: GLenum = 0x8E80;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_GEN_LEVEL: GLenum = 0x8E7E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TESS_PATCH_COMPONENTS: GLenum = 0x8E84;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TEXTURE_BUFFER_SIZE: GLenum = 0x8C2B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TEXTURE_IMAGE_UNITS: GLenum = 0x8872;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TEXTURE_LOD_BIAS: GLenum = 0x84FD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TEXTURE_SIZE: GLenum = 0x0D33;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS: GLenum = 0x8C8A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS: GLenum = 0x8C8B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS: GLenum = 0x8C80;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_UNIFORM_BLOCK_SIZE: GLenum = 0x8A30;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_UNIFORM_BUFFER_BINDINGS: GLenum = 0x8A2F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_UNIFORM_LOCATIONS: GLenum = 0x826E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VARYING_COMPONENTS: GLenum = 0x8B4B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VARYING_VECTORS: GLenum = 0x8DFC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_ATOMIC_COUNTERS: GLenum = 0x92D2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS: GLenum = 0x92CC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_ATTRIBS: GLenum = 0x8869;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_ATTRIB_BINDINGS: GLenum = 0x82DA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET: GLenum = 0x82D9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_ATTRIB_STRIDE: GLenum = 0x82E5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_IMAGE_UNIFORMS: GLenum = 0x90CA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_OUTPUT_COMPONENTS: GLenum = 0x9122;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS: GLenum = 0x90D6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS: GLenum = 0x8B4C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_UNIFORM_BLOCKS: GLenum = 0x8A2B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_UNIFORM_COMPONENTS: GLenum = 0x8B4A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VERTEX_UNIFORM_VECTORS: GLenum = 0x8DFB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MAX_VIEWPORT_DIMS: GLenum = 0x0D3A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MEDIUM_FLOAT: GLenum = 0x8DF1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MEDIUM_INT: GLenum = 0x8DF4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MIN: GLenum = 0x8007;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MINOR_VERSION: GLenum = 0x821C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MIN_FRAGMENT_INTERPOLATION_OFFSET: GLenum = 0x8E5B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MIN_PROGRAM_TEXEL_OFFSET: GLenum = 0x8904;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET: GLenum = 0x8E5E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MIN_SAMPLE_SHADING_VALUE: GLenum = 0x8C37;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MIRRORED_REPEAT: GLenum = 0x8370;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MULTIPLY: GLenum = 0x9294;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY: GLenum = 0x9382;
#[allow(dead_code, non_upper_case_globals)] pub const GL_MULTISAMPLE_LINE_WIDTH_RANGE: GLenum = 0x9381;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NAME_LENGTH: GLenum = 0x92F9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NEAREST: GLenum = 0x2600;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NEAREST_MIPMAP_LINEAR: GLenum = 0x2702;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NEAREST_MIPMAP_NEAREST: GLenum = 0x2700;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NEVER: GLenum = 0x0200;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NICEST: GLenum = 0x1102;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NONE: GLenum = 0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NOTEQUAL: GLenum = 0x0205;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NO_ERROR: GLenum = 0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NO_RESET_NOTIFICATION: GLenum = 0x8261;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NUM_ACTIVE_VARIABLES: GLenum = 0x9304;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NUM_COMPRESSED_TEXTURE_FORMATS: GLenum = 0x86A2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NUM_EXTENSIONS: GLenum = 0x821D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NUM_PROGRAM_BINARY_FORMATS: GLenum = 0x87FE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NUM_SAMPLE_COUNTS: GLenum = 0x9380;
#[allow(dead_code, non_upper_case_globals)] pub const GL_NUM_SHADER_BINARY_FORMATS: GLenum = 0x8DF9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_OBJECT_TYPE: GLenum = 0x9112;
#[allow(dead_code, non_upper_case_globals)] pub const GL_OFFSET: GLenum = 0x92FC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE: GLenum = 1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE_MINUS_CONSTANT_ALPHA: GLenum = 0x8004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE_MINUS_CONSTANT_COLOR: GLenum = 0x8002;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE_MINUS_DST_ALPHA: GLenum = 0x0305;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE_MINUS_DST_COLOR: GLenum = 0x0307;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE_MINUS_SRC_ALPHA: GLenum = 0x0303;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ONE_MINUS_SRC_COLOR: GLenum = 0x0301;
#[allow(dead_code, non_upper_case_globals)] pub const GL_OUT_OF_MEMORY: GLenum = 0x0505;
#[allow(dead_code, non_upper_case_globals)] pub const GL_OVERLAY: GLenum = 0x9296;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PACK_ALIGNMENT: GLenum = 0x0D05;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PACK_ROW_LENGTH: GLenum = 0x0D02;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PACK_SKIP_PIXELS: GLenum = 0x0D04;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PACK_SKIP_ROWS: GLenum = 0x0D03;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PATCHES: GLenum = 0x000E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PATCH_VERTICES: GLenum = 0x8E72;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PIXEL_BUFFER_BARRIER_BIT: GLenum = 0x00000080;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PIXEL_PACK_BUFFER: GLenum = 0x88EB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PIXEL_PACK_BUFFER_BINDING: GLenum = 0x88ED;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PIXEL_UNPACK_BUFFER: GLenum = 0x88EC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PIXEL_UNPACK_BUFFER_BINDING: GLenum = 0x88EF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_POINTS: GLenum = 0x0000;
#[allow(dead_code, non_upper_case_globals)] pub const GL_POLYGON_OFFSET_FACTOR: GLenum = 0x8038;
#[allow(dead_code, non_upper_case_globals)] pub const GL_POLYGON_OFFSET_FILL: GLenum = 0x8037;
#[allow(dead_code, non_upper_case_globals)] pub const GL_POLYGON_OFFSET_UNITS: GLenum = 0x2A00;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PRIMITIVES_GENERATED: GLenum = 0x8C87;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PRIMITIVE_BOUNDING_BOX: GLenum = 0x92BE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PRIMITIVE_RESTART_FIXED_INDEX: GLenum = 0x8D69;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED: GLenum = 0x8221;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM: GLenum = 0x82E2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_BINARY_FORMATS: GLenum = 0x87FF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_BINARY_LENGTH: GLenum = 0x8741;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_BINARY_RETRIEVABLE_HINT: GLenum = 0x8257;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_INPUT: GLenum = 0x92E3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_OUTPUT: GLenum = 0x92E4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_PIPELINE: GLenum = 0x82E4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_PIPELINE_BINDING: GLenum = 0x825A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_PROGRAM_SEPARABLE: GLenum = 0x8258;
#[allow(dead_code, non_upper_case_globals)] pub const GL_QUADS: GLenum = 0x0007;
#[allow(dead_code, non_upper_case_globals)] pub const GL_QUERY: GLenum = 0x82E3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_QUERY_RESULT: GLenum = 0x8866;
#[allow(dead_code, non_upper_case_globals)] pub const GL_QUERY_RESULT_AVAILABLE: GLenum = 0x8867;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R11F_G11F_B10F: GLenum = 0x8C3A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R16F: GLenum = 0x822D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R16I: GLenum = 0x8233;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R16UI: GLenum = 0x8234;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R32F: GLenum = 0x822E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R32I: GLenum = 0x8235;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R32UI: GLenum = 0x8236;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R8: GLenum = 0x8229;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R8I: GLenum = 0x8231;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R8UI: GLenum = 0x8232;
#[allow(dead_code, non_upper_case_globals)] pub const GL_R8_SNORM: GLenum = 0x8F94;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RASTERIZER_DISCARD: GLenum = 0x8C89;
#[allow(dead_code, non_upper_case_globals)] pub const GL_READ_BUFFER: GLenum = 0x0C02;
#[allow(dead_code, non_upper_case_globals)] pub const GL_READ_FRAMEBUFFER: GLenum = 0x8CA8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_READ_FRAMEBUFFER_BINDING: GLenum = 0x8CAA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_READ_ONLY: GLenum = 0x88B8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_READ_WRITE: GLenum = 0x88BA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RED: GLenum = 0x1903;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RED_BITS: GLenum = 0x0D52;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RED_INTEGER: GLenum = 0x8D94;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REFERENCED_BY_COMPUTE_SHADER: GLenum = 0x930B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REFERENCED_BY_FRAGMENT_SHADER: GLenum = 0x930A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REFERENCED_BY_GEOMETRY_SHADER: GLenum = 0x9309;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REFERENCED_BY_TESS_CONTROL_SHADER: GLenum = 0x9307;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REFERENCED_BY_TESS_EVALUATION_SHADER: GLenum = 0x9308;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REFERENCED_BY_VERTEX_SHADER: GLenum = 0x9306;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER: GLenum = 0x8D41;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_ALPHA_SIZE: GLenum = 0x8D53;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_BINDING: GLenum = 0x8CA7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_BLUE_SIZE: GLenum = 0x8D52;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_DEPTH_SIZE: GLenum = 0x8D54;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_GREEN_SIZE: GLenum = 0x8D51;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_HEIGHT: GLenum = 0x8D43;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_INTERNAL_FORMAT: GLenum = 0x8D44;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_RED_SIZE: GLenum = 0x8D50;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_SAMPLES: GLenum = 0x8CAB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_STENCIL_SIZE: GLenum = 0x8D55;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERBUFFER_WIDTH: GLenum = 0x8D42;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RENDERER: GLenum = 0x1F01;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REPEAT: GLenum = 0x2901;
#[allow(dead_code, non_upper_case_globals)] pub const GL_REPLACE: GLenum = 0x1E01;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RESET_NOTIFICATION_STRATEGY: GLenum = 0x8256;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG: GLenum = 0x8227;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG16F: GLenum = 0x822F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG16I: GLenum = 0x8239;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG16UI: GLenum = 0x823A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG32F: GLenum = 0x8230;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG32I: GLenum = 0x823B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG32UI: GLenum = 0x823C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG8: GLenum = 0x822B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG8I: GLenum = 0x8237;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG8UI: GLenum = 0x8238;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG8_SNORM: GLenum = 0x8F95;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB: GLenum = 0x1907;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB10_A2: GLenum = 0x8059;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB10_A2UI: GLenum = 0x906F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB16F: GLenum = 0x881B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB16I: GLenum = 0x8D89;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB16UI: GLenum = 0x8D77;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB32F: GLenum = 0x8815;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB32I: GLenum = 0x8D83;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB32UI: GLenum = 0x8D71;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB565: GLenum = 0x8D62;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB5_A1: GLenum = 0x8057;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB8: GLenum = 0x8051;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB8I: GLenum = 0x8D8F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB8UI: GLenum = 0x8D7D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB8_SNORM: GLenum = 0x8F96;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB9_E5: GLenum = 0x8C3D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA: GLenum = 0x1908;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA16F: GLenum = 0x881A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA16I: GLenum = 0x8D88;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA16UI: GLenum = 0x8D76;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA32F: GLenum = 0x8814;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA32I: GLenum = 0x8D82;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA32UI: GLenum = 0x8D70;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA4: GLenum = 0x8056;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA8: GLenum = 0x8058;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA8I: GLenum = 0x8D8E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA8UI: GLenum = 0x8D7C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA8_SNORM: GLenum = 0x8F97;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGBA_INTEGER: GLenum = 0x8D99;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RGB_INTEGER: GLenum = 0x8D98;
#[allow(dead_code, non_upper_case_globals)] pub const GL_RG_INTEGER: GLenum = 0x8228;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER: GLenum = 0x82E6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_2D: GLenum = 0x8B5E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_2D_ARRAY: GLenum = 0x8DC1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_2D_ARRAY_SHADOW: GLenum = 0x8DC4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_2D_MULTISAMPLE: GLenum = 0x9108;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_2D_MULTISAMPLE_ARRAY: GLenum = 0x910B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_2D_SHADOW: GLenum = 0x8B62;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_3D: GLenum = 0x8B5F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_BINDING: GLenum = 0x8919;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_BUFFER: GLenum = 0x8DC2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_CUBE: GLenum = 0x8B60;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_CUBE_MAP_ARRAY: GLenum = 0x900C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW: GLenum = 0x900D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLER_CUBE_SHADOW: GLenum = 0x8DC5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLES: GLenum = 0x80A9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_ALPHA_TO_COVERAGE: GLenum = 0x809E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_BUFFERS: GLenum = 0x80A8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_COVERAGE: GLenum = 0x80A0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_COVERAGE_INVERT: GLenum = 0x80AB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_COVERAGE_VALUE: GLenum = 0x80AA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_MASK: GLenum = 0x8E51;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_MASK_VALUE: GLenum = 0x8E52;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_POSITION: GLenum = 0x8E50;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SAMPLE_SHADING: GLenum = 0x8C36;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SCISSOR_BOX: GLenum = 0x0C10;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SCISSOR_TEST: GLenum = 0x0C11;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SCREEN: GLenum = 0x9295;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SEPARATE_ATTRIBS: GLenum = 0x8C8D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER: GLenum = 0x82E1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_BINARY_FORMATS: GLenum = 0x8DF8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_COMPILER: GLenum = 0x8DFA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_IMAGE_ACCESS_BARRIER_BIT: GLenum = 0x00000020;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_SOURCE_LENGTH: GLenum = 0x8B88;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BARRIER_BIT: GLenum = 0x00002000;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BLOCK: GLenum = 0x92E6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BUFFER: GLenum = 0x90D2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BUFFER_BINDING: GLenum = 0x90D3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT: GLenum = 0x90DF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BUFFER_SIZE: GLenum = 0x90D5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_STORAGE_BUFFER_START: GLenum = 0x90D4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADER_TYPE: GLenum = 0x8B4F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHADING_LANGUAGE_VERSION: GLenum = 0x8B8C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SHORT: GLenum = 0x1402;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SIGNALED: GLenum = 0x9119;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SIGNED_NORMALIZED: GLenum = 0x8F9C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SOFTLIGHT: GLenum = 0x929C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SRC_ALPHA: GLenum = 0x0302;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SRC_ALPHA_SATURATE: GLenum = 0x0308;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SRC_COLOR: GLenum = 0x0300;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SRGB: GLenum = 0x8C40;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SRGB8: GLenum = 0x8C41;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SRGB8_ALPHA8: GLenum = 0x8C43;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STACK_OVERFLOW: GLenum = 0x0503;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STACK_UNDERFLOW: GLenum = 0x0504;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STATIC_COPY: GLenum = 0x88E6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STATIC_DRAW: GLenum = 0x88E4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STATIC_READ: GLenum = 0x88E5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL: GLenum = 0x1802;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_ATTACHMENT: GLenum = 0x8D20;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_FAIL: GLenum = 0x8801;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_FUNC: GLenum = 0x8800;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_PASS_DEPTH_FAIL: GLenum = 0x8802;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_PASS_DEPTH_PASS: GLenum = 0x8803;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_REF: GLenum = 0x8CA3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_VALUE_MASK: GLenum = 0x8CA4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BACK_WRITEMASK: GLenum = 0x8CA5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BITS: GLenum = 0x0D57;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_BUFFER_BIT: GLenum = 0x00000400;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_CLEAR_VALUE: GLenum = 0x0B91;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_FAIL: GLenum = 0x0B94;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_FUNC: GLenum = 0x0B92;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_INDEX: GLenum = 0x1901;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_INDEX8: GLenum = 0x8D48;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_PASS_DEPTH_FAIL: GLenum = 0x0B95;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_PASS_DEPTH_PASS: GLenum = 0x0B96;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_REF: GLenum = 0x0B97;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_TEST: GLenum = 0x0B90;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_VALUE_MASK: GLenum = 0x0B93;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STENCIL_WRITEMASK: GLenum = 0x0B98;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STREAM_COPY: GLenum = 0x88E2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STREAM_DRAW: GLenum = 0x88E0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_STREAM_READ: GLenum = 0x88E1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SUBPIXEL_BITS: GLenum = 0x0D50;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SYNC_CONDITION: GLenum = 0x9113;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SYNC_FENCE: GLenum = 0x9116;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SYNC_FLAGS: GLenum = 0x9115;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SYNC_FLUSH_COMMANDS_BIT: GLenum = 0x00000001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SYNC_GPU_COMMANDS_COMPLETE: GLenum = 0x9117;
#[allow(dead_code, non_upper_case_globals)] pub const GL_SYNC_STATUS: GLenum = 0x9114;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_CONTROL_OUTPUT_VERTICES: GLenum = 0x8E75;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_CONTROL_SHADER: GLenum = 0x8E88;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_CONTROL_SHADER_BIT: GLenum = 0x00000008;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_EVALUATION_SHADER: GLenum = 0x8E87;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_EVALUATION_SHADER_BIT: GLenum = 0x00000010;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_GEN_MODE: GLenum = 0x8E76;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_GEN_POINT_MODE: GLenum = 0x8E79;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_GEN_SPACING: GLenum = 0x8E77;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TESS_GEN_VERTEX_ORDER: GLenum = 0x8E78;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE: GLenum = 0x1702;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE0: GLenum = 0x84C0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE1: GLenum = 0x84C1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE10: GLenum = 0x84CA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE11: GLenum = 0x84CB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE12: GLenum = 0x84CC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE13: GLenum = 0x84CD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE14: GLenum = 0x84CE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE15: GLenum = 0x84CF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE16: GLenum = 0x84D0;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE17: GLenum = 0x84D1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE18: GLenum = 0x84D2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE19: GLenum = 0x84D3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE2: GLenum = 0x84C2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE20: GLenum = 0x84D4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE21: GLenum = 0x84D5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE22: GLenum = 0x84D6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE23: GLenum = 0x84D7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE24: GLenum = 0x84D8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE25: GLenum = 0x84D9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE26: GLenum = 0x84DA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE27: GLenum = 0x84DB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE28: GLenum = 0x84DC;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE29: GLenum = 0x84DD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE3: GLenum = 0x84C3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE30: GLenum = 0x84DE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE31: GLenum = 0x84DF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE4: GLenum = 0x84C4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE5: GLenum = 0x84C5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE6: GLenum = 0x84C6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE7: GLenum = 0x84C7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE8: GLenum = 0x84C8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE9: GLenum = 0x84C9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_2D: GLenum = 0x0DE1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_2D_ARRAY: GLenum = 0x8C1A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_2D_MULTISAMPLE: GLenum = 0x9100;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_2D_MULTISAMPLE_ARRAY: GLenum = 0x9102;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_3D: GLenum = 0x806F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_ALPHA_SIZE: GLenum = 0x805F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_ALPHA_TYPE: GLenum = 0x8C13;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BASE_LEVEL: GLenum = 0x813C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_2D: GLenum = 0x8069;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_2D_ARRAY: GLenum = 0x8C1D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_2D_MULTISAMPLE: GLenum = 0x9104;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY: GLenum = 0x9105;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_3D: GLenum = 0x806A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_BUFFER: GLenum = 0x8C2C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_CUBE_MAP: GLenum = 0x8514;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BINDING_CUBE_MAP_ARRAY: GLenum = 0x900A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BLUE_SIZE: GLenum = 0x805E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BLUE_TYPE: GLenum = 0x8C12;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BORDER_COLOR: GLenum = 0x1004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BUFFER: GLenum = 0x8C2A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BUFFER_BINDING: GLenum = 0x8C2A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BUFFER_DATA_STORE_BINDING: GLenum = 0x8C2D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BUFFER_OFFSET: GLenum = 0x919D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT: GLenum = 0x919F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_BUFFER_SIZE: GLenum = 0x919E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_COMPARE_FUNC: GLenum = 0x884D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_COMPARE_MODE: GLenum = 0x884C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_COMPRESSED: GLenum = 0x86A1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP: GLenum = 0x8513;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_ARRAY: GLenum = 0x9009;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_NEGATIVE_X: GLenum = 0x8516;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: GLenum = 0x8518;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: GLenum = 0x851A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_POSITIVE_X: GLenum = 0x8515;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_POSITIVE_Y: GLenum = 0x8517;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_CUBE_MAP_POSITIVE_Z: GLenum = 0x8519;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_DEPTH: GLenum = 0x8071;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_DEPTH_SIZE: GLenum = 0x884A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_DEPTH_TYPE: GLenum = 0x8C16;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_FETCH_BARRIER_BIT: GLenum = 0x00000008;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_FIXED_SAMPLE_LOCATIONS: GLenum = 0x9107;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_GREEN_SIZE: GLenum = 0x805D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_GREEN_TYPE: GLenum = 0x8C11;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_HEIGHT: GLenum = 0x1001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_IMMUTABLE_FORMAT: GLenum = 0x912F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_IMMUTABLE_LEVELS: GLenum = 0x82DF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_INTERNAL_FORMAT: GLenum = 0x1003;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_MAG_FILTER: GLenum = 0x2800;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_MAX_LEVEL: GLenum = 0x813D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_MAX_LOD: GLenum = 0x813B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_MIN_FILTER: GLenum = 0x2801;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_MIN_LOD: GLenum = 0x813A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_RED_SIZE: GLenum = 0x805C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_RED_TYPE: GLenum = 0x8C10;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_SAMPLES: GLenum = 0x9106;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_SHARED_SIZE: GLenum = 0x8C3F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_STENCIL_SIZE: GLenum = 0x88F1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_SWIZZLE_A: GLenum = 0x8E45;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_SWIZZLE_B: GLenum = 0x8E44;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_SWIZZLE_G: GLenum = 0x8E43;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_SWIZZLE_R: GLenum = 0x8E42;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_UPDATE_BARRIER_BIT: GLenum = 0x00000100;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_WIDTH: GLenum = 0x1000;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_WRAP_R: GLenum = 0x8072;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_WRAP_S: GLenum = 0x2802;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TEXTURE_WRAP_T: GLenum = 0x2803;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TIMEOUT_EXPIRED: GLenum = 0x911B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TIMEOUT_IGNORED: GLuint64 = 0xFFFFFFFFFFFFFFFF;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TOP_LEVEL_ARRAY_SIZE: GLenum = 0x930C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TOP_LEVEL_ARRAY_STRIDE: GLenum = 0x930D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK: GLenum = 0x8E22;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_ACTIVE: GLenum = 0x8E24;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BARRIER_BIT: GLenum = 0x00000800;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BINDING: GLenum = 0x8E25;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BUFFER: GLenum = 0x8C8E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BUFFER_BINDING: GLenum = 0x8C8F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BUFFER_MODE: GLenum = 0x8C7F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BUFFER_SIZE: GLenum = 0x8C85;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_BUFFER_START: GLenum = 0x8C84;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_PAUSED: GLenum = 0x8E23;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN: GLenum = 0x8C88;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_VARYING: GLenum = 0x92F4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_VARYINGS: GLenum = 0x8C83;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH: GLenum = 0x8C76;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRIANGLES: GLenum = 0x0004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRIANGLES_ADJACENCY: GLenum = 0x000C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRIANGLE_FAN: GLenum = 0x0006;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRIANGLE_STRIP: GLenum = 0x0005;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRIANGLE_STRIP_ADJACENCY: GLenum = 0x000D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TRUE: GLboolean = 1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_TYPE: GLenum = 0x92FA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNDEFINED_VERTEX: GLenum = 0x8260;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM: GLenum = 0x92E1;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_ARRAY_STRIDE: GLenum = 0x8A3C;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BARRIER_BIT: GLenum = 0x00000004;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK: GLenum = 0x92E2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS: GLenum = 0x8A42;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES: GLenum = 0x8A43;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_BINDING: GLenum = 0x8A3F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_DATA_SIZE: GLenum = 0x8A40;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_INDEX: GLenum = 0x8A3A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_NAME_LENGTH: GLenum = 0x8A41;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER: GLenum = 0x8A46;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER: GLenum = 0x8A44;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BUFFER: GLenum = 0x8A11;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BUFFER_BINDING: GLenum = 0x8A28;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT: GLenum = 0x8A34;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BUFFER_SIZE: GLenum = 0x8A2A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_BUFFER_START: GLenum = 0x8A29;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_IS_ROW_MAJOR: GLenum = 0x8A3E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_MATRIX_STRIDE: GLenum = 0x8A3D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_NAME_LENGTH: GLenum = 0x8A39;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_OFFSET: GLenum = 0x8A3B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_SIZE: GLenum = 0x8A38;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNIFORM_TYPE: GLenum = 0x8A37;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNKNOWN_CONTEXT_RESET: GLenum = 0x8255;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNPACK_ALIGNMENT: GLenum = 0x0CF5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNPACK_IMAGE_HEIGHT: GLenum = 0x806E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNPACK_ROW_LENGTH: GLenum = 0x0CF2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNPACK_SKIP_IMAGES: GLenum = 0x806D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNPACK_SKIP_PIXELS: GLenum = 0x0CF4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNPACK_SKIP_ROWS: GLenum = 0x0CF3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNALED: GLenum = 0x9118;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_BYTE: GLenum = 0x1401;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT: GLenum = 0x1405;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_10F_11F_11F_REV: GLenum = 0x8C3B;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_24_8: GLenum = 0x84FA;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_2_10_10_10_REV: GLenum = 0x8368;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_5_9_9_9_REV: GLenum = 0x8C3E;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_ATOMIC_COUNTER: GLenum = 0x92DB;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_IMAGE_2D: GLenum = 0x9063;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_IMAGE_2D_ARRAY: GLenum = 0x9069;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_IMAGE_3D: GLenum = 0x9064;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_IMAGE_BUFFER: GLenum = 0x9067;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_IMAGE_CUBE: GLenum = 0x9066;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY: GLenum = 0x906A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_2D: GLenum = 0x8DD2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_2D_ARRAY: GLenum = 0x8DD7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE: GLenum = 0x910A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLenum = 0x910D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_3D: GLenum = 0x8DD3;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_BUFFER: GLenum = 0x8DD8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_CUBE: GLenum = 0x8DD4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY: GLenum = 0x900F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_VEC2: GLenum = 0x8DC6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_VEC3: GLenum = 0x8DC7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_INT_VEC4: GLenum = 0x8DC8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_NORMALIZED: GLenum = 0x8C17;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_SHORT: GLenum = 0x1403;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_SHORT_4_4_4_4: GLenum = 0x8033;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_SHORT_5_5_5_1: GLenum = 0x8034;
#[allow(dead_code, non_upper_case_globals)] pub const GL_UNSIGNED_SHORT_5_6_5: GLenum = 0x8363;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VALIDATE_STATUS: GLenum = 0x8B83;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VENDOR: GLenum = 0x1F00;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERSION: GLenum = 0x1F02;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ARRAY: GLenum = 0x8074;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ARRAY_BINDING: GLenum = 0x85B5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT: GLenum = 0x00000001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING: GLenum = 0x889F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_DIVISOR: GLenum = 0x88FE;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_ENABLED: GLenum = 0x8622;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_INTEGER: GLenum = 0x88FD;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_NORMALIZED: GLenum = 0x886A;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_POINTER: GLenum = 0x8645;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_SIZE: GLenum = 0x8623;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_STRIDE: GLenum = 0x8624;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_ARRAY_TYPE: GLenum = 0x8625;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_BINDING: GLenum = 0x82D4;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_ATTRIB_RELATIVE_OFFSET: GLenum = 0x82D5;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_BINDING_BUFFER: GLenum = 0x8F4F;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_BINDING_DIVISOR: GLenum = 0x82D6;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_BINDING_OFFSET: GLenum = 0x82D7;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_BINDING_STRIDE: GLenum = 0x82D8;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_SHADER: GLenum = 0x8B31;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VERTEX_SHADER_BIT: GLenum = 0x00000001;
#[allow(dead_code, non_upper_case_globals)] pub const GL_VIEWPORT: GLenum = 0x0BA2;
#[allow(dead_code, non_upper_case_globals)] pub const GL_WAIT_FAILED: GLenum = 0x911D;
#[allow(dead_code, non_upper_case_globals)] pub const GL_WRITE_ONLY: GLenum = 0x88B9;
#[allow(dead_code, non_upper_case_globals)] pub const GL_ZERO: GLenum = 0;

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn glActiveShaderProgram(pipeline: GLuint, program: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glActiveTexture(texture: GLenum) -> () {
  assert_eq!(texture, GL_TEXTURE0);
}

#[no_mangle]
pub extern "C" fn glAttachShader(program: GLuint, shader: GLuint) -> () {
  let current = current();

  let program = current.programs.get_mut(&program).unwrap();
  let shader = current.shaders.get(&shader).unwrap();

  program.shaders.push(Arc::clone(shader));
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBeginQuery(target: GLenum, id: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBeginTransformFeedback(primitiveMode: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindAttribLocation(program: GLuint, index: GLuint, name: *const GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glBindBuffer(target: GLenum, buffer: GLuint) -> () {
  let current = current();

  *current.buffer_target_mut(target) = buffer;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindBufferBase(target: GLenum, index: GLuint, buffer: GLuint) -> () {
  let current = current();

  *current.buffer_target_mut(target) = buffer;
  *current.indexed_buffer_target_mut(target, index as usize) = buffer;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindBufferRange(target: GLenum, index: GLuint, buffer: GLuint, offset: GLintptr, size: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindFramebuffer(target: GLenum, framebuffer: GLuint) -> () {
  assert_eq!(framebuffer, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindImageTexture(unit: GLuint, texture: GLuint, level: GLint, layered: GLboolean, layer: GLint, access: GLenum, format: GLenum) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glBindProgramPipeline(pipeline: GLuint) -> () {
  assert_eq!(pipeline, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindRenderbuffer(target: GLenum, renderbuffer: GLuint) -> () {
  assert_eq!(renderbuffer, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindSampler(unit: GLuint, sampler: GLuint) -> () {
  assert_eq!(sampler, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindTexture(target: GLenum, texture: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindTransformFeedback(target: GLenum, id: GLuint) -> () {
  assert_eq!(id, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindVertexArray(array: GLuint) -> () {
  current().vertex_array = array;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBindVertexBuffer(bindingindex: GLuint, buffer: GLuint, offset: GLintptr, stride: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendBarrier() -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glBlendColor(red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat) -> () {
  assert_eq!(red, 0.0);
  assert_eq!(green, 0.0);
  assert_eq!(blue, 0.0);
  assert_eq!(alpha, 0.0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendEquation(mode: GLenum) -> () {
  assert_eq!(mode, GL_FUNC_ADD);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendEquationSeparate(modeRGB: GLenum, modeAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendEquationSeparatei(buf: GLuint, modeRGB: GLenum, modeAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendEquationi(buf: GLuint, mode: GLenum) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glBlendFunc(sfactor: GLenum, dfactor: GLenum) -> () {
  assert_eq!(sfactor, GL_ONE);
  assert_eq!(dfactor, GL_ZERO);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendFuncSeparate(sfactorRGB: GLenum, dfactorRGB: GLenum, sfactorAlpha: GLenum, dfactorAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendFuncSeparatei(buf: GLuint, srcRGB: GLenum, dstRGB: GLenum, srcAlpha: GLenum, dstAlpha: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlendFunci(buf: GLuint, src: GLenum, dst: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBlitFramebuffer(srcX0: GLint, srcY0: GLint, srcX1: GLint, srcY1: GLint, dstX0: GLint, dstY0: GLint, dstX1: GLint, dstY1: GLint, mask: GLbitfield, filter: GLenum) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glBufferData(target: GLenum, size: GLsizeiptr, data: *const c_void, _usage: GLenum) -> () {
  let current = current();

  let vec = if data.is_null() {
    let mut v = Vec::with_capacity(size as usize);
    unsafe{ v.set_len(size as usize); }
    v
  } else {
    let slice = unsafe{ ::std::slice::from_raw_parts(data as *const u8, size as usize) };
    Vec::from(slice)
  };

  let name = *current.buffer_target(target);
  current.buffers.insert(name, Some(vec));
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glBufferSubData(target: GLenum, offset: GLintptr, size: GLsizeiptr, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCheckFramebufferStatus(target: GLenum) -> GLenum {
  unimplemented!()
}

#[no_mangle]
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
    if (mask & GL_DEPTH_BUFFER_BIT) > 0 { unimplemented!() } else { None },
    if (mask & GL_STENCIL_BUFFER_BIT) > 0 { unimplemented!() } else { None },
    scissor,
    current.color_mask,
  )).unwrap();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glClearBufferfi(buffer: GLenum, drawbuffer: GLint, depth: GLfloat, stencil: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glClearBufferfv(buffer: GLenum, drawbuffer: GLint, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glClearBufferiv(buffer: GLenum, drawbuffer: GLint, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glClearBufferuiv(buffer: GLenum, drawbuffer: GLint, value: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glClearColor(red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat) -> () {
  current().clear_color = (red, green, blue, alpha);
}

#[no_mangle]
pub extern "C" fn glClearDepthf(d: GLfloat) -> () {
  assert_eq!(d, 1.0);
}

#[no_mangle]
pub extern "C" fn glClearStencil(s: GLint) -> () {
  assert_eq!(s, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glClientWaitSync(sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum {
  unimplemented!()
}

#[no_mangle]
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
pub extern "C" fn glColorMaski(index: GLuint, r: GLboolean, g: GLboolean, b: GLboolean, a: GLboolean) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glCompileShader(shader: GLuint) -> () {
  let current = current();
  let shader = current.shaders.get_mut(&shader).unwrap();

  let result = glsl::compile(&shader.source.borrow(), shader.type_);

  *shader.info_log.borrow_mut() = result.as_ref().err().map(|e| e.clone()).unwrap_or_else(String::new);
  *shader.compiled.borrow_mut() = result.ok();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCompressedTexImage2D(target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, border: GLint, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCompressedTexImage3D(target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCompressedTexSubImage2D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCompressedTexSubImage3D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, imageSize: GLsizei, data: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCopyBufferSubData(readTarget: GLenum, writeTarget: GLenum, readOffset: GLintptr, writeOffset: GLintptr, size: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCopyImageSubData(srcName: GLuint, srcTarget: GLenum, srcLevel: GLint, srcX: GLint, srcY: GLint, srcZ: GLint, dstName: GLuint, dstTarget: GLenum, dstLevel: GLint, dstX: GLint, dstY: GLint, dstZ: GLint, srcWidth: GLsizei, srcHeight: GLsizei, srcDepth: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCopyTexImage2D(target: GLenum, level: GLint, internalformat: GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei, border: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCopyTexSubImage2D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCopyTexSubImage3D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glCreateProgram() -> GLuint {
  let current = current();

  let name = (1..).find(|n| !current.programs.contains_key(n)).unwrap();
  current.programs.insert(name, Program::new());

  name
}

#[no_mangle]
pub extern "C" fn glCreateShader(type_: GLenum) -> GLuint {
  let current = current();

  let name = (1..).find(|n| !current.shaders.contains_key(n)).unwrap();
  current.shaders.insert(name, Arc::new(Shader::new(type_)));

  name
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glCreateShaderProgramv(type_: GLenum, count: GLsizei, strings: *const *const GLchar) -> GLuint {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glCullFace(mode: GLenum) -> () {
  current().cull_face = mode;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDebugMessageCallback(callback: GLDEBUGPROC, userParam: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDebugMessageControl(source: GLenum, type_: GLenum, severity: GLenum, count: GLsizei, ids: *const GLuint, enabled: GLboolean) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDebugMessageInsert(source: GLenum, type_: GLenum, id: GLuint, severity: GLenum, length: GLsizei, buf: *const GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glDeleteBuffers(n: GLsizei, buffers: *const GLuint) -> () {
  let current = current();

  let buffers = unsafe{ ::std::slice::from_raw_parts(buffers, n as usize) };
  for name in buffers {
    current.buffers.remove(&name);
  }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteFramebuffers(n: GLsizei, framebuffers: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glDeleteProgram(program: GLuint) -> () {
  let current = current();

  current.programs.remove(&program);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteProgramPipelines(n: GLsizei, pipelines: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteQueries(n: GLsizei, ids: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteRenderbuffers(n: GLsizei, renderbuffers: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteSamplers(count: GLsizei, samplers: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glDeleteShader(shader: GLuint) -> () {
  let current = current();

  current.shaders.remove(&shader);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteSync(sync: GLsync) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteTextures(n: GLsizei, textures: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteTransformFeedbacks(n: GLsizei, ids: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDeleteVertexArrays(n: GLsizei, arrays: *const GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glDepthFunc(func: GLenum) -> () {
  assert_eq!(func, GL_LESS);
}

#[no_mangle]
pub extern "C" fn glDepthMask(flag: GLboolean) -> () {
  assert_eq!(flag, GL_TRUE);
}

#[no_mangle]
pub extern "C" fn glDepthRangef(n: GLfloat, f: GLfloat) -> () {
  current().depth_range = (n, f);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDetachShader(program: GLuint, shader: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glDisable(cap: GLenum) -> () {
  *current().cap_mut(cap) = false;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDisableVertexAttribArray(index: GLuint) -> () {
  // unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDisablei(target: GLenum, index: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDispatchCompute(
  num_groups_x: GLuint,
  num_groups_y: GLuint,
  num_groups_z: GLuint,
) -> () {
  use glsl::interpret::{self,Vars,Value};
  use glsl::{Interface};

  let current = current();
  let program = current.programs.get(&current.program).unwrap();

  let shader = program.shaders.iter().find(|s| s.type_ == GL_COMPUTE_SHADER).unwrap();
  let compiled = Ref::map(shader.compiled.borrow(), |s| s.as_ref().unwrap());
  let work_group_size = compiled.work_group_size.unwrap();

  for gx in 0..num_groups_x {
    for gy in 0..num_groups_y {
      for gz in 0..num_groups_z {
        for lx in 0..work_group_size[0] {
          for ly in 0..work_group_size[1] {
            for lz in 0..work_group_size[2] {

              let mut vars = Vars::new();
              vars.push();
              vars.insert("gl_NumWorkGroups".to_string(), Value::UVec3([num_groups_x,
                                                                       num_groups_y,
                                                                       num_groups_z]));
              vars.insert("gl_WorkGroupSize".to_string(), Value::UVec3(work_group_size));

              vars.insert("gl_WorkGroupID".to_string(), Value::UVec3([gx, gy, gz]));
              vars.insert("gl_LocalInvocationID".to_string(), Value::UVec3([lx, ly, lz]));

              let gid = [
                gx * work_group_size[0] + lx,
                gy * work_group_size[1] + ly,
                gz * work_group_size[2] + lz,
              ];
              vars.insert("gl_GlobalInvocationID".to_string(), Value::UVec3(gid));

              let lii = lz * work_group_size[0] * work_group_size[1]
                + ly * work_group_size[0]
                + lx;

              vars.insert("gl_LocalInvocationIndex;".to_string(), Value::Uint(lii));

              for iface in &compiled.interfaces {
                if let &Interface::ShaderStorageBlock(ref info) = iface {
                  let buffer = current.indexed_shader_storage_buffer[info.binding as usize];
                  let buffer = current.buffers.get_mut(&buffer).unwrap().as_mut().unwrap();

                  vars.insert(info.var_name.clone(), Value::Buffer(info.name.clone(), buffer.as_mut_ptr(), None))
                }
              }

              vars.push();

              let main = &compiled.functions[&"main".to_string()][0];

              interpret::execute(&main.1, &mut vars, &compiled);
            }
          }
        }
      }
    }
  }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDispatchComputeIndirect(indirect: GLintptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawArrays(mode: GLenum, first: GLint, count: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawArraysIndirect(mode: GLenum, indirect: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawArraysInstanced(mode: GLenum, first: GLint, count: GLsizei, instancecount: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glDrawBuffers(n: GLsizei, bufs: *const GLenum) -> () {
  assert_eq!(n, 1);
  assert_eq!(unsafe{ *bufs }, GL_BACK);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawElements(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawElementsBaseVertex(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void, basevertex: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawElementsIndirect(mode: GLenum, type_: GLenum, indirect: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawElementsInstanced(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void, instancecount: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawElementsInstancedBaseVertex(mode: GLenum, count: GLsizei, type_: GLenum, indices: *const c_void, instancecount: GLsizei, basevertex: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawRangeElements(mode: GLenum, start: GLuint, end: GLuint, count: GLsizei, type_: GLenum, indices: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glDrawRangeElementsBaseVertex(mode: GLenum, start: GLuint, end: GLuint, count: GLsizei, type_: GLenum, indices: *const c_void, basevertex: GLint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glEnable(cap: GLenum) -> () {
  *current().cap_mut(cap) = true;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glEnableVertexAttribArray(index: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glEnablei(target: GLenum, index: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glEndQuery(target: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glEndTransformFeedback() -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFenceSync(condition: GLenum, flags: GLbitfield) -> GLsync {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glFinish() -> () {
  let (tx, rx) = mpsc::channel();

  current().tx.send(Command::Finish(tx)).unwrap();

  rx.recv().unwrap();
}

#[no_mangle]
pub extern "C" fn glFlush() -> () {
  let (tx, rx) = mpsc::channel();

  current().tx.send(Command::Flush(tx)).unwrap();

  rx.recv().unwrap();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFlushMappedBufferRange(target: GLenum, offset: GLintptr, length: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFramebufferParameteri(target: GLenum, pname: GLenum, param: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFramebufferRenderbuffer(target: GLenum, attachment: GLenum, renderbuffertarget: GLenum, renderbuffer: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFramebufferTexture(target: GLenum, attachment: GLenum, texture: GLuint, level: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFramebufferTexture2D(target: GLenum, attachment: GLenum, textarget: GLenum, texture: GLuint, level: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glFramebufferTextureLayer(target: GLenum, attachment: GLenum, texture: GLuint, level: GLint, layer: GLint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glFrontFace(mode: GLenum) -> () {
  current().front_face = mode;
}

#[no_mangle]
pub extern "C" fn glGenBuffers(n: GLsizei, buffers: *mut GLuint) -> () {
  let current = current();

  let mut search = 1;
  for i in 0..n {
    while current.buffers.contains_key(&search) { search += 1 };
    current.buffers.insert(search, None);
    unsafe{ ptr::write(buffers.offset(i as isize), search); }
  }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenFramebuffers(n: GLsizei, framebuffers: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenProgramPipelines(n: GLsizei, pipelines: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenQueries(n: GLsizei, ids: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenRenderbuffers(n: GLsizei, renderbuffers: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenSamplers(count: GLsizei, samplers: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenTextures(n: GLsizei, textures: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenTransformFeedbacks(n: GLsizei, ids: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenVertexArrays(n: GLsizei, arrays: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGenerateMipmap(target: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetActiveAttrib(program: GLuint, index: GLuint, bufSize: GLsizei, length: *mut GLsizei, size: *mut GLint, type_: *mut GLenum, name: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetActiveUniform(program: GLuint, index: GLuint, bufSize: GLsizei, length: *mut GLsizei, size: *mut GLint, type_: *mut GLenum, name: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetActiveUniformBlockName(program: GLuint, uniformBlockIndex: GLuint, bufSize: GLsizei, length: *mut GLsizei, uniformBlockName: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetActiveUniformBlockiv(program: GLuint, uniformBlockIndex: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetActiveUniformsiv(program: GLuint, uniformCount: GLsizei, uniformIndices: *const GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetAttachedShaders(program: GLuint, maxCount: GLsizei, count: *mut GLsizei, shaders: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetAttribLocation(program: GLuint, name: *const GLchar) -> GLint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetBooleani_v(target: GLenum, index: GLuint, data: *mut GLboolean) -> () {
  unimplemented!()
}

#[no_mangle]
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
pub extern "C" fn glGetBufferParameteri64v(target: GLenum, pname: GLenum, params: *mut GLint64) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetBufferParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetBufferPointerv(target: GLenum, pname: GLenum, params: *const *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetDebugMessageLog(count: GLuint, bufSize: GLsizei, sources: *mut GLenum, types: *mut GLenum, ids: *mut GLuint, severities: *mut GLenum, lengths: *mut GLsizei, messageLog: *mut GLchar) -> GLuint {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glGetError() -> GLenum {
  GL_NONE
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetFloatv(pname: GLenum, data: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetFragDataLocation(program: GLuint, name: *const GLchar) -> GLint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetFramebufferAttachmentParameteriv(target: GLenum, attachment: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetFramebufferParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetGraphicsResetStatus() -> GLenum {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetInteger64i_v(target: GLenum, index: GLuint, data: *mut GLint64) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetInteger64v(pname: GLenum, data: *mut GLint64) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetIntegeri_v(target: GLenum, index: GLuint, data: *mut GLint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glGetIntegerv(pname: GLenum, data: *mut GLint) -> () {
  let result = match pname {
    GL_NUM_EXTENSIONS => 0,
    GL_MAX_VERTEX_ATTRIBS => 0, // TODO
    GL_MAX_SAMPLE_MASK_WORDS => 0, // TODO
    GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS => 0, // TODO
    GL_MAX_UNIFORM_BUFFER_BINDINGS => 0, // TODO
    GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS => 0, // TODO
    GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS => 0, // TODO
    GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS => 0, // TODO
    GL_MAX_IMAGE_UNITS => 0, // TODO
    x => unimplemented!("{:x}", x),
  };

  unsafe{ ptr::write(data, result) };
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetInternalformativ(target: GLenum, internalformat: GLenum, pname: GLenum, bufSize: GLsizei, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetMultisamplefv(pname: GLenum, index: GLuint, val: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetObjectLabel(identifier: GLenum, name: GLuint, bufSize: GLsizei, length: *mut GLsizei, label: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetObjectPtrLabel(ptr: *const c_void, bufSize: GLsizei, length: *mut GLsizei, label: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetPointerv(pname: GLenum, params: *const *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramBinary(program: GLuint, bufSize: GLsizei, length: *mut GLsizei, binaryFormat: *mut GLenum, binary: *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramInfoLog(program: GLuint, bufSize: GLsizei, length: *mut GLsizei, infoLog: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramInterfaceiv(program: GLuint, programInterface: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramPipelineInfoLog(pipeline: GLuint, bufSize: GLsizei, length: *mut GLsizei, infoLog: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramPipelineiv(pipeline: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramResourceIndex(program: GLuint, programInterface: GLenum, name: *const GLchar) -> GLuint {
  let current = current();

  let program = current.programs.get(&program).unwrap();

  let name = unsafe{ CStr::from_ptr(name) };
  let name = name.to_str().unwrap();

  match programInterface {
    GL_SHADER_STORAGE_BLOCK => {
      *program.ssbos.iter().find(|&(_, ref info)| {
        info.name == name
      }).unwrap().0
    },
    GL_BUFFER_VARIABLE => {
      for (_, ref info) in &program.ssbos {
        for var in &info.active_variables {
          if name == format!("{}.{}", info.name, var.name) { return var.index as GLuint };
        }
      }
      unimplemented!()
    },
    x => unimplemented!("{:x}", x),
  }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetProgramResourceLocation(program: GLuint, programInterface: GLenum, name: *const GLchar) -> GLint {
  unimplemented!()
}

#[no_mangle]
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
    GL_BUFFER_VARIABLE => { &program.ssbos.values().flat_map(|ssbo| ssbo.active_variables.iter()).find(|a| a.index == index as i32).unwrap().name },
    x => unimplemented!("{:x}", x),
  };

  unsafe{ write_string(n, name, bufSize, length) };
}

#[no_mangle]
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
      GL_SHADER_STORAGE_BLOCK => {
        let ssbo = program.ssbos.get(&index).unwrap();

        for &prop in props {
          match prop {
            GL_BUFFER_BINDING => push(ssbo.binding as GLint),
            GL_BUFFER_DATA_SIZE => push(ssbo.size as GLint),
            GL_NUM_ACTIVE_VARIABLES => push(ssbo.active_variables.len() as GLint),
            GL_ACTIVE_VARIABLES => ssbo.active_variables.iter().for_each(|v| push(v.index)),
            GL_NAME_LENGTH => push(ssbo.name.len() as GLint),
            x => unimplemented!("{:x}", x),
          };
        }
      },
      GL_BUFFER_VARIABLE => {
        let var = program.ssbos.values().flat_map(|ssbo| ssbo.active_variables.iter()).find(|a| a.index == index as i32).unwrap();
        for &prop in props {
          match prop {
            GL_BLOCK_INDEX => push(*program.ssbos.iter().find(|&(_, ref ssbo)| ssbo.active_variables.contains(var)).unwrap().0 as i32),
            GL_TYPE => push(var.type_ as i32),
            GL_ARRAY_SIZE => push(var.array_size as i32),
            GL_OFFSET => push(var.offset as i32),
            GL_ARRAY_STRIDE => {
              let s = match var.type_ {
                GL_UNSIGNED_INT => mem::size_of::<GLuint>() as i32,
                x => unimplemented!("{:x}", x),
              };
              push(s);
            },
            GL_MATRIX_STRIDE => push(0), // TODO
            GL_IS_ROW_MAJOR => push(0), // TODO
            GL_TOP_LEVEL_ARRAY_SIZE => push(1), // TODO
            GL_TOP_LEVEL_ARRAY_STRIDE => push(0), // TODO
            GL_NAME_LENGTH => push(var.name.len() as i32),
            x => unimplemented!("{:x}", x),
          };
        }
      }
      x => unimplemented!("{:x}", x),
    }
  }

  if !length.is_null() {
    unsafe{ *length = pusher.current_offset as i32; }
  }

}

#[no_mangle]
pub extern "C" fn glGetProgramiv(_program: GLuint, pname: GLenum, params: *mut GLint) -> () {
  let value = match pname {
    GL_LINK_STATUS => GL_TRUE as GLint, // TODO
    GL_INFO_LOG_LENGTH => 0, // TODO
    x => unimplemented!("{:x}", x),
  };

  unsafe{ ptr::write(params, value) };
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetQueryObjectuiv(id: GLuint, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glGetQueryiv(_target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  assert_eq!(pname, GL_CURRENT_QUERY);
  unsafe{ ptr::write(params, 0) };
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetRenderbufferParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetSamplerParameterIiv(sampler: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetSamplerParameterIuiv(sampler: GLuint, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetSamplerParameterfv(sampler: GLuint, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetSamplerParameteriv(sampler: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[no_mangle]
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
pub extern "C" fn glGetShaderPrecisionFormat(shadertype: GLenum, precisiontype: GLenum, range: *mut GLint, precision: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetShaderSource(shader: GLuint, bufSize: GLsizei, length: *mut GLsizei, source: *mut GLchar) -> () {
  unimplemented!()
}

#[no_mangle]
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

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetString(name: GLenum) -> *const GLubyte {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetStringi(name: GLenum, index: GLuint) -> *const GLubyte {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetSynciv(sync: GLsync, pname: GLenum, bufSize: GLsizei, length: *mut GLsizei, values: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTexLevelParameterfv(target: GLenum, level: GLint, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTexLevelParameteriv(target: GLenum, level: GLint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTexParameterIiv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTexParameterIuiv(target: GLenum, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTexParameterfv(target: GLenum, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTexParameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetTransformFeedbackVarying(program: GLuint, index: GLuint, bufSize: GLsizei, length: *mut GLsizei, size: *mut GLsizei, type_: *mut GLenum, name: *mut GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetUniformBlockIndex(program: GLuint, uniformBlockName: *const GLchar) -> GLuint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetUniformIndices(program: GLuint, uniformCount: GLsizei, uniformNames: *const *const GLchar, uniformIndices: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetUniformLocation(program: GLuint, name: *const GLchar) -> GLint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetUniformfv(program: GLuint, location: GLint, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetUniformiv(program: GLuint, location: GLint, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetUniformuiv(program: GLuint, location: GLint, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetVertexAttribIiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetVertexAttribIuiv(index: GLuint, pname: GLenum, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetVertexAttribPointerv(index: GLuint, pname: GLenum, pointer: *const *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetVertexAttribfv(index: GLuint, pname: GLenum, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetVertexAttribiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetnUniformfv(program: GLuint, location: GLint, bufSize: GLsizei, params: *mut GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetnUniformiv(program: GLuint, location: GLint, bufSize: GLsizei, params: *mut GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glGetnUniformuiv(program: GLuint, location: GLint, bufSize: GLsizei, params: *mut GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glHint(target: GLenum, mode: GLenum) -> () {

}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glInvalidateFramebuffer(target: GLenum, numAttachments: GLsizei, attachments: *const GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glInvalidateSubFramebuffer(target: GLenum, numAttachments: GLsizei, attachments: *const GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsBuffer(buffer: GLuint) -> GLboolean {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glIsEnabled(cap: GLenum) -> GLboolean {
  if *current().cap_mut(cap) { GL_TRUE } else { GL_FALSE }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsEnabledi(target: GLenum, index: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsFramebuffer(framebuffer: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsProgram(program: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsProgramPipeline(pipeline: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsQuery(id: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsRenderbuffer(renderbuffer: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsSampler(sampler: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsShader(shader: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsSync(sync: GLsync) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsTexture(texture: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsTransformFeedback(id: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glIsVertexArray(array: GLuint) -> GLboolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glLineWidth(width: GLfloat) -> () {
  current().line_width = width;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glLinkProgram(program: GLuint) -> () {
  let current = current();

  let program = current.programs.get_mut(&program).unwrap();

  let mut ssbos = HashMap::new();

  let mut next_variable_location = 0;

  for shader in &program.shaders {
    for iface in &shader.compiled.borrow().as_ref().unwrap().interfaces {
      if let &glsl::Interface::ShaderStorageBlock(ref info) = iface {
        let i = ssbos.len() + 1;
        let mut info = info.clone();
        for ref mut var in info.active_variables.iter_mut() {
          var.index = next_variable_location;
          next_variable_location += 1;
        }
        ssbos.insert(i as GLuint, info);
      }
    }
  }

  program.ssbos = ssbos;
}

#[no_mangle]
pub extern "C" fn glMapBufferRange(target: GLenum, offset: GLintptr, _length: GLsizeiptr, _access: GLbitfield) -> *mut c_void {
  let current = current();

  // println!("{:#?}", current);

  let target_name = *current.buffer_target(target);
  let mut buffer = current.buffers.get_mut(&target_name);
  let buffer = buffer.as_mut().unwrap().as_mut().unwrap();
  let range = &mut buffer[(offset as usize)..];
  range.as_mut_ptr() as _
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glMemoryBarrier(barriers: GLbitfield) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glMemoryBarrierByRegion(barriers: GLbitfield) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glMinSampleShading(value: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glObjectLabel(identifier: GLenum, name: GLuint, length: GLsizei, label: *const GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glObjectPtrLabel(ptr: *const c_void, length: GLsizei, label: *const GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glPatchParameteri(pname: GLenum, value: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glPauseTransformFeedback() -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glPixelStorei(pname: GLenum, param: GLint) -> () {
  // unimplemented!()
}

#[no_mangle]
pub extern "C" fn glPolygonOffset(factor: GLfloat, units: GLfloat) -> () {
  current().polygon_offset = (factor, units);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glPopDebugGroup() -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glPrimitiveBoundingBox(minX: GLfloat, minY: GLfloat, minZ: GLfloat, minW: GLfloat, maxX: GLfloat, maxY: GLfloat, maxZ: GLfloat, maxW: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramBinary(program: GLuint, binaryFormat: GLenum, binary: *const c_void, length: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramParameteri(program: GLuint, pname: GLenum, value: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform1f(program: GLuint, location: GLint, v0: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform1fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform1i(program: GLuint, location: GLint, v0: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform1iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform1ui(program: GLuint, location: GLint, v0: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform1uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform2f(program: GLuint, location: GLint, v0: GLfloat, v1: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform2fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform2i(program: GLuint, location: GLint, v0: GLint, v1: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform2iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform2ui(program: GLuint, location: GLint, v0: GLuint, v1: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform2uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform3f(program: GLuint, location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform3fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform3i(program: GLuint, location: GLint, v0: GLint, v1: GLint, v2: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform3iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform3ui(program: GLuint, location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform3uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform4f(program: GLuint, location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat, v3: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform4fv(program: GLuint, location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform4i(program: GLuint, location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform4iv(program: GLuint, location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform4ui(program: GLuint, location: GLint, v0: GLuint, v1: GLuint, v2: GLuint, v3: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniform4uiv(program: GLuint, location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix2fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix2x3fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix2x4fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix3fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix3x2fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix3x4fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix4fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix4x2fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glProgramUniformMatrix4x3fv(program: GLuint, location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glPushDebugGroup(source: GLenum, id: GLuint, length: GLsizei, message: *const GLchar) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glReadBuffer(src: GLenum) -> () {
  assert_eq!(src, GL_BACK);
}

#[no_mangle]
pub extern "C" fn glReadPixels(x: GLint, y: GLint, width: GLsizei, height: GLsizei, format: GLenum, type_: GLenum, pixels: *mut c_void) -> () {
  assert_eq!(format, GL_RGBA);
  assert_eq!(type_, GL_UNSIGNED_BYTE);

  let current = current();

  glFinish();

  let read = unsafe{ current.read_surface.as_ref() };
  let read = read.as_ref().unwrap();

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
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glReadnPixels(x: GLint, y: GLint, width: GLsizei, height: GLsizei, format: GLenum, type_: GLenum, bufSize: GLsizei, data: *mut c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glReleaseShaderCompiler() -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glRenderbufferStorage(target: GLenum, internalformat: GLenum, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glRenderbufferStorageMultisample(target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glResumeTransformFeedback() -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glSampleCoverage(value: GLfloat, invert: GLboolean) -> () {
  current().sample_coverage = (value, invert == GL_TRUE);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSampleMaski(maskNumber: GLuint, mask: GLbitfield) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSamplerParameterIiv(sampler: GLuint, pname: GLenum, param: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSamplerParameterIuiv(sampler: GLuint, pname: GLenum, param: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSamplerParameterf(sampler: GLuint, pname: GLenum, param: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSamplerParameterfv(sampler: GLuint, pname: GLenum, param: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSamplerParameteri(sampler: GLuint, pname: GLenum, param: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glSamplerParameteriv(sampler: GLuint, pname: GLenum, param: *const GLint) -> () {
  unimplemented!()
}

#[no_mangle]
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
pub extern "C" fn glShaderBinary(count: GLsizei, shaders: *const GLuint, binaryformat: GLenum, binary: *const c_void, length: GLsizei) -> () {
  unimplemented!()
}

#[no_mangle]
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
pub extern "C" fn glStencilFunc(func: GLenum, ref_: GLint, mask: GLuint) -> () {
  assert_eq!(func, GL_ALWAYS);
  assert_eq!(ref_, 0);
  assert_eq!(mask, 0xFFFF_FFFF);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glStencilFuncSeparate(face: GLenum, func: GLenum, ref_: GLint, mask: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glStencilMask(mask: GLuint) -> () {
  assert_eq!(mask, 0xFFFF_FFFF);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glStencilMaskSeparate(face: GLenum, mask: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glStencilOp(fail: GLenum, zfail: GLenum, zpass: GLenum) -> () {
  assert_eq!(fail, GL_KEEP);
  assert_eq!(zfail, GL_KEEP);
  assert_eq!(zpass, GL_KEEP);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glStencilOpSeparate(face: GLenum, sfail: GLenum, dpfail: GLenum, dppass: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexBuffer(target: GLenum, internalformat: GLenum, buffer: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexBufferRange(target: GLenum, internalformat: GLenum, buffer: GLuint, offset: GLintptr, size: GLsizeiptr) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexImage2D(target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, border: GLint, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexImage3D(target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexParameterIiv(target: GLenum, pname: GLenum, params: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexParameterIuiv(target: GLenum, pname: GLenum, params: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexParameterf(target: GLenum, pname: GLenum, param: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexParameterfv(target: GLenum, pname: GLenum, params: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexParameteri(target: GLenum, pname: GLenum, param: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexParameteriv(target: GLenum, pname: GLenum, params: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexStorage2D(target: GLenum, levels: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexStorage2DMultisample(target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, fixedsamplelocations: GLboolean) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexStorage3D(target: GLenum, levels: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexStorage3DMultisample(target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei, fixedsamplelocations: GLboolean) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexSubImage2D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTexSubImage3D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, type_: GLenum, pixels: *const c_void) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glTransformFeedbackVaryings(program: GLuint, count: GLsizei, varyings: *const *const GLchar, bufferMode: GLenum) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform1f(location: GLint, v0: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform1fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform1i(location: GLint, v0: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform1iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform1ui(location: GLint, v0: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform1uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform2f(location: GLint, v0: GLfloat, v1: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform2fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform2i(location: GLint, v0: GLint, v1: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform2iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform2ui(location: GLint, v0: GLuint, v1: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform2uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform3f(location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform3fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform3i(location: GLint, v0: GLint, v1: GLint, v2: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform3iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform3ui(location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform3uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform4f(location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat, v3: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform4fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform4i(location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform4iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform4ui(location: GLint, v0: GLuint, v1: GLuint, v2: GLuint, v3: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniform4uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformBlockBinding(program: GLuint, uniformBlockIndex: GLuint, uniformBlockBinding: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix2fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix2x3fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix2x4fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix3fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix3x2fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix3x4fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix4fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix4x2fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUniformMatrix4x3fv(location: GLint, count: GLsizei, transpose: GLboolean, value: *const GLfloat) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glUnmapBuffer(_target: GLenum) -> GLboolean {
  GL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUseProgram(program: GLuint) -> () {
  current().program = program;
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glUseProgramStages(pipeline: GLuint, stages: GLbitfield, program: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glValidateProgram(program: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glValidateProgramPipeline(pipeline: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib1f(index: GLuint, x: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib1fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib2f(index: GLuint, x: GLfloat, y: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib2fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib3f(index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib3fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib4f(index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttrib4fv(index: GLuint, v: *const GLfloat) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribBinding(attribindex: GLuint, bindingindex: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glVertexAttribDivisor(_index: GLuint, divisor: GLuint) -> () {
  assert_eq!(divisor, 0);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribFormat(attribindex: GLuint, size: GLint, type_: GLenum, normalized: GLboolean, relativeoffset: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribI4i(index: GLuint, x: GLint, y: GLint, z: GLint, w: GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribI4iv(index: GLuint, v: *const GLint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribI4ui(index: GLuint, x: GLuint, y: GLuint, z: GLuint, w: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribI4uiv(index: GLuint, v: *const GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribIFormat(attribindex: GLuint, size: GLint, type_: GLenum, relativeoffset: GLuint) -> () {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexAttribIPointer(index: GLuint, size: GLint, type_: GLenum, stride: GLsizei, pointer: *const c_void) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glVertexAttribPointer(_index: GLuint, size: GLint, type_: GLenum, normalized: GLboolean, stride: GLsizei, pointer: *const c_void) -> () {
  assert_eq!(size, 4);
  assert_eq!(type_, GL_FLOAT);
  assert_eq!(normalized, GL_FALSE);
  assert_eq!(stride, 0);
  assert!(pointer.is_null());
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn glVertexBindingDivisor(bindingindex: GLuint, divisor: GLuint) -> () {
  unimplemented!()
}

#[no_mangle]
pub extern "C" fn glViewport(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
  current().viewport = (x, y, width, height);
}

#[allow(unused_variables)]
#[no_mangle]
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
