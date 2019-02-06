#![allow(non_snake_case)]


use std::os::raw::*;
use std::fmt;
use std::mem;
use std::ptr;
use std::cell::Cell;
use gl;
use gl::Context;
use types::*;

#[cfg(feature = "trace_egl")]
use trace::trace;
#[cfg(feature = "trace_egl")]
trace::init_depth_var!();


#[repr(C)]
pub struct AttribList(*const EGLint);

impl fmt::Debug for AttribList {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let attribs = self.iter().map(|a| format!("({:x}, {:x})", a.0, a.1)).collect::<Vec<_>>();

    write!(f, "AttribList({:?})", attribs)
  }
}


impl AttribList {
  fn iter(&self) -> AttribListIterator {
    AttribListIterator{ pos: self.0 }
  }
}

struct AttribListIterator {
  pos: *const EGLint,
}

impl Iterator for AttribListIterator {
  type Item = (EGLint, EGLint);

  fn next(&mut self) -> Option<Self::Item> {
    if self.pos.is_null() {
      return None;
    }

    let first = unsafe { ptr::read(self.pos) };
    if first == EGL_NONE as EGLint {
      return None;
    }

    let second = unsafe { ptr::read(self.pos.offset(1)) };
    self.pos = unsafe { self.pos.offset(2) };

    Some((first, second))
  }
}


#[allow(dead_code, non_camel_case_types)]
pub type khronos_utime_nanoseconds_t = u64;

#[allow(dead_code, non_camel_case_types)]
pub type khronos_uint64_t = u64;

#[allow(dead_code, non_camel_case_types)]
pub type khronos_ssize_t = isize;

pub struct MyNativeDisplayType;
#[allow(dead_code)]
pub type EGLNativeDisplayType = *const MyNativeDisplayType;

pub struct MyNativePixmapType;
#[allow(dead_code)]
pub type EGLNativePixmapType = *const MyNativePixmapType;

pub struct MyNativeWindowType;
#[allow(dead_code)]
pub type EGLNativeWindowType = *const MyNativeWindowType;

// I'd really prefer for this to be i64, but the CTS typedefs its EGLint to 32 bits
pub type EGLint = i32;

// #[allow(dead_code)]
// pub type NativeDisplayType = super::NativeDisplayType;

// #[allow(dead_code)]
// pub type NativePixmapType = super::NativePixmapType;

// #[allow(dead_code)]
// pub type NativeWindowType = super::NativeWindowType;

pub struct Display;
static THE_DISPLAY: Display = Display;

impl fmt::Debug for Display {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Display")
  }
}


#[derive(Clone,Copy,PartialEq,Debug)]
pub enum ColorFormat {
  RGB888A8,
  RGB888,
  RGB444A4,
  RGB565,
  RGB555A1,
}

impl ColorFormat {
  fn sizes(self) -> (u8, u8, u8, u8) {
    match self {
      ColorFormat::RGB888A8 => (8, 8, 8, 8),
      ColorFormat::RGB888 => (8, 8, 8, 0),
      ColorFormat::RGB444A4 => (4, 4, 4, 4),
      ColorFormat::RGB565 => (5, 6, 5, 0),
      ColorFormat::RGB555A1 => (5, 5, 5, 1),
    }
  }

  #[inline]
  fn size(self) -> u8 {
    let sizes = self.sizes();
    sizes.0 + sizes.1 + sizes.2 + sizes.3
  }

  #[inline]
  fn byte_size(self) -> usize {
    let size = self.size();
    assert!(size % 8 == 0);
    size as usize / 8
  }

  #[inline]
  fn encode(self, (r, g, b, a): (f32, f32, f32, f32)) -> [u8; 4] {
    match self {
      ColorFormat::RGB565 => {
        let red = (r * 0b11111 as f32) as u8;
        let green = (g * 0b111111 as f32) as u8;
        let blue = (b * 0b11111 as f32) as u8;

        let first_byte = red << 3 | green >> 3;
        let second_byte = green << 5 | blue;
        [first_byte, second_byte, 0, 0]
      },
      ColorFormat::RGB888 => {
        [
          (r * 0xFF as f32) as u8,
          (g * 0xFF as f32) as u8,
          (b * 0xFF as f32) as u8,
          0,
        ]
      },
      ColorFormat::RGB444A4 => {
        let red = (r * 0xF as f32) as u8;
        let green = (g * 0xF as f32) as u8;
        let blue = (b * 0xF as f32) as u8;
        let alpha = (a * 0xF as f32) as u8;

        let first_byte = red << 4 | green;
        let second_byte = blue << 4 | alpha;
        [first_byte, second_byte, 0, 0]
      },
      ColorFormat::RGB888A8 => {
        [
          (r * 0xFF as f32) as u8,
          (g * 0xFF as f32) as u8,
          (b * 0xFF as f32) as u8,
          (a * 0xFF as f32) as u8,
        ]
      },
      x => unimplemented!("{:?}", x),
    }
  }

  #[inline]
  fn decode(self, bytes: [u8; 4]) -> (f32, f32, f32, f32) {
    match self {
      ColorFormat::RGB565 => {
        let red = bytes[0] >> 3;
        let green = ((bytes[0] & 0b111) << 3) | (bytes[1] >> 5);
        let blue = bytes[1] & 0b11111;

        (
          red as f32 / 0b11111 as f32,
          green as f32 / 0b111111 as f32,
          blue as f32 / 0b11111 as f32,
          1.0,
        )
      },
      ColorFormat::RGB888 => {
        (
          bytes[0] as f32 / 0xFF as f32,
          bytes[1] as f32 / 0xFF as f32,
          bytes[2] as f32 / 0xFF as f32,
          1.0,
        )
      },
      ColorFormat::RGB444A4 => {
        let red = bytes[0] >> 4;
        let green = bytes[0] & 0b1111;
        let blue = bytes[1] >> 4;
        let alpha = bytes[1] & 0b1111;

        (
          red as f32 / 0xF as f32,
          green as f32 / 0xF as f32,
          blue as f32 / 0xF as f32,
          alpha as f32 / 0xF as f32,
        )
      },
      ColorFormat::RGB888A8 => {
        (
          bytes[0] as f32 / 0xFF as f32,
          bytes[1] as f32 / 0xFF as f32,
          bytes[2] as f32 / 0xFF as f32,
          bytes[3] as f32 / 0xFF as f32,
        )
      },
      x => unimplemented!("{:?}", x),
    }
  }
}

// TODO: move this into gl
pub struct Surface {
  pub config: &'static Config,
  pub width: EGLint,
  pub height: EGLint,
  multisample_resolve: EGLenum,
  pub color_buffer: Vec<u8>,
  pub ds_buffer: Vec<u8>,
}

impl fmt::Debug for Surface {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Surface({:?}, {}x{})", self.config, self.width, self.height)
  }
}

impl Surface {
  #[inline]
  pub fn set_pixel(&mut self,
                   x: GLint,
                   y: GLint,
                   color: (f32, f32, f32, f32),
                   mask: gl::ColorMask) {
    let fully_masked = mask.0 && mask.1 && mask.2 && mask.3;

    let color = if fully_masked {
      color
    } else {
      let old = self.get_pixel(x, y);
      (
        if mask.0 { color.0 } else { old.0 },
        if mask.1 { color.1 } else { old.1 },
        if mask.2 { color.2 } else { old.2 },
        if mask.3 { color.3 } else { old.3 },
      )
    };

    let color_format = self.config.color_format;
    let size = color_format.byte_size();
    let loc = (y as usize * self.width as usize + x as usize) * size;

    let encoded = color_format.encode(color);

    for i in 0..size {
      self.color_buffer[loc + i] = encoded[i];
    }
  }

  #[inline]
  pub fn get_pixel(&self, x: GLint, y: GLint) -> (f32, f32, f32, f32) {
    let color_format = self.config.color_format;
    let size = color_format.byte_size();
    let loc = (y as usize * self.width as usize + x as usize) * size;

    let mut bytes: [u8; 4] = unsafe{ mem::uninitialized() };
    for i in 0..size {
      bytes[i] = self.color_buffer[loc + i];
    }

    color_format.decode(bytes)
  }

  #[inline]
  pub fn set_depth(&mut self,
                   x: GLint,
                   y: GLint,
                   depth: GLfloat) {
    let loc = (y as usize * self.width as usize + x as usize) * self.config.ds_bytes();
    let size = self.config.depth_size as usize / 8;

    let encoded = match size {
      3 => {
        let whole = (depth * 0xFF_FF_FF as f32) as u32;
        let a = (whole >> 16) as u8;
        let b = (whole >> 8 ) as u8;
        let c = (whole >> 0 ) as u8;
        [a, b, c, 0]
      },
      x => unimplemented!("{}", x),
    };

    for i in 0..size {
      self.ds_buffer[loc + i] = encoded[i];
    }
  }

  #[inline]
  pub fn get_depth(
    &mut self,
    x: GLint,
    y: GLint,
  ) -> GLfloat {
    let loc = (y as usize * self.width as usize + x as usize) * self.config.ds_bytes();
    let size = self.config.depth_size as usize / 8;

    let mut bytes: [u8; 4] = unsafe{ mem::uninitialized() };
    for i in 0..size {
      bytes[i] = self.ds_buffer[loc + i];
    }


    match size {
      3 => {
        let w = ((bytes[0] as u32) << 16) | ((bytes[1] as u32) << 8) | ((bytes[2] as u32) << 0);

        w as f32 / 0xFF_FF_FF as f32
      },
      x => unimplemented!("{}", x),
    }
  }

}

thread_local! {
  static API: Cell<EGLenum> = Cell::new(EGL_OPENGL_ES_API);
  pub static CONTEXT: Cell<EGLContext> = Cell::new(EGL_NO_CONTEXT);
}


#[derive(PartialEq,Debug)]
pub struct Config {
  id: EGLint,
  pub color_format: ColorFormat,
  pub depth_size: u8,
  pub stencil_size: u8,
  pub samples: u8,
}

impl Config {
  fn sample_buffers(&self) -> u8 {
    if self.samples == 0 { 0 } else { 1 }
  }

  #[inline]
  fn ds_bytes(&self) -> usize {
    (self.depth_size + self.stencil_size) as usize / 8
  }
}

lazy_static! {
  static ref CONFIGS: Vec<Config> = {
    let color_formats = &[
      ColorFormat::RGB888A8,
      ColorFormat::RGB888,
      ColorFormat::RGB444A4,
      ColorFormat::RGB565,
      ColorFormat::RGB555A1,
    ];
    let depth_sizes = &[24, 0];
    let stencil_sizes = &[8, 0, 16];

    let samples = &[0, 4];

    let mut id = 1;

    let mut result = Vec::with_capacity(color_formats.len() * depth_sizes.len() * stencil_sizes.len() * samples.len());

    for &color_format in color_formats {
      for &depth_size in depth_sizes {
        for &stencil_size in stencil_sizes {
          for &samples in samples {
            result.push(Config{ id, color_format, depth_size, stencil_size, samples });

            id += 1;
          }
        }
      }
    }

    result
  };
}


pub type EGLBoolean = c_uint;
pub type EGLenum = c_uint;
pub type EGLAttrib = isize;
pub type EGLConfig = &'static Config;
pub type EGLContext = *mut Context;
pub type EGLDisplay = *const Display;
pub type EGLSurface = *mut Surface;
pub type EGLClientBuffer = *const c_void;
pub type EGLImage = *const c_void;
pub type EGLSync = *const c_void;
pub type EGLTime = khronos_utime_nanoseconds_t;

#[repr(C)]
pub struct EGLClientPixmapHI {
    pData: *const c_void,
    iWidth: EGLint,
    iHeight: EGLint,
    iStride: EGLint,
}



#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_FORMAT: EGLenum = 0x3088;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_FORMAT_NONPRE: EGLenum = 0x308B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_FORMAT_PRE: EGLenum = 0x308C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_MASK_SIZE: EGLenum = 0x303E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_SIZE: EGLenum = 0x3021;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BACK_BUFFER: EGLenum = 0x3084;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_ACCESS: EGLenum = 0x3002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_ALLOC: EGLenum = 0x3003;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_ATTRIBUTE: EGLenum = 0x3004;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_CONFIG: EGLenum = 0x3005;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_CONTEXT: EGLenum = 0x3006;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_CURRENT_SURFACE: EGLenum = 0x3007;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_DISPLAY: EGLenum = 0x3008;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_MATCH: EGLenum = 0x3009;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_NATIVE_PIXMAP: EGLenum = 0x300A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_NATIVE_WINDOW: EGLenum = 0x300B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_PARAMETER: EGLenum = 0x300C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_SURFACE: EGLenum = 0x300D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BIND_TO_TEXTURE_RGB: EGLenum = 0x3039;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BIND_TO_TEXTURE_RGBA: EGLenum = 0x303A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BLUE_SIZE: EGLenum = 0x3022;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BUFFER_DESTROYED: EGLenum = 0x3095;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BUFFER_PRESERVED: EGLenum = 0x3094;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BUFFER_SIZE: EGLenum = 0x3020;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CLIENT_APIS: EGLenum = 0x308D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CL_EVENT_HANDLE: EGLenum = 0x309C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLORSPACE: EGLenum = 0x3087;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLORSPACE_LINEAR: EGLenum = 0x308A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLORSPACE_sRGB: EGLenum = 0x3089;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLOR_BUFFER_TYPE: EGLenum = 0x303F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONDITION_SATISFIED: EGLenum = 0x30F6;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONFIG_CAVEAT: EGLenum = 0x3027;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONFIG_ID: EGLenum = 0x3028;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONFORMANT: EGLenum = 0x3042;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_CLIENT_TYPE: EGLenum = 0x3097;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_CLIENT_VERSION: EGLenum = 0x3098;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_LOST: EGLenum = 0x300E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_MAJOR_VERSION: EGLenum = 0x3098;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_MINOR_VERSION: EGLenum = 0x30FB;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT: EGLenum = 0x00000002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT: EGLenum = 0x00000001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_DEBUG: EGLenum = 0x31B0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE: EGLenum = 0x31B1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_PROFILE_MASK: EGLenum = 0x30FD;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY: EGLenum = 0x31BD;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_ROBUST_ACCESS: EGLenum = 0x31B2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CORE_NATIVE_ENGINE: EGLenum = 0x305B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DEFAULT_DISPLAY: EGLNativeDisplayType = 0 as EGLNativeDisplayType;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DEPTH_SIZE: EGLenum = 0x3025;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DISPLAY_SCALING: EGLenum = 10000;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DONT_CARE: EGLint = -1 as EGLint;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DRAW: EGLenum = 0x3059;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_EXTENSIONS: EGLenum = 0x3055;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_FALSE: EGLBoolean = 0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_FOREVER: u64 = 0xFFFFFFFFFFFFFFFF;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_COLORSPACE: EGLenum = 0x309D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_COLORSPACE_LINEAR: EGLenum = 0x308A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_COLORSPACE_SRGB: EGLenum = 0x3089;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_RENDERBUFFER: EGLenum = 0x30B9;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_2D: EGLenum = 0x30B1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_3D: EGLenum = 0x30B2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X: EGLenum = 0x30B4;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: EGLenum = 0x30B6;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: EGLenum = 0x30B8;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X: EGLenum = 0x30B3;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y: EGLenum = 0x30B5;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z: EGLenum = 0x30B7;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_LEVEL: EGLenum = 0x30BC;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_ZOFFSET: EGLenum = 0x30BD;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GREEN_SIZE: EGLenum = 0x3023;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_HEIGHT: EGLenum = 0x3056;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_HORIZONTAL_RESOLUTION: EGLenum = 0x3090;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_IMAGE_PRESERVED: EGLenum = 0x30D2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LARGEST_PBUFFER: EGLenum = 0x3058;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LEVEL: EGLenum = 0x3029;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LOSE_CONTEXT_ON_RESET: EGLenum = 0x31BF;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LUMINANCE_BUFFER: EGLenum = 0x308F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LUMINANCE_SIZE: EGLenum = 0x303D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MATCH_NATIVE_PIXMAP: EGLenum = 0x3041;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_PBUFFER_HEIGHT: EGLenum = 0x302A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_PBUFFER_PIXELS: EGLenum = 0x302B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_PBUFFER_WIDTH: EGLenum = 0x302C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_SWAP_INTERVAL: EGLenum = 0x303C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MIN_SWAP_INTERVAL: EGLenum = 0x303B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MIPMAP_LEVEL: EGLenum = 0x3083;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MIPMAP_TEXTURE: EGLenum = 0x3082;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE: EGLenum = 0x3099;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE_BOX: EGLenum = 0x309B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE_BOX_BIT: EGLenum = 0x0200;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE_DEFAULT: EGLenum = 0x309A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NATIVE_RENDERABLE: EGLenum = 0x302D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NATIVE_VISUAL_ID: EGLenum = 0x302E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NATIVE_VISUAL_TYPE: EGLenum = 0x302F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NONE: EGLenum = 0x3038;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NON_CONFORMANT_CONFIG: EGLenum = 0x3051;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NOT_INITIALIZED: EGLenum = 0x3001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_CONTEXT: EGLContext = 0 as EGLContext;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_DISPLAY: EGLDisplay = 0 as EGLDisplay;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_IMAGE: EGLImage = 0 as EGLImage;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_RESET_NOTIFICATION: EGLenum = 0x31BE;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_SURFACE: EGLSurface = 0 as EGLSurface;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_SYNC: EGLSync = 0 as EGLSync;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_TEXTURE: EGLenum = 0x305C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_API: EGLenum = 0x30A2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_BIT: EGLenum = 0x0008;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES2_BIT: EGLenum = 0x0004;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES3_BIT: EGLenum = 0x00000040;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES_API: EGLenum = 0x30A0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES_BIT: EGLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENVG_API: EGLenum = 0x30A1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENVG_BIT: EGLenum = 0x0002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENVG_IMAGE: EGLenum = 0x3096;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_PBUFFER_BIT: EGLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_PIXEL_ASPECT_RATIO: EGLenum = 0x3092;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_PIXMAP_BIT: EGLenum = 0x0002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_READ: EGLenum = 0x305A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RED_SIZE: EGLenum = 0x3024;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RENDERABLE_TYPE: EGLenum = 0x3040;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RENDER_BUFFER: EGLenum = 0x3086;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RGB_BUFFER: EGLenum = 0x308E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SAMPLES: EGLenum = 0x3031;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SAMPLE_BUFFERS: EGLenum = 0x3032;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SIGNALED: EGLenum = 0x30F2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SINGLE_BUFFER: EGLenum = 0x3085;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SLOW_CONFIG: EGLenum = 0x3050;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_STENCIL_SIZE: EGLenum = 0x3026;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SUCCESS: EGLenum = 0x3000;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SURFACE_TYPE: EGLenum = 0x3033;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SWAP_BEHAVIOR: EGLenum = 0x3093;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SWAP_BEHAVIOR_PRESERVED_BIT: EGLenum = 0x0400;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_CL_EVENT: EGLenum = 0x30FE;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_CL_EVENT_COMPLETE: EGLenum = 0x30FF;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_CONDITION: EGLenum = 0x30F8;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_FENCE: EGLenum = 0x30F9;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_FLUSH_COMMANDS_BIT: EGLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_PRIOR_COMMANDS_COMPLETE: EGLenum = 0x30F0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_STATUS: EGLenum = 0x30F1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_TYPE: EGLenum = 0x30F7;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_2D: EGLenum = 0x305F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_FORMAT: EGLenum = 0x3080;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_RGB: EGLenum = 0x305D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_RGBA: EGLenum = 0x305E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_TARGET: EGLenum = 0x3081;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TIMEOUT_EXPIRED: EGLenum = 0x30F5;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_BLUE_VALUE: EGLenum = 0x3035;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_GREEN_VALUE: EGLenum = 0x3036;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_RED_VALUE: EGLenum = 0x3037;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_RGB: EGLenum = 0x3052;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_TYPE: EGLenum = 0x3034;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRUE: EGLBoolean = 1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_UNKNOWN: EGLint = -1 as EGLint;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_UNSIGNALED: EGLenum = 0x30F3;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VENDOR: EGLenum = 0x3053;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VERSION: EGLenum = 0x3054;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VERTICAL_RESOLUTION: EGLenum = 0x3091;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT: EGLenum = 0x3088;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT_NONPRE: EGLenum = 0x308B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT_PRE: EGLenum = 0x308C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT_PRE_BIT: EGLenum = 0x0040;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE: EGLenum = 0x3087;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE_LINEAR: EGLenum = 0x308A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE_LINEAR_BIT: EGLenum = 0x0020;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE_sRGB: EGLenum = 0x3089;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_WIDTH: EGLenum = 0x3057;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_WINDOW_BIT: EGLenum = 0x0004;


#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglBindAPI(api: EGLenum) -> EGLBoolean {
  if api != EGL_OPENGL_ES_API {
    return EGL_FALSE;
  }

  API.with(|a| a.set(api));
  EGL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglBindTexImage(dpy: EGLDisplay, surface: EGLSurface, buffer: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub unsafe extern "C" fn eglChooseConfig(_dpy: EGLDisplay, attrib_list: AttribList, configs: *mut EGLConfig, config_size: EGLint, num_config: *mut EGLint) -> EGLBoolean {
  if num_config.is_null() {
    // TODO: set error
    return EGL_FALSE
  }

  let mut buffer_size = 0;
  let mut red_size = 0;
  let mut green_size = 0;
  let mut blue_size = 0;
  let mut luminance_size = 0;
  let mut alpha_size = 0;
  let mut alpha_mask_size = 0;
  let mut bind_to_texture_rgb = EGL_DONT_CARE;
  let mut bind_to_texture_rgba = EGL_DONT_CARE;
  let mut color_buffer_type = EGL_RGB_BUFFER as EGLint;
  let mut config_caveat = EGL_DONT_CARE;
  let mut config_id = EGL_DONT_CARE;
  let mut conformant = 0;
  let mut depth_size = 0;
  let mut level = 0;
  let mut match_native_pixmap = EGL_NONE as EGLint;
  let mut max_swap_interval = EGL_DONT_CARE;
  let mut min_swap_interval = EGL_DONT_CARE;
  let mut native_renderable = EGL_DONT_CARE;
  let mut native_visual_type = EGL_DONT_CARE;
  let mut renderable_type = EGL_OPENGL_ES_BIT as EGLint;
  let mut sample_buffers = 0;
  let mut samples = 0;
  let mut stencil_size = 0;
  let mut surface_type = EGL_WINDOW_BIT as EGLint;
  let mut transparent_type = EGL_NONE as EGLint;
  let mut transparent_red_value = EGL_DONT_CARE;
  let mut transparent_green_value = EGL_DONT_CARE;
  let mut transparent_blue_value = EGL_DONT_CARE;

  for (attrib, value) in attrib_list.iter() {
    match attrib as EGLenum {
      EGL_BUFFER_SIZE => buffer_size = value,
      EGL_RED_SIZE => red_size = value,
      EGL_GREEN_SIZE => green_size = value,
      EGL_BLUE_SIZE => blue_size = value,
      EGL_LUMINANCE_SIZE => luminance_size = value,
      EGL_ALPHA_SIZE => alpha_size = value,
      EGL_ALPHA_MASK_SIZE => alpha_mask_size = value,
      EGL_BIND_TO_TEXTURE_RGB => bind_to_texture_rgb = value,
      EGL_BIND_TO_TEXTURE_RGBA => bind_to_texture_rgba = value,
      EGL_COLOR_BUFFER_TYPE => color_buffer_type = value,
      EGL_CONFIG_CAVEAT => config_caveat = value,
      EGL_CONFIG_ID => config_id = value,
      EGL_CONFORMANT => conformant = value,
      EGL_DEPTH_SIZE => depth_size = value,
      EGL_LEVEL => level = value,
      EGL_MATCH_NATIVE_PIXMAP => match_native_pixmap = value,
      EGL_MAX_SWAP_INTERVAL => max_swap_interval = value,
      EGL_MIN_SWAP_INTERVAL => min_swap_interval = value,
      EGL_NATIVE_RENDERABLE => native_renderable = value,
      EGL_NATIVE_VISUAL_TYPE => native_visual_type = value,
      EGL_RENDERABLE_TYPE => renderable_type = value,
      EGL_SAMPLE_BUFFERS => sample_buffers = value,
      EGL_SAMPLES => samples = value,
      EGL_STENCIL_SIZE => stencil_size = value,
      EGL_SURFACE_TYPE => surface_type = value,
      EGL_TRANSPARENT_TYPE => transparent_type = value,
      EGL_TRANSPARENT_RED_VALUE => transparent_red_value = value,
      EGL_TRANSPARENT_GREEN_VALUE => transparent_green_value = value,
      EGL_TRANSPARENT_BLUE_VALUE => transparent_blue_value = value,
      x => unimplemented!("{:x}", x),
    }
  }

  let mut matching_configs: Vec<&'static Config> = if config_id != EGL_DONT_CARE {
    vec![&CONFIGS[config_id as usize - 1]]
  } else {
    CONFIGS.iter().filter(|&config| {
      (buffer_size == EGL_DONT_CARE || config.color_format.size() as EGLint >= buffer_size)
        && (red_size == EGL_DONT_CARE || config.color_format.sizes().0 as EGLint >= red_size)
        && (green_size == EGL_DONT_CARE || config.color_format.sizes().1 as EGLint >= green_size)
        && (blue_size == EGL_DONT_CARE || config.color_format.sizes().2 as EGLint >= blue_size)
        && (luminance_size == EGL_DONT_CARE || 0 >= luminance_size)
        && (alpha_size == EGL_DONT_CARE || config.color_format.sizes().3 as EGLint >= alpha_size)
        && (alpha_mask_size == EGL_DONT_CARE || 0 >= alpha_mask_size)
        && (bind_to_texture_rgb == EGL_DONT_CARE || bind_to_texture_rgb == EGL_FALSE as EGLint)
        && (bind_to_texture_rgba == EGL_DONT_CARE || bind_to_texture_rgba == EGL_FALSE as EGLint)
        && (color_buffer_type == EGL_DONT_CARE || color_buffer_type == EGL_RGB_BUFFER as EGLint)
        && (config_caveat == EGL_DONT_CARE || config_caveat == EGL_NONE as EGLint)
        && (config_id == EGL_DONT_CARE || unreachable!())
        && (conformant == EGL_DONT_CARE || ((EGL_OPENGL_ES3_BIT | EGL_OPENGL_ES2_BIT) as EGLint & conformant) == conformant)
        && (depth_size == EGL_DONT_CARE || config.depth_size as EGLint >= depth_size)
        && (level == 0)
        && (match_native_pixmap == EGL_NONE as EGLint)
        && (max_swap_interval == EGL_DONT_CARE || 0 == max_swap_interval)
        && (min_swap_interval == EGL_DONT_CARE || 0 == min_swap_interval)
        && (native_renderable == EGL_DONT_CARE || EGL_FALSE as EGLint == native_renderable)
        && (native_visual_type == EGL_DONT_CARE)
        && (renderable_type == EGL_DONT_CARE || ((EGL_OPENGL_ES3_BIT | EGL_OPENGL_ES2_BIT) as EGLint & renderable_type) == renderable_type)
        && (sample_buffers == EGL_DONT_CARE || config.sample_buffers() as EGLint >= sample_buffers)
        && (samples == EGL_DONT_CARE || config.samples as EGLint >= samples)
        && (stencil_size == EGL_DONT_CARE || config.stencil_size as EGLint >= stencil_size)
        && (surface_type == EGL_DONT_CARE || (EGL_PBUFFER_BIT as EGLint & surface_type) == surface_type)
        && (transparent_type == EGL_DONT_CARE || transparent_type == EGL_NONE as EGLint)
        && (transparent_red_value == EGL_DONT_CARE || transparent_red_value == 0)
        && (transparent_green_value == EGL_DONT_CARE || transparent_green_value == 0)
        && (transparent_blue_value == EGL_DONT_CARE || transparent_blue_value == 0)
    }).collect::<Vec<_>>()
  };

  ptr::write(num_config, matching_configs.len() as EGLint);

  if !configs.is_null() {
    let count_red = red_size != EGL_DONT_CARE && red_size > 0;
    let count_green = green_size != EGL_DONT_CARE && green_size > 0;
    let count_blue = blue_size != EGL_DONT_CARE && blue_size > 0;
    let count_alpha = alpha_size != EGL_DONT_CARE && alpha_size > 0;

    matching_configs.sort_by(|a, b| {
      // 1. EGL_CONFIG_CAVEAT is the same for all configs
      // 2. EGL_COLOR_BUFFER_TYPE is the same for all configs
      // 3.
      let mut a_bits = 0;
      let mut b_bits = 0;
      if count_red {
        a_bits += a.color_format.sizes().0;
        b_bits += b.color_format.sizes().0;
      }
      if count_green {
        a_bits += a.color_format.sizes().1;
        b_bits += b.color_format.sizes().1;
      }
      if count_blue {
        a_bits += a.color_format.sizes().2;
        b_bits += b.color_format.sizes().2;
      }
      if count_alpha {
        a_bits += a.color_format.sizes().3;
        b_bits += b.color_format.sizes().3;
      }

      a_bits.cmp(&b_bits).reverse()
        .then_with(|| { // 4.
          a.color_format.size().cmp(&b.color_format.size())
        }).then_with(|| { // 5.
          a.sample_buffers().cmp(&b.sample_buffers())
        }).then_with(|| { // 6.
          a.samples.cmp(&b.samples)
        }).then_with(|| { // 7.
          a.depth_size.cmp(&b.depth_size)
        }).then_with(|| { // 8.
          a.stencil_size.cmp(&b.stencil_size)
        })
      // 9. We don't use EGL_ALPHA_MASK_SIZE
      // 10. We don't use EGL_NATIVE_VISUAL_TYPE
        .then_with(|| { // 11.
          a.id.cmp(&b.id)
        })
    });

    let num = (config_size as usize).min(matching_configs.len());

    for i in 0..num {
      ptr::write(configs.offset(i as _), matching_configs[i as usize]);
    }
  }

  EGL_TRUE
}


#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglClientWaitSync(dpy: EGLDisplay, sync: EGLSync, flags: EGLint, timeout: EGLTime) -> EGLint {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCopyBuffers(dpy: EGLDisplay, surface: EGLSurface, target: EGLNativePixmapType) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreateContext(dpy: EGLDisplay, config: EGLConfig, share_context: EGLContext, attrib_list: AttribList) -> Box<Context> {
  Box::new(Context::new(config))
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreateImage(dpy: EGLDisplay, ctx: EGLContext, target: EGLenum, buffer: EGLClientBuffer, attrib_list: AttribList) -> EGLImage {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreatePbufferFromClientBuffer(dpy: EGLDisplay, buftype: EGLenum, buffer: EGLClientBuffer, config: EGLConfig, attrib_list: AttribList) -> EGLSurface {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreatePbufferSurface(_dpy: EGLDisplay, config: EGLConfig, attrib_list: AttribList) -> EGLSurface {
  let mut width = 0;
  let mut height = 0;
  let mut _texture_format = EGL_NO_TEXTURE;

  for (attrib, value) in attrib_list.iter() {
    match attrib as EGLenum {
      EGL_WIDTH => { width = value; },
      EGL_HEIGHT => { height = value; },
      EGL_TEXTURE_FORMAT => { _texture_format = value as EGLenum; }
      x => unimplemented!("{:x}", x),
    }
  }

  let color_buffer_size = width as usize * height as usize * config.color_format.byte_size();
  let mut color_buffer = Vec::with_capacity(color_buffer_size);
  color_buffer.resize(color_buffer_size, 0);

  let ds_buffer_size = width as usize * height as usize * config.ds_bytes();
  let mut ds_buffer = Vec::with_capacity(ds_buffer_size);
  ds_buffer.resize(ds_buffer_size, 0);

  let surface = Box::new(Surface{
    config,
    width,
    height,
    color_buffer,
    ds_buffer,
    multisample_resolve: EGL_MULTISAMPLE_RESOLVE_DEFAULT,
  });
  Box::into_raw(surface)
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreatePixmapSurface(dpy: EGLDisplay, config: EGLConfig, pixmap: EGLNativePixmapType, attrib_list: AttribList) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreatePlatformPixmapSurface(dpy: EGLDisplay, config: EGLConfig, native_pixmap: *mut c_void, attrib_list: AttribList) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreatePlatformWindowSurface(dpy: EGLDisplay, config: EGLConfig, native_window: *mut c_void, attrib_list: AttribList) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreateSync(dpy: EGLDisplay, type_: EGLenum, attrib_list: AttribList) -> EGLSync {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglCreateWindowSurface(dpy: EGLDisplay, config: EGLConfig, win: EGLNativeWindowType, attrib_list: AttribList) -> EGLSurface {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglDestroyContext(_dpy: EGLDisplay, _ctx: Box<Context>) -> EGLBoolean {
  EGL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglDestroyImage(dpy: EGLDisplay, image: EGLImage) -> EGLBoolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglDestroySurface(_dpy: EGLDisplay, _surface: Box<Surface>) -> EGLBoolean {
  EGL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglDestroySync(dpy: EGLDisplay, sync: EGLSync) -> EGLBoolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetConfigAttrib(
  _dpy: EGLDisplay,
  config: &Config,
  attribute: EGLint,
  value: &mut EGLint,
) -> EGLBoolean {
  let result = match attribute as EGLenum {
    EGL_CONFIG_ID => { config.id as EGLint },
    EGL_RENDERABLE_TYPE => {
      // TODO: GLES 1.0?
      (EGL_OPENGL_ES3_BIT | EGL_OPENGL_ES2_BIT) as EGLint
    },
    EGL_CONFORMANT => {
      // TODO: GLES 1.0?
      (EGL_OPENGL_ES3_BIT | EGL_OPENGL_ES2_BIT) as EGLint
    },
    EGL_RED_SIZE => { config.color_format.sizes().0 as EGLint },
    EGL_GREEN_SIZE => { config.color_format.sizes().1 as EGLint },
    EGL_BLUE_SIZE => { config.color_format.sizes().2 as EGLint },
    EGL_ALPHA_SIZE => { config.color_format.sizes().3 as EGLint },
    EGL_BUFFER_SIZE => { config.color_format.size() as EGLint },
    EGL_DEPTH_SIZE => { config.depth_size as EGLint },
    EGL_STENCIL_SIZE => { config.stencil_size as EGLint },
    EGL_SAMPLES => { config.samples as EGLint },
    EGL_LUMINANCE_SIZE => { 0 },
    EGL_ALPHA_MASK_SIZE => { 0 },
    EGL_SURFACE_TYPE => { EGL_PBUFFER_BIT as EGLint },
    EGL_BIND_TO_TEXTURE_RGB => EGL_FALSE as EGLint,
    EGL_BIND_TO_TEXTURE_RGBA => EGL_FALSE as EGLint,
    EGL_COLOR_BUFFER_TYPE => EGL_RGB_BUFFER as EGLint,
    EGL_CONFIG_CAVEAT => EGL_NONE as EGLint,
    EGL_LEVEL => 0,
    EGL_MAX_PBUFFER_WIDTH => 4096,
    EGL_MAX_PBUFFER_HEIGHT => 4096,
    EGL_MAX_PBUFFER_PIXELS => 4096 * 4096,
    EGL_MIN_SWAP_INTERVAL => 0,
    EGL_MAX_SWAP_INTERVAL => 0,
    EGL_NATIVE_RENDERABLE => EGL_FALSE as EGLint,
    EGL_SAMPLE_BUFFERS => config.sample_buffers() as EGLint,
    EGL_TRANSPARENT_TYPE => EGL_NONE as EGLint,
    EGL_TRANSPARENT_RED_VALUE => 0,
    EGL_TRANSPARENT_GREEN_VALUE => 0,
    EGL_TRANSPARENT_BLUE_VALUE => 0,
    EGL_NATIVE_VISUAL_ID => 0,
    EGL_NATIVE_VISUAL_TYPE => EGL_NONE as EGLint,
    x => unimplemented!("{:x}", x),
    // _ => { return EGL_FALSE; },
  };

  *value = result;

  EGL_TRUE
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetConfigs(_dpy: EGLDisplay, configs: *mut EGLConfig, config_size: EGLint, num_config: Option<&mut EGLint>) -> EGLBoolean {
  if num_config.is_none() {
    // TODO: set error
    return EGL_FALSE
  }
  let num_config = num_config.unwrap();

  if configs.is_null() {
    *num_config = CONFIGS.len() as EGLint;
  } else {
    let config_size = config_size.max(0) as usize;

    // TODO: make some sort sort of pointer FromIterator thing for this
    let configs = unsafe{ ::std::slice::from_raw_parts_mut(configs, config_size) };

    for (to, from) in configs.iter_mut().zip(CONFIGS.iter()) {
      *to = from;
    }

    let num = CONFIGS.len().min(configs.len());
    *num_config = num as EGLint;
  }

  EGL_TRUE
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetCurrentContext() -> EGLContext {
  CONTEXT.with(|c| c.get())
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetCurrentDisplay() -> EGLDisplay {
  &THE_DISPLAY
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetCurrentSurface(readdraw: EGLint) -> EGLSurface {
  let context = gl::current();
  match readdraw as EGLenum {
    EGL_READ => context.read_surface,
    EGL_DRAW => context.draw_surface,
    _ => EGL_NO_SURFACE, // TODO: set error
  }
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetDisplay(_display_id: EGLNativeDisplayType) -> EGLDisplay {
  &THE_DISPLAY as *const _
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetError() -> EGLint {
  EGL_SUCCESS as EGLint
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetPlatformDisplay(platform: EGLenum, native_display: *mut c_void, attrib_list: AttribList) -> EGLDisplay {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetProcAddress(procname: *const c_char) -> *const c_void {
  let cstr = unsafe{ ::std::ffi::CStr::from_ptr(procname) };

  match cstr.to_bytes() {
    b"glActiveShaderProgram" => gl::glActiveShaderProgram as _,
    b"glActiveTexture" => gl::glActiveTexture as _,
    b"glAttachShader" => gl::glAttachShader as _,
    b"glBeginQuery" => gl::glBeginQuery as _,
    b"glBeginTransformFeedback" => gl::glBeginTransformFeedback as _,
    b"glBindAttribLocation" => gl::glBindAttribLocation as _,
    b"glBindBuffer" => gl::glBindBuffer as _,
    b"glBindBufferBase" => gl::glBindBufferBase as _,
    b"glBindBufferRange" => gl::glBindBufferRange as _,
    b"glBindFramebuffer" => gl::glBindFramebuffer as _,
    b"glBindImageTexture" => gl::glBindImageTexture as _,
    b"glBindProgramPipeline" => gl::glBindProgramPipeline as _,
    b"glBindRenderbuffer" => gl::glBindRenderbuffer as _,
    b"glBindSampler" => gl::glBindSampler as _,
    b"glBindTexture" => gl::glBindTexture as _,
    b"glBindTransformFeedback" => gl::glBindTransformFeedback as _,
    b"glBindVertexArray" => gl::glBindVertexArray as _,
    b"glBindVertexBuffer" => gl::glBindVertexBuffer as _,
    b"glBlendBarrier" => gl::glBlendBarrier as _,
    b"glBlendColor" => gl::glBlendColor as _,
    b"glBlendEquation" => gl::glBlendEquation as _,
    b"glBlendEquationSeparate" => gl::glBlendEquationSeparate as _,
    b"glBlendEquationSeparatei" => gl::glBlendEquationSeparatei as _,
    b"glBlendEquationi" => gl::glBlendEquationi as _,
    b"glBlendFunc" => gl::glBlendFunc as _,
    b"glBlendFuncSeparate" => gl::glBlendFuncSeparate as _,
    b"glBlendFuncSeparatei" => gl::glBlendFuncSeparatei as _,
    b"glBlendFunci" => gl::glBlendFunci as _,
    b"glBlitFramebuffer" => gl::glBlitFramebuffer as _,
    b"glBufferData" => gl::glBufferData as _,
    b"glBufferSubData" => gl::glBufferSubData as _,
    b"glCheckFramebufferStatus" => gl::glCheckFramebufferStatus as _,
    b"glClear" => gl::glClear as _,
    b"glClearBufferfi" => gl::glClearBufferfi as _,
    b"glClearBufferfv" => gl::glClearBufferfv as _,
    b"glClearBufferiv" => gl::glClearBufferiv as _,
    b"glClearBufferuiv" => gl::glClearBufferuiv as _,
    b"glClearColor" => gl::glClearColor as _,
    b"glClearDepthf" => gl::glClearDepthf as _,
    b"glClearStencil" => gl::glClearStencil as _,
    b"glClientWaitSync" => gl::glClientWaitSync as _,
    b"glColorMask" => gl::glColorMask as _,
    b"glColorMaski" => gl::glColorMaski as _,
    b"glCompileShader" => gl::glCompileShader as _,
    b"glCompressedTexImage2D" => gl::glCompressedTexImage2D as _,
    b"glCompressedTexImage3D" => gl::glCompressedTexImage3D as _,
    b"glCompressedTexSubImage2D" => gl::glCompressedTexSubImage2D as _,
    b"glCompressedTexSubImage3D" => gl::glCompressedTexSubImage3D as _,
    b"glCopyBufferSubData" => gl::glCopyBufferSubData as _,
    b"glCopyImageSubData" => gl::glCopyImageSubData as _,
    b"glCopyTexImage2D" => gl::glCopyTexImage2D as _,
    b"glCopyTexSubImage2D" => gl::glCopyTexSubImage2D as _,
    b"glCopyTexSubImage3D" => gl::glCopyTexSubImage3D as _,
    b"glCreateProgram" => gl::glCreateProgram as _,
    b"glCreateShader" => gl::glCreateShader as _,
    b"glCreateShaderProgramv" => gl::glCreateShaderProgramv as _,
    b"glCullFace" => gl::glCullFace as _,
    b"glDebugMessageCallback" => gl::glDebugMessageCallback as _,
    b"glDebugMessageControl" => gl::glDebugMessageControl as _,
    b"glDebugMessageInsert" => gl::glDebugMessageInsert as _,
    b"glDeleteBuffers" => gl::glDeleteBuffers as _,
    b"glDeleteFramebuffers" => gl::glDeleteFramebuffers as _,
    b"glDeleteProgram" => gl::glDeleteProgram as _,
    b"glDeleteProgramPipelines" => gl::glDeleteProgramPipelines as _,
    b"glDeleteQueries" => gl::glDeleteQueries as _,
    b"glDeleteRenderbuffers" => gl::glDeleteRenderbuffers as _,
    b"glDeleteSamplers" => gl::glDeleteSamplers as _,
    b"glDeleteShader" => gl::glDeleteShader as _,
    b"glDeleteSync" => gl::glDeleteSync as _,
    b"glDeleteTextures" => gl::glDeleteTextures as _,
    b"glDeleteTransformFeedbacks" => gl::glDeleteTransformFeedbacks as _,
    b"glDeleteVertexArrays" => gl::glDeleteVertexArrays as _,
    b"glDepthFunc" => gl::glDepthFunc as _,
    b"glDepthMask" => gl::glDepthMask as _,
    b"glDepthRangef" => gl::glDepthRangef as _,
    b"glDetachShader" => gl::glDetachShader as _,
    b"glDisable" => gl::glDisable as _,
    b"glDisableVertexAttribArray" => gl::glDisableVertexAttribArray as _,
    b"glDisablei" => gl::glDisablei as _,
    b"glDispatchCompute" => gl::glDispatchCompute as _,
    b"glDispatchComputeIndirect" => gl::glDispatchComputeIndirect as _,
    b"glDrawArrays" => gl::glDrawArrays as _,
    b"glDrawArraysIndirect" => gl::glDrawArraysIndirect as _,
    b"glDrawArraysInstanced" => gl::glDrawArraysInstanced as _,
    b"glDrawBuffers" => gl::glDrawBuffers as _,
    b"glDrawElements" => gl::glDrawElements as _,
    b"glDrawElementsBaseVertex" => gl::glDrawElementsBaseVertex as _,
    b"glDrawElementsIndirect" => gl::glDrawElementsIndirect as _,
    b"glDrawElementsInstanced" => gl::glDrawElementsInstanced as _,
    b"glDrawElementsInstancedBaseVertex" => gl::glDrawElementsInstancedBaseVertex as _,
    b"glDrawRangeElements" => gl::glDrawRangeElements as _,
    b"glDrawRangeElementsBaseVertex" => gl::glDrawRangeElementsBaseVertex as _,
    b"glEnable" => gl::glEnable as _,
    b"glEnableVertexAttribArray" => gl::glEnableVertexAttribArray as _,
    b"glEnablei" => gl::glEnablei as _,
    b"glEndQuery" => gl::glEndQuery as _,
    b"glEndTransformFeedback" => gl::glEndTransformFeedback as _,
    b"glFenceSync" => gl::glFenceSync as _,
    b"glFinish" => gl::glFinish as _,
    b"glFlush" => gl::glFlush as _,
    b"glFlushMappedBufferRange" => gl::glFlushMappedBufferRange as _,
    b"glFramebufferParameteri" => gl::glFramebufferParameteri as _,
    b"glFramebufferRenderbuffer" => gl::glFramebufferRenderbuffer as _,
    b"glFramebufferTexture" => gl::glFramebufferTexture as _,
    b"glFramebufferTexture2D" => gl::glFramebufferTexture2D as _,
    b"glFramebufferTextureLayer" => gl::glFramebufferTextureLayer as _,
    b"glFrontFace" => gl::glFrontFace as _,
    b"glGenBuffers" => gl::glGenBuffers as _,
    b"glGenFramebuffers" => gl::glGenFramebuffers as _,
    b"glGenProgramPipelines" => gl::glGenProgramPipelines as _,
    b"glGenQueries" => gl::glGenQueries as _,
    b"glGenRenderbuffers" => gl::glGenRenderbuffers as _,
    b"glGenSamplers" => gl::glGenSamplers as _,
    b"glGenTextures" => gl::glGenTextures as _,
    b"glGenTransformFeedbacks" => gl::glGenTransformFeedbacks as _,
    b"glGenVertexArrays" => gl::glGenVertexArrays as _,
    b"glGenerateMipmap" => gl::glGenerateMipmap as _,
    b"glGetActiveAttrib" => gl::glGetActiveAttrib as _,
    b"glGetActiveUniform" => gl::glGetActiveUniform as _,
    b"glGetActiveUniformBlockName" => gl::glGetActiveUniformBlockName as _,
    b"glGetActiveUniformBlockiv" => gl::glGetActiveUniformBlockiv as _,
    b"glGetActiveUniformsiv" => gl::glGetActiveUniformsiv as _,
    b"glGetAttachedShaders" => gl::glGetAttachedShaders as _,
    b"glGetAttribLocation" => gl::glGetAttribLocation as _,
    b"glGetBooleani_v" => gl::glGetBooleani_v as _,
    b"glGetBooleanv" => gl::glGetBooleanv as _,
    b"glGetBufferParameteri64v" => gl::glGetBufferParameteri64v as _,
    b"glGetBufferParameteriv" => gl::glGetBufferParameteriv as _,
    b"glGetBufferPointerv" => gl::glGetBufferPointerv as _,
    b"glGetDebugMessageLog" => gl::glGetDebugMessageLog as _,
    b"glGetError" => gl::glGetError as _,
    b"glGetFloatv" => gl::glGetFloatv as _,
    b"glGetFragDataLocation" => gl::glGetFragDataLocation as _,
    b"glGetFramebufferAttachmentParameteriv" => gl::glGetFramebufferAttachmentParameteriv as _,
    b"glGetFramebufferParameteriv" => gl::glGetFramebufferParameteriv as _,
    b"glGetGraphicsResetStatus" => gl::glGetGraphicsResetStatus as _,
    b"glGetInteger64i_v" => gl::glGetInteger64i_v as _,
    b"glGetInteger64v" => gl::glGetInteger64v as _,
    b"glGetIntegeri_v" => gl::glGetIntegeri_v as _,
    b"glGetIntegerv" => gl::glGetIntegerv as _,
    b"glGetInternalformativ" => gl::glGetInternalformativ as _,
    b"glGetMultisamplefv" => gl::glGetMultisamplefv as _,
    b"glGetObjectLabel" => gl::glGetObjectLabel as _,
    b"glGetObjectPtrLabel" => gl::glGetObjectPtrLabel as _,
    b"glGetPointerv" => gl::glGetPointerv as _,
    b"glGetProgramBinary" => gl::glGetProgramBinary as _,
    b"glGetProgramInfoLog" => gl::glGetProgramInfoLog as _,
    b"glGetProgramInterfaceiv" => gl::glGetProgramInterfaceiv as _,
    b"glGetProgramPipelineInfoLog" => gl::glGetProgramPipelineInfoLog as _,
    b"glGetProgramPipelineiv" => gl::glGetProgramPipelineiv as _,
    b"glGetProgramResourceIndex" => gl::glGetProgramResourceIndex as _,
    b"glGetProgramResourceLocation" => gl::glGetProgramResourceLocation as _,
    b"glGetProgramResourceName" => gl::glGetProgramResourceName as _,
    b"glGetProgramResourceiv" => gl::glGetProgramResourceiv as _,
    b"glGetProgramiv" => gl::glGetProgramiv as _,
    b"glGetQueryObjectuiv" => gl::glGetQueryObjectuiv as _,
    b"glGetQueryiv" => gl::glGetQueryiv as _,
    b"glGetRenderbufferParameteriv" => gl::glGetRenderbufferParameteriv as _,
    b"glGetSamplerParameterIiv" => gl::glGetSamplerParameterIiv as _,
    b"glGetSamplerParameterIuiv" => gl::glGetSamplerParameterIuiv as _,
    b"glGetSamplerParameterfv" => gl::glGetSamplerParameterfv as _,
    b"glGetSamplerParameteriv" => gl::glGetSamplerParameteriv as _,
    b"glGetShaderInfoLog" => gl::glGetShaderInfoLog as _,
    b"glGetShaderPrecisionFormat" => gl::glGetShaderPrecisionFormat as _,
    b"glGetShaderSource" => gl::glGetShaderSource as _,
    b"glGetShaderiv" => gl::glGetShaderiv as _,
    b"glGetString" => gl::glGetString as _,
    b"glGetStringi" => gl::glGetStringi as _,
    b"glGetSynciv" => gl::glGetSynciv as _,
    b"glGetTexLevelParameterfv" => gl::glGetTexLevelParameterfv as _,
    b"glGetTexLevelParameteriv" => gl::glGetTexLevelParameteriv as _,
    b"glGetTexParameterIiv" => gl::glGetTexParameterIiv as _,
    b"glGetTexParameterIuiv" => gl::glGetTexParameterIuiv as _,
    b"glGetTexParameterfv" => gl::glGetTexParameterfv as _,
    b"glGetTexParameteriv" => gl::glGetTexParameteriv as _,
    b"glGetTransformFeedbackVarying" => gl::glGetTransformFeedbackVarying as _,
    b"glGetUniformBlockIndex" => gl::glGetUniformBlockIndex as _,
    b"glGetUniformIndices" => gl::glGetUniformIndices as _,
    b"glGetUniformLocation" => gl::glGetUniformLocation as _,
    b"glGetUniformfv" => gl::glGetUniformfv as _,
    b"glGetUniformiv" => gl::glGetUniformiv as _,
    b"glGetUniformuiv" => gl::glGetUniformuiv as _,
    b"glGetVertexAttribIiv" => gl::glGetVertexAttribIiv as _,
    b"glGetVertexAttribIuiv" => gl::glGetVertexAttribIuiv as _,
    b"glGetVertexAttribPointerv" => gl::glGetVertexAttribPointerv as _,
    b"glGetVertexAttribfv" => gl::glGetVertexAttribfv as _,
    b"glGetVertexAttribiv" => gl::glGetVertexAttribiv as _,
    b"glGetnUniformfv" => gl::glGetnUniformfv as _,
    b"glGetnUniformiv" => gl::glGetnUniformiv as _,
    b"glGetnUniformuiv" => gl::glGetnUniformuiv as _,
    b"glHint" => gl::glHint as _,
    b"glInvalidateFramebuffer" => gl::glInvalidateFramebuffer as _,
    b"glInvalidateSubFramebuffer" => gl::glInvalidateSubFramebuffer as _,
    b"glIsBuffer" => gl::glIsBuffer as _,
    b"glIsEnabled" => gl::glIsEnabled as _,
    b"glIsEnabledi" => gl::glIsEnabledi as _,
    b"glIsFramebuffer" => gl::glIsFramebuffer as _,
    b"glIsProgram" => gl::glIsProgram as _,
    b"glIsProgramPipeline" => gl::glIsProgramPipeline as _,
    b"glIsQuery" => gl::glIsQuery as _,
    b"glIsRenderbuffer" => gl::glIsRenderbuffer as _,
    b"glIsSampler" => gl::glIsSampler as _,
    b"glIsShader" => gl::glIsShader as _,
    b"glIsSync" => gl::glIsSync as _,
    b"glIsTexture" => gl::glIsTexture as _,
    b"glIsTransformFeedback" => gl::glIsTransformFeedback as _,
    b"glIsVertexArray" => gl::glIsVertexArray as _,
    b"glLineWidth" => gl::glLineWidth as _,
    b"glLinkProgram" => gl::glLinkProgram as _,
    b"glMapBufferRange" => gl::glMapBufferRange as _,
    b"glMemoryBarrier" => gl::glMemoryBarrier as _,
    b"glMemoryBarrierByRegion" => gl::glMemoryBarrierByRegion as _,
    b"glMinSampleShading" => gl::glMinSampleShading as _,
    b"glObjectLabel" => gl::glObjectLabel as _,
    b"glObjectPtrLabel" => gl::glObjectPtrLabel as _,
    b"glPatchParameteri" => gl::glPatchParameteri as _,
    b"glPauseTransformFeedback" => gl::glPauseTransformFeedback as _,
    b"glPixelStorei" => gl::glPixelStorei as _,
    b"glPolygonOffset" => gl::glPolygonOffset as _,
    b"glPopDebugGroup" => gl::glPopDebugGroup as _,
    b"glPrimitiveBoundingBox" => gl::glPrimitiveBoundingBox as _,
    b"glProgramBinary" => gl::glProgramBinary as _,
    b"glProgramParameteri" => gl::glProgramParameteri as _,
    b"glProgramUniform1f" => gl::glProgramUniform1f as _,
    b"glProgramUniform1fv" => gl::glProgramUniform1fv as _,
    b"glProgramUniform1i" => gl::glProgramUniform1i as _,
    b"glProgramUniform1iv" => gl::glProgramUniform1iv as _,
    b"glProgramUniform1ui" => gl::glProgramUniform1ui as _,
    b"glProgramUniform1uiv" => gl::glProgramUniform1uiv as _,
    b"glProgramUniform2f" => gl::glProgramUniform2f as _,
    b"glProgramUniform2fv" => gl::glProgramUniform2fv as _,
    b"glProgramUniform2i" => gl::glProgramUniform2i as _,
    b"glProgramUniform2iv" => gl::glProgramUniform2iv as _,
    b"glProgramUniform2ui" => gl::glProgramUniform2ui as _,
    b"glProgramUniform2uiv" => gl::glProgramUniform2uiv as _,
    b"glProgramUniform3f" => gl::glProgramUniform3f as _,
    b"glProgramUniform3fv" => gl::glProgramUniform3fv as _,
    b"glProgramUniform3i" => gl::glProgramUniform3i as _,
    b"glProgramUniform3iv" => gl::glProgramUniform3iv as _,
    b"glProgramUniform3ui" => gl::glProgramUniform3ui as _,
    b"glProgramUniform3uiv" => gl::glProgramUniform3uiv as _,
    b"glProgramUniform4f" => gl::glProgramUniform4f as _,
    b"glProgramUniform4fv" => gl::glProgramUniform4fv as _,
    b"glProgramUniform4i" => gl::glProgramUniform4i as _,
    b"glProgramUniform4iv" => gl::glProgramUniform4iv as _,
    b"glProgramUniform4ui" => gl::glProgramUniform4ui as _,
    b"glProgramUniform4uiv" => gl::glProgramUniform4uiv as _,
    b"glProgramUniformMatrix2fv" => gl::glProgramUniformMatrix2fv as _,
    b"glProgramUniformMatrix2x3fv" => gl::glProgramUniformMatrix2x3fv as _,
    b"glProgramUniformMatrix2x4fv" => gl::glProgramUniformMatrix2x4fv as _,
    b"glProgramUniformMatrix3fv" => gl::glProgramUniformMatrix3fv as _,
    b"glProgramUniformMatrix3x2fv" => gl::glProgramUniformMatrix3x2fv as _,
    b"glProgramUniformMatrix3x4fv" => gl::glProgramUniformMatrix3x4fv as _,
    b"glProgramUniformMatrix4fv" => gl::glProgramUniformMatrix4fv as _,
    b"glProgramUniformMatrix4x2fv" => gl::glProgramUniformMatrix4x2fv as _,
    b"glProgramUniformMatrix4x3fv" => gl::glProgramUniformMatrix4x3fv as _,
    b"glPushDebugGroup" => gl::glPushDebugGroup as _,
    b"glReadBuffer" => gl::glReadBuffer as _,
    b"glReadPixels" => gl::glReadPixels as _,
    b"glReadnPixels" => gl::glReadnPixels as _,
    b"glReleaseShaderCompiler" => gl::glReleaseShaderCompiler as _,
    b"glRenderbufferStorage" => gl::glRenderbufferStorage as _,
    b"glRenderbufferStorageMultisample" => gl::glRenderbufferStorageMultisample as _,
    b"glResumeTransformFeedback" => gl::glResumeTransformFeedback as _,
    b"glSampleCoverage" => gl::glSampleCoverage as _,
    b"glSampleMaski" => gl::glSampleMaski as _,
    b"glSamplerParameterIiv" => gl::glSamplerParameterIiv as _,
    b"glSamplerParameterIuiv" => gl::glSamplerParameterIuiv as _,
    b"glSamplerParameterf" => gl::glSamplerParameterf as _,
    b"glSamplerParameterfv" => gl::glSamplerParameterfv as _,
    b"glSamplerParameteri" => gl::glSamplerParameteri as _,
    b"glSamplerParameteriv" => gl::glSamplerParameteriv as _,
    b"glScissor" => gl::glScissor as _,
    b"glShaderBinary" => gl::glShaderBinary as _,
    b"glShaderSource" => gl::glShaderSource as _,
    b"glStencilFunc" => gl::glStencilFunc as _,
    b"glStencilFuncSeparate" => gl::glStencilFuncSeparate as _,
    b"glStencilMask" => gl::glStencilMask as _,
    b"glStencilMaskSeparate" => gl::glStencilMaskSeparate as _,
    b"glStencilOp" => gl::glStencilOp as _,
    b"glStencilOpSeparate" => gl::glStencilOpSeparate as _,
    b"glTexBuffer" => gl::glTexBuffer as _,
    b"glTexBufferRange" => gl::glTexBufferRange as _,
    b"glTexImage2D" => gl::glTexImage2D as _,
    b"glTexImage3D" => gl::glTexImage3D as _,
    b"glTexParameterIiv" => gl::glTexParameterIiv as _,
    b"glTexParameterIuiv" => gl::glTexParameterIuiv as _,
    b"glTexParameterf" => gl::glTexParameterf as _,
    b"glTexParameterfv" => gl::glTexParameterfv as _,
    b"glTexParameteri" => gl::glTexParameteri as _,
    b"glTexParameteriv" => gl::glTexParameteriv as _,
    b"glTexStorage2D" => gl::glTexStorage2D as _,
    b"glTexStorage2DMultisample" => gl::glTexStorage2DMultisample as _,
    b"glTexStorage3D" => gl::glTexStorage3D as _,
    b"glTexStorage3DMultisample" => gl::glTexStorage3DMultisample as _,
    b"glTexSubImage2D" => gl::glTexSubImage2D as _,
    b"glTexSubImage3D" => gl::glTexSubImage3D as _,
    b"glTransformFeedbackVaryings" => gl::glTransformFeedbackVaryings as _,
    b"glUniform1f" => gl::glUniform1f as _,
    b"glUniform1fv" => gl::glUniform1fv as _,
    b"glUniform1i" => gl::glUniform1i as _,
    b"glUniform1iv" => gl::glUniform1iv as _,
    b"glUniform1ui" => gl::glUniform1ui as _,
    b"glUniform1uiv" => gl::glUniform1uiv as _,
    b"glUniform2f" => gl::glUniform2f as _,
    b"glUniform2fv" => gl::glUniform2fv as _,
    b"glUniform2i" => gl::glUniform2i as _,
    b"glUniform2iv" => gl::glUniform2iv as _,
    b"glUniform2ui" => gl::glUniform2ui as _,
    b"glUniform2uiv" => gl::glUniform2uiv as _,
    b"glUniform3f" => gl::glUniform3f as _,
    b"glUniform3fv" => gl::glUniform3fv as _,
    b"glUniform3i" => gl::glUniform3i as _,
    b"glUniform3iv" => gl::glUniform3iv as _,
    b"glUniform3ui" => gl::glUniform3ui as _,
    b"glUniform3uiv" => gl::glUniform3uiv as _,
    b"glUniform4f" => gl::glUniform4f as _,
    b"glUniform4fv" => gl::glUniform4fv as _,
    b"glUniform4i" => gl::glUniform4i as _,
    b"glUniform4iv" => gl::glUniform4iv as _,
    b"glUniform4ui" => gl::glUniform4ui as _,
    b"glUniform4uiv" => gl::glUniform4uiv as _,
    b"glUniformBlockBinding" => gl::glUniformBlockBinding as _,
    b"glUniformMatrix2fv" => gl::glUniformMatrix2fv as _,
    b"glUniformMatrix2x3fv" => gl::glUniformMatrix2x3fv as _,
    b"glUniformMatrix2x4fv" => gl::glUniformMatrix2x4fv as _,
    b"glUniformMatrix3fv" => gl::glUniformMatrix3fv as _,
    b"glUniformMatrix3x2fv" => gl::glUniformMatrix3x2fv as _,
    b"glUniformMatrix3x4fv" => gl::glUniformMatrix3x4fv as _,
    b"glUniformMatrix4fv" => gl::glUniformMatrix4fv as _,
    b"glUniformMatrix4x2fv" => gl::glUniformMatrix4x2fv as _,
    b"glUniformMatrix4x3fv" => gl::glUniformMatrix4x3fv as _,
    b"glUnmapBuffer" => gl::glUnmapBuffer as _,
    b"glUseProgram" => gl::glUseProgram as _,
    b"glUseProgramStages" => gl::glUseProgramStages as _,
    b"glValidateProgram" => gl::glValidateProgram as _,
    b"glValidateProgramPipeline" => gl::glValidateProgramPipeline as _,
    b"glVertexAttrib1f" => gl::glVertexAttrib1f as _,
    b"glVertexAttrib1fv" => gl::glVertexAttrib1fv as _,
    b"glVertexAttrib2f" => gl::glVertexAttrib2f as _,
    b"glVertexAttrib2fv" => gl::glVertexAttrib2fv as _,
    b"glVertexAttrib3f" => gl::glVertexAttrib3f as _,
    b"glVertexAttrib3fv" => gl::glVertexAttrib3fv as _,
    b"glVertexAttrib4f" => gl::glVertexAttrib4f as _,
    b"glVertexAttrib4fv" => gl::glVertexAttrib4fv as _,
    b"glVertexAttribBinding" => gl::glVertexAttribBinding as _,
    b"glVertexAttribDivisor" => gl::glVertexAttribDivisor as _,
    b"glVertexAttribFormat" => gl::glVertexAttribFormat as _,
    b"glVertexAttribI4i" => gl::glVertexAttribI4i as _,
    b"glVertexAttribI4iv" => gl::glVertexAttribI4iv as _,
    b"glVertexAttribI4ui" => gl::glVertexAttribI4ui as _,
    b"glVertexAttribI4uiv" => gl::glVertexAttribI4uiv as _,
    b"glVertexAttribIFormat" => gl::glVertexAttribIFormat as _,
    b"glVertexAttribIPointer" => gl::glVertexAttribIPointer as _,
    b"glVertexAttribPointer" => gl::glVertexAttribPointer as _,
    b"glVertexBindingDivisor" => gl::glVertexBindingDivisor as _,
    b"glViewport" => gl::glViewport as _,
    b"glWaitSync" => gl::glWaitSync as _,
    b"eglCreateImageKHR" => eglCreateImage as _,
    b"eglDestroyImageKHR" => eglDestroyImage as _,
    b"eglClientWaitSyncKHR"
      | b"eglCreatePlatformPixmapSurfaceEXT"
      | b"eglCreatePlatformWindowSurfaceEXT"
      | b"eglCreateSyncKHR"
      | b"eglDestroySyncKHR"
      | b"eglGetPlatformDisplayEXT"
      | b"eglGetSyncAttribKHR"
      | b"eglLockSurfaceKHR"
      | b"eglSetDamageRegionKHR"
      | b"eglSignalSyncKHR"
      | b"eglSwapBuffersWithDamageKHR"
      | b"eglUnlockSurfaceKHR"
      | b"eglWaitSyncKHR"
      | b"glEGLImageTargetRenderbufferStorageOES"
      | b"glEGLImageTargetTexture2DOES"
      => ptr::null(),
    _ => unimplemented!("{:?}", cstr)
  }
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglGetSyncAttrib(dpy: EGLDisplay, sync: EGLSync, attribute: EGLint, value: *mut EGLAttrib) -> EGLBoolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglInitialize(_dpy: EGLDisplay, major: &mut EGLint, minor: &mut EGLint) -> EGLBoolean {
  *major = 1;
  *minor = 5;
  EGL_TRUE
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglMakeCurrent(_dpy: EGLDisplay, draw: EGLSurface, read: EGLSurface, ctx: EGLContext) -> EGLBoolean {
  CONTEXT.with(|c| c.set(ctx));
  let context = unsafe{ ctx.as_mut() };
  if let Some(context) = context {
    context.set_surfaces(draw, read);
  }

  EGL_TRUE
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglQueryAPI() -> EGLenum {
  API.with(|a| a.get())
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglQueryContext(_dpy: EGLDisplay, ctx: &Context, attribute: EGLint, value: &mut EGLint) -> EGLBoolean {
  let result = match attribute as EGLenum {
    EGL_CONFIG_ID => ctx.config.id,
    EGL_CONTEXT_CLIENT_TYPE => EGL_OPENGL_ES_API as EGLint,
    EGL_CONTEXT_CLIENT_VERSION => 3,
    EGL_RENDER_BUFFER => EGL_BACK_BUFFER as EGLint,
    x => unimplemented!("{:x}", x),
    // _ => { return EGL_FALSE; } // TODO: set error
  };

  *value = result;

  EGL_TRUE
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglQueryString(_dpy: EGLDisplay, name: EGLint) -> *const c_char {
  let result: &'static [u8] = match name as EGLenum {
    EGL_CLIENT_APIS => b"OpenGL_ES\0",
    EGL_EXTENSIONS => b"EGL_KHR_create_context EGL_KHR_get_all_proc_addresses\0",
    EGL_VENDOR => b"MyGL\0",
    EGL_VERSION => b"1.5\0",
    x => unimplemented!("{:x}", x),
    // _ => {
    //   // TODO: set EGL_BAD_PARAMETER error
    //   ptr::null()
    // },
  };

  result.as_ptr() as _
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglQuerySurface(_dpy: EGLDisplay, surface: &Surface, attribute: EGLint, value: &mut EGLint) -> EGLBoolean {

  let result = match attribute as EGLenum {
    EGL_CONFIG_ID => surface.config.id,
    EGL_WIDTH => surface.width,
    EGL_HEIGHT => surface.height,
    EGL_HORIZONTAL_RESOLUTION => -1,
    EGL_VERTICAL_RESOLUTION => -1,
    EGL_PIXEL_ASPECT_RATIO => -1,
    EGL_MULTISAMPLE_RESOLVE => surface.multisample_resolve as EGLint,
    EGL_RENDER_BUFFER => EGL_BACK_BUFFER as EGLint,
    EGL_SWAP_BEHAVIOR => EGL_BUFFER_DESTROYED as EGLint,
    EGL_VG_ALPHA_FORMAT => EGL_VG_ALPHA_FORMAT_NONPRE as EGLint,
    EGL_VG_COLORSPACE => EGL_VG_COLORSPACE_sRGB as EGLint,
    EGL_LARGEST_PBUFFER => EGL_FALSE as EGLint,
    EGL_TEXTURE_FORMAT => EGL_NO_TEXTURE as EGLint,
    EGL_TEXTURE_TARGET => EGL_NO_TEXTURE as EGLint,
    EGL_MIPMAP_TEXTURE => EGL_FALSE as EGLint,
    EGL_MIPMAP_LEVEL => 0,
    x => unimplemented!("{:x}", x),
  };

  *value = result;
  EGL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglReleaseTexImage(dpy: EGLDisplay, surface: EGLSurface, buffer: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglReleaseThread() -> EGLBoolean {
  eglMakeCurrent(&THE_DISPLAY, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

  EGL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglSurfaceAttrib(dpy: EGLDisplay, surface: EGLSurface, attribute: EGLint, value: EGLint) -> EGLBoolean {
  match attribute as EGLenum {
    EGL_MIPMAP_LEVEL => {}, // TODO
    x => unimplemented!("{:x}", x),
  }

  EGL_TRUE
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglSwapBuffers(dpy: EGLDisplay, surface: EGLSurface) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglSwapInterval(dpy: EGLDisplay, interval: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglTerminate(_dpy: EGLDisplay) -> EGLBoolean {
  EGL_TRUE // TODO
}

#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglWaitClient() -> EGLBoolean {
  EGL_TRUE // TODO
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglWaitGL() -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglWaitNative(engine: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables)]
#[no_mangle]
#[cfg_attr(feature = "trace_egl", trace)]
pub extern "C" fn eglWaitSync(dpy: EGLDisplay, sync: EGLSync, flags: EGLint) -> EGLBoolean {
  unimplemented!()
}
