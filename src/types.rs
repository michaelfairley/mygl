use std::os::raw::*;

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
