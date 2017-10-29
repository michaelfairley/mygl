#[macro_use]
extern crate lazy_static;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;


pub mod egl;
pub mod gl;
pub mod glsl;

// use std::ffi;
// use std::os::raw::*;

// #[allow(non_snake_case)]
// #[no_mangle]
// pub extern "C" fn eglGetProcAddress(name: *const c_char) -> *const c_void {
//   let cstr = unsafe { ffi::CStr::from_ptr(name) };
//   unimplemented!("{:?}", cstr);
// }
