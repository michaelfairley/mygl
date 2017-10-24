pub mod egl;

// use std::ffi;
// use std::os::raw::*;

// #[allow(non_snake_case)]
// #[no_mangle]
// pub extern "C" fn eglGetProcAddress(name: *const c_char) -> *const c_void {
//   let cstr = unsafe { ffi::CStr::from_ptr(name) };
//   unimplemented!("{:?}", cstr);
// }
