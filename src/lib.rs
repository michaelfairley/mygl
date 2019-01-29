#![feature(integer_atomics)]

#[macro_use]
extern crate lazy_static;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[cfg(feature = "trace")]
extern crate trace;


pub mod egl;
pub mod gl;
pub mod glsl;
pub mod draw;
