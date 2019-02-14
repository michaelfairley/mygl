use types::*;

#[inline(always)]
pub(crate) fn fixed_to_float(fixed: GLfixed) -> GLfloat {
  fixed as f32 / 65536.0
}

extern "C" {
  // #[link_name = "llvm.convert.to.fp16.f32"]
  // fn convert_to_fp16_f32(f: f32) -> u16;

  // #[link_name = "llvm.convert.to.fp16.f64"]
  // fn convert_to_fp16_f64(f: f64) -> u16;

  #[link_name = "llvm.convert.from.fp16.f32"]
  fn convert_from_fp16_f32(i: u16) -> f32;

  // #[link_name = "llvm.convert.from.fp16.f64"]
  // fn convert_from_fp16_f64(i: u16) -> f64;
}

// #[inline(always)]
// pub(crate) fn f32_to_f16(f: f32) -> u16 {
//   unsafe { convert_to_fp16_f32(f) }
// }

// #[inline(always)]
// pub(crate) fn f64_to_f16(f: f64) -> u16 {
//   unsafe { convert_to_fp16_f64(f) }
// }

#[inline(always)]
pub(crate) fn f16_to_f32(i: u16) -> f32 {
  unsafe { convert_from_fp16_f32(i) }
}

// #[inline(always)]
// pub(crate) fn f16_to_f64(i: u16) -> f64 {
//   unsafe { convert_from_fp16_f64(i) }
// }

#[inline(always)]
pub(crate) fn normalize_i8(b: i8) -> f32 {
  (b as f32 / 0x7F as f32).max(-1.0)
}

#[inline(always)]
pub(crate) fn normalize_u8(b: u8) -> f32 {
  b as f32 / 0xFF as f32
}

#[inline(always)]
pub(crate) fn normalize_i16(b: i16) -> f32 {
  (b as f32 / 0x7F_FF as f32).max(-1.0)
}

#[inline(always)]
pub(crate) fn normalize_u16(b: u16) -> f32 {
  b as f32 / 0xFF_FF as f32
}

#[inline(always)]
pub(crate) fn normalize_i32(b: i32) -> f32 {
  (b as f32 / 0x7F_FF_FF_FF as f32).max(-1.0)
}

#[inline(always)]
pub(crate) fn normalize_u32(b: u32) -> f32 {
  b as f32 / 0xFF_FF_FF_FFu32 as f32
}
