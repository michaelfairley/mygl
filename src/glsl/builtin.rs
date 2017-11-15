use super::parse::{TypeSpecifierNonArray,TypeQualifier,StorageQualifier,Statement,FunctionPrototype};
use super::interpret::{Vars,Value};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Func(pub Box<fn(&mut Vars) -> Value>);

impl ::std::fmt::Debug for Func {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "builtin::Func")
  }
}

impl ::std::cmp::PartialEq for Func {
  fn eq(&self, _other: &Self) -> bool {
    false
  }
}

// TODO: this could maybe just be folded into uint_buffer
macro_rules! builtin_flag {
  (inout) => {TypeQualifier::Storage(StorageQualifier::Inout)};
}
macro_rules! builtin_flags {
  ($($flags:ident)*) => { vec![$(builtin_flag!($flags))*] };
}
macro_rules! builtin_type_specifier {
  (float) => {TypeSpecifierNonArray::Float};
  (vec2) => {TypeSpecifierNonArray::Vec2};
  (vec3) => {TypeSpecifierNonArray::Vec3};
  (vec4) => {TypeSpecifierNonArray::Vec4};
  (int) => {TypeSpecifierNonArray::Int};
  (ivec2) => {TypeSpecifierNonArray::IVec2};
  (ivec3) => {TypeSpecifierNonArray::IVec3};
  (ivec4) => {TypeSpecifierNonArray::IVec4};
  (uint) => {TypeSpecifierNonArray::Uint};
  (uint_buffer) => {TypeSpecifierNonArray::Uint};
  (uvec2) => {TypeSpecifierNonArray::UVec2};
  (uvec3) => {TypeSpecifierNonArray::UVec3};
  (uvec4) => {TypeSpecifierNonArray::UVec4};
  (uimage2d) => {TypeSpecifierNonArray::UImage2D};
  (void) => {TypeSpecifierNonArray::Void};
  (atomic_uint) => {TypeSpecifierNonArray::AtomicUint};
}
macro_rules! builtin_type {
  ($(!$flag:ident)* $type:ident) => {(builtin_flags!($($flag)*), (builtin_type_specifier!($type), vec![]))};
}
macro_rules! builtin_value_ref {
  (float, $aname:ident) => {Value::Float(ref $aname)};
  (vec2, $aname:ident) => {Value::Vec2(ref $aname)};
  (vec3, $aname:ident) => {Value::Vec3(ref $aname)};
  (vec4, $aname:ident) => {Value::Vec4(ref $aname)};
  (int, $aname:ident) => {Value::Int(ref $aname)};
  (ivec2, $aname:ident) => {Value::IVec2(ref $aname)};
  (ivec3, $aname:ident) => {Value::IVec3(ref $aname)};
  (ivec4, $aname:ident) => {Value::IVec4(ref $aname)};
  (uint, $aname:ident) => {Value::Uint(ref $aname)};
  (uint_buffer, $aname:ident) => {Value::Buffer(TypeSpecifierNonArray::Uint, ref $aname, _)};
  (uvec2, $aname:ident) => {Value::UVec2(ref $aname)};
  (uvec3, $aname:ident) => {Value::UVec3(ref $aname)};
  (uvec4, $aname:ident) => {Value::UVec4(ref $aname)};
  (uimage2d, $aname:ident) => {Value::UImage2D(ref $aname)};
  (atomic_uint, $aname:ident) => {Value::Buffer(_, ref $aname, _)};
}
macro_rules! builtin_value {
  (float) => {Value::Float};
  (vec2) => {Value::Vec2};
  (vec3) => {Value::Vec3};
  (vec4) => {Value::Vec4};
  (int) => {Value::Int};
  (ivec2) => {Value::IVec2};
  (ivec3) => {Value::IVec3};
  (ivec4) => {Value::IVec4};
  (uint) => {Value::Uint};
  (uvec2) => {Value::UVec2};
  (uvec3) => {Value::UVec3};
  (uvec4) => {Value::UVec4};
  (uimage2d) => {Value::UImage2D};
  (void) => {Value::Void};
}
macro_rules! builtin_body {
  (void, $body:block) => {{ $body; Value::Void }};
  ($rtype:ident, $body:block) => { builtin_value!($rtype)($body) };
}
macro_rules! builtin_body_bindings {
  ($vars:ident, () => $rtype:ident $body:block) => {{
    let _ = $vars; // Prevents unused_variables warning
    builtin_body!($rtype, $body)
  }};
  ($vars:ident, ($($aname:ident : $atype:ident),+) => $rtype:ident $body:block) => {
    if let (
      $( &builtin_value_ref!($atype, $aname), )*
    ) = (
      $( $vars.get(&stringify!($aname).to_string()), )*
    ) {
      builtin_body!($rtype, $body)
    } else { unreachable!() }
  };
}

macro_rules! builtin_raw {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident),*) => $rtype:ident $func:expr) => {{
    let func_name = $fname.to_string();

    let prototype = FunctionPrototype{
      typ: builtin_type!($rtype),
      name: func_name.clone(),
      params: vec![
        $(
          (builtin_type!($(!$flag)* $atype), Some((stringify!($aname).to_string(), vec![]))),
        )*
      ],
    };

    let func = $func;

    $funcs.entry(func_name).or_insert_with(Vec::new)
      .push((prototype, Statement::Builtin(Func(Box::new(func)))));
  }}
}

macro_rules! builtin {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident),*) => $rtype:ident $body:block) => {{
    let func = |vars: &mut Vars| -> Value {
      builtin_body_bindings!(vars, ($($aname : $atype),*) => $rtype $body)
    };

    builtin_raw!($funcs, $fname, ($($aname : $(!$flag)* $atype),*) => $rtype func)
  }}
}

// Do a lazy static thing
pub fn all() -> HashMap<String, Vec<(FunctionPrototype, Statement)>> {
  use std::sync::atomic::{AtomicU32,Ordering};

  let mut funcs = HashMap::new();

  builtin!(funcs, "abs", (f: float) => float {
    f.abs()
  });
  builtin!(funcs, "abs", (v: vec2) => vec2 {
    [v[0].abs(), v[1].abs()]
  });
  builtin!(funcs, "abs", (v: vec3) => vec3 {
    [v[0].abs(), v[1].abs(), v[2].abs()]
  });
  builtin!(funcs, "abs", (v: vec4) => vec4 {
    [v[0].abs(), v[1].abs(), v[2].abs(), v[3].abs()]
  });
  builtin!(funcs, "abs", (f: int) => int {
    f.abs()
  });
  builtin!(funcs, "abs", (v: ivec2) => ivec2 {
    [v[0].abs(), v[1].abs()]
  });
  builtin!(funcs, "abs", (v: ivec3) => ivec3 {
    [v[0].abs(), v[1].abs(), v[2].abs()]
  });
  builtin!(funcs, "abs", (v: ivec4) => ivec4 {
    [v[0].abs(), v[1].abs(), v[2].abs(), v[3].abs()]
  });

  builtin!(funcs, "imageLoad", (i: uimage2d, t: ivec2) => uvec4 {
    let (x, y) = (t[0] as usize, t[1] as usize);
    let texel = y * i.width + x;

    let p = i.buffer.as_ptr() as *const u32;
    let r = unsafe{ *p.offset(texel as isize) };

    [r, 0, 0, 1]
  });

  builtin!(funcs, "imageStore", (i: uimage2d, t: ivec2, v: uvec4) => void {
    let (x, y) = (t[0] as usize, t[1] as usize);
    let texel = y * i.width + x;

    let p = i.buffer.as_ptr() as *mut u32;

    unsafe{ *p.offset(texel as isize) = v[0] };
  });

  builtin!(funcs, "imageAtomicAdd", (i: uimage2d, t: ivec2, d: uint) => uint {
    let (x, y) = (t[0] as usize, t[1] as usize);
    let texel = y * i.width + x;

    let p = i.buffer.as_ptr() as *const AtomicU32;
    let p = unsafe{ &*p.offset(texel as isize) };

    p.fetch_add(*d, Ordering::AcqRel)
  });

  builtin!(funcs, "memoryBarrierBuffer", () => void {});

  builtin_raw!(funcs, "barrier", () => void |vars: &mut Vars| {
    if let &Value::Barrier(ref barrier) = vars.get(&"__barrier".to_string()) {
      barrier.wait();
      Value::Void
    } else { unreachable!() }
  });

  builtin!(funcs, "atomicAdd", (p: !inout uint_buffer, v: uint) => uint {
    let a = unsafe{ &*(*p as *const AtomicU32) };

    a.fetch_add(*v, Ordering::AcqRel)
  });

  builtin!(funcs, "atomicCounterIncrement", (p: atomic_uint) => uint {
    let a = unsafe{ &*(*p as *const AtomicU32) };

    a.fetch_add(1, Ordering::AcqRel)
  });

  funcs
}
