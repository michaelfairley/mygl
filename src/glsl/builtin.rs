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
  (out) => {TypeQualifier::Storage(StorageQualifier::Out)};
}
macro_rules! builtin_flags {
  ($($flags:ident)*) => { vec![$(builtin_flag!($flags))*] };
}
macro_rules! builtin_type_specifier_nongen {
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
macro_rules! builtin_type_specifier {
  (genftype #one) => {TypeSpecifierNonArray::Float};
  (genftype #two) => {TypeSpecifierNonArray::Vec2};
  (genftype #three) => {TypeSpecifierNonArray::Vec3};
  (genftype #four) => {TypeSpecifierNonArray::Vec4};
  (genitype #one) => {TypeSpecifierNonArray::Int};
  (genitype #two) => {TypeSpecifierNonArray::IVec2};
  (genitype #three) => {TypeSpecifierNonArray::IVec3};
  (genitype #four) => {TypeSpecifierNonArray::IVec4};
  (genutype #one) => {TypeSpecifierNonArray::Uint};
  (genutype #two) => {TypeSpecifierNonArray::UVec2};
  (genutype #three) => {TypeSpecifierNonArray::UVec3};
  (genutype #four) => {TypeSpecifierNonArray::Vec4};
  (genbtype #one) => {TypeSpecifierNonArray::Bool};
  (genbtype #two) => {TypeSpecifierNonArray::BVec2};
  (genbtype #three) => {TypeSpecifierNonArray::BVec3};
  (genbtype #four) => {TypeSpecifierNonArray::BVec4};
  ($type:ident $(#$num:ident)*) => {builtin_type_specifier_nongen!($type)};
}
macro_rules! builtin_type {
  ($(!$flag:ident)* $type:ident $(#$num:ident)*) => {(builtin_flags!($($flag)*), (builtin_type_specifier!($type $(#$num)*), vec![]))};
}
macro_rules! builtin_value_ref_nongen {
  (float, $aname:ident) => {Value::Float($aname)};
  (vec2, $aname:ident) => {Value::Vec2(ref $aname)};
  (vec3, $aname:ident) => {Value::Vec3(ref $aname)};
  (vec4, $aname:ident) => {Value::Vec4(ref $aname)};
  (int, $aname:ident) => {Value::Int($aname)};
  (ivec2, $aname:ident) => {Value::IVec2(ref $aname)};
  (ivec3, $aname:ident) => {Value::IVec3(ref $aname)};
  (ivec4, $aname:ident) => {Value::IVec4(ref $aname)};
  (uint, $aname:ident) => {Value::Uint($aname)};
  (uint_buffer, $aname:ident) => {Value::Buffer(TypeSpecifierNonArray::Uint, ref $aname, _)};
  (uvec2, $aname:ident) => {Value::UVec2(ref $aname)};
  (uvec3, $aname:ident) => {Value::UVec3(ref $aname)};
  (uvec4, $aname:ident) => {Value::UVec4(ref $aname)};
  (uimage2d, $aname:ident) => {Value::UImage2D(ref $aname)};
  (atomic_uint, $aname:ident) => {Value::Buffer(_, ref $aname, _)};
}
macro_rules! builtin_value_ref {
  (genftype #one, $aname:ident) => {Value::Float($aname)};
  (genftype #two, $aname:ident) => {Value::Vec2(ref $aname)};
  (genftype #three, $aname:ident) => {Value::Vec3(ref $aname)};
  (genftype #four, $aname:ident) => {Value::Vec4(ref $aname)};
  (genitype #one, $aname:ident) => {Value::Int($aname)};
  (genitype #two, $aname:ident) => {Value::IVec2(ref $aname)};
  (genitype #three, $aname:ident) => {Value::IVec3(ref $aname)};
  (genitype #four, $aname:ident) => {Value::IVec4(ref $aname)};
  (genutype #one, $aname:ident) => {Value::Uint($aname)};
  (genutype #two, $aname:ident) => {Value::UVec2(ref $aname)};
  (genutype #three, $aname:ident) => {Value::UVec3(ref $aname)};
  (genutype #four, $aname:ident) => {Value::UVec4(ref $aname)};
  (genbtype #one, $aname:ident) => {Value::Bool($aname)};
  (genbtype #two, $aname:ident) => {Value::BVec2(ref $aname)};
  (genbtype #three, $aname:ident) => {Value::BVec3(ref $aname)};
  (genbtype #four, $aname:ident) => {Value::BVec4(ref $aname)};
  ($type:ident $(#$num:ident)*, $aname:ident) => {builtin_value_ref_nongen!($type, $aname)};
}
macro_rules! builtin_value_nongen {
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
macro_rules! builtin_value {
  (genftype #one) => {Value::Float};
  (genftype #two) => {Value::Vec2};
  (genftype #three) => {Value::Vec3};
  (genftype #four) => {Value::Vec4};
  (genitype #one) => {Value::Int};
  (genitype #two) => {Value::IVec2};
  (genitype #three) => {Value::IVec3};
  (genitype #four) => {Value::IVec4};
  (genutype #one) => {Value::Uint};
  (genutype #two) => {Value::UVec2};
  (genutype #three) => {Value::UVec3};
  (genutype #four) => {Value::UVec4};
  (genbtype #one) => {Value::Bool};
  (genbtype #two) => {Value::BVec2};
  (genbtype #three) => {Value::BVec3};
  (genbtype #four) => {Value::BVec4};
  ($type:ident $(#$num:ident)*) => {builtin_value_nongen!($type)};
}
macro_rules! builtin_body {
  (void $(#$rnum:ident)*, $body:block) => {{ $body; Value::Void }};
  ($rtype:ident $(#$rnum:ident)*, $body:block) => { builtin_value!($rtype $(#$rnum)*)($body) };
}
macro_rules! builtin_body_bindings {
  ($vars:ident, () => $rtype:ident $(#$rnum:ident)* $body:block) => {{
    let _ = $vars; // Prevents unused_variables warning
    builtin_body!($rtype, $body)
  }};
  ($vars:ident, ($($aname:ident : $atype:ident $(#$anum:ident)*),+) => $rtype:ident $(#$rnum:ident)* $body:block) => {
    if let (
      $( &builtin_value_ref!($atype $(#$anum)*, $aname), )*
    ) = (
      $( $vars.get(&stringify!($aname).to_string()), )*
    ) {
      builtin_body!($rtype $(#$rnum)*, $body)
    } else { unreachable!() }
  };
}

macro_rules! builtin_raw {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident $(#$anum:ident)*),*) => $rtype:ident $(#$rnum:ident)* - $func:expr) => {{
    let func_name = $fname.to_string();

    let prototype = FunctionPrototype{
      typ: builtin_type!($rtype $(#$rnum)*),
      name: func_name.clone(),
      params: vec![
        $(
          (builtin_type!($(!$flag)* $atype $(#$anum)*), Some((stringify!($aname).to_string(), vec![]))),
        )*
      ],
    };

    let func = $func;

    $funcs.entry(func_name).or_insert_with(Vec::new)
      .push((prototype, Statement::Builtin(Func(Box::new(func)))));
  }}
}

macro_rules! builtin {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident $(#$anum:ident)*),*) => $rtype:ident $(#$rnum:ident)* $body:block) => {{
    let func = |vars: &mut Vars| -> Value {
      builtin_body_bindings!(vars, ($($aname : $atype $(#$anum)*),*) => $rtype $(#$rnum)* $body)
    };

    builtin_raw!($funcs, $fname, ($($aname : $(!$flag)* $atype $(#$anum)*),*) => $rtype $(#$rnum)* - func)
  }}
}

macro_rules! builtin_gentype {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident),*) => $rtype:ident $body:block) => {{
    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #one),*) => $rtype #one $body);

    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #two),*) => $rtype #two {
      [
        { $(let $aname = $aname[0];)* $body },
        { $(let $aname = $aname[1];)* $body },
      ]
    });

    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #three),*) => $rtype #three {
      [
        { $(let $aname = $aname[0];)* $body },
        { $(let $aname = $aname[1];)* $body },
        { $(let $aname = $aname[2];)* $body },
      ]
    });

    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #four),*) => $rtype #four {
      [
        { $(let $aname = $aname[0];)* $body },
        { $(let $aname = $aname[1];)* $body },
        { $(let $aname = $aname[2];)* $body },
        { $(let $aname = $aname[3];)* $body },
      ]
    });
  }}
}

// TODO: Do a lazy static thing
pub fn all() -> HashMap<String, Vec<(FunctionPrototype, Statement)>> {
  use std::sync::atomic::{AtomicU32,Ordering};

  let mut funcs = HashMap::new();

  builtin_gentype!(funcs, "abs", (f: genftype) => genftype {
    f.abs()
  });
  builtin_gentype!(funcs, "abs", (i: genitype) => genitype {
    i.abs()
  });

  builtin_gentype!(funcs, "sign", (f: genftype) => genftype {
    if f > 0.0 {
      1.0
    } else if f < 0.0 {
      -1.0
    } else {
      0.0
    }
  });
  builtin_gentype!(funcs, "sign", (i: genitype) => genitype {
    i.signum()
  });

  builtin_gentype!(funcs, "floor", (f: genftype) => genftype {
    f.floor()
  });

  builtin_gentype!(funcs, "trunc", (f: genftype) => genftype {
    f.trunc()
  });

  builtin_gentype!(funcs, "round", (f: genftype) => genftype {
    f.round()
  });

  builtin_gentype!(funcs, "roundEven", (f: genftype) => genftype {
    if f.fract().abs() == 0.5 {
      let trunc = f.trunc();
      if trunc % 2.0 == 0.0 {
        trunc
      } else if f > 0.0 {
        trunc + 1.0
      } else {
        trunc - 1.0
      }
    } else {
      f.round()
    }
  });

  builtin_gentype!(funcs, "ceil", (f: genftype) => genftype {
    f.ceil()
  });

  builtin_gentype!(funcs, "fract", (f: genftype) => genftype {
    (f.fract() + 1.0) % 1.0
  });

  builtin_raw!(funcs, "modf", (x: float, i: !out float) => float - |vars: &mut Vars| {
    if let &Value::Float(x) = vars.get(&"x".to_string()) {
      vars.get_mut(&"i".to_string()).set(Value::Float(x.trunc()));
      Value::Float(x.fract())
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "modf", (x: vec2, i: !out vec2) => vec2 - |vars: &mut Vars| {
    if let &Value::Vec2(x) = vars.get(&"x".to_string()) {
      vars.get_mut(&"i".to_string()).set(Value::Vec2([x[0].trunc(), x[1].trunc()]));
      Value::Vec2([x[0].fract(), x[1].fract()])
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "modf", (x: vec3, i: !out vec3) => vec3 - |vars: &mut Vars| {
    if let &Value::Vec3(x) = vars.get(&"x".to_string()) {
      vars.get_mut(&"i".to_string()).set(Value::Vec3([x[0].trunc(), x[1].trunc(), x[2].trunc()]));
      Value::Vec3([x[0].fract(), x[1].fract(), x[2].fract()])
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "modf", (x: vec4, i: !out vec4) => vec4 - |vars: &mut Vars| {
    if let &Value::Vec4(x) = vars.get(&"x".to_string()) {
      vars.get_mut(&"i".to_string()).set(Value::Vec4([x[0].trunc(), x[1].trunc(), x[2].trunc(), x[3].trunc()]));
      Value::Vec4([x[0].fract(), x[1].fract(), x[2].fract(), x[3].fract()])
    } else { unreachable!() }
  });

  builtin_gentype!(funcs, "isnan", (f: genftype) => genbtype {
    f.is_nan() as u32
  });



  builtin_gentype!(funcs, "min", (a: genftype, b: genftype) => genftype {
    a.min(b)
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

    p.fetch_add(d, Ordering::AcqRel)
  });

  builtin!(funcs, "memoryBarrierBuffer", () => void {});

  builtin_raw!(funcs, "barrier", () => void - |vars: &mut Vars| {
    if let &Value::Barrier(ref barrier) = vars.get(&"__barrier".to_string()) {
      barrier.wait();
      Value::Void
    } else { unreachable!() }
  });

  builtin!(funcs, "atomicAdd", (p: !inout uint_buffer, v: uint) => uint {
    let a = unsafe{ &*(*p as *const AtomicU32) };

    a.fetch_add(v, Ordering::AcqRel)
  });

  builtin!(funcs, "atomicCounterIncrement", (p: atomic_uint) => uint {
    let a = unsafe{ &*(*p as *const AtomicU32) };

    a.fetch_add(1, Ordering::AcqRel)
  });

  funcs
}
