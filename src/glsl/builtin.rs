use super::parse::{TypeSpecifierNonArray,TypeQualifier,StorageQualifier,Statement,FunctionPrototype};
use super::interpret::{Vars,Value};
use std::collections::HashMap;
use string_cache::DefaultAtom as Atom;

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
  (genutype #four) => {TypeSpecifierNonArray::UVec4};
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
      $( $vars.get(&stringify!($aname).into()), )*
    ) {
      builtin_body!($rtype $(#$rnum)*, $body)
    } else { unreachable!() }
  };
}

macro_rules! builtin_raw {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident $(#$anum:ident)*),*) => $rtype:ident $(#$rnum:ident)* - $func:expr) => {{
    let func_name: Atom = $fname.into();

    let prototype = FunctionPrototype{
      typ: builtin_type!($rtype $(#$rnum)*),
      name: func_name.clone(),
      params: vec![
        $(
          (builtin_type!($(!$flag)* $atype $(#$anum)*), Some((stringify!($aname).into(), vec![]))),
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

macro_rules! index_gentype {
  ($i:expr, $aname:ident : genftype) => { let $aname = $aname[$i]; };
  ($i:expr, $aname:ident : genitype) => { let $aname = $aname[$i]; };
  ($i:expr, $aname:ident : genutype) => { let $aname = $aname[$i]; };
  ($i:expr, $aname:ident : genbtype) => { let $aname = $aname[$i]; };
  ($i:expr, $aname:ident : $(!$flag:ident)* $atype:ident) => {};
}

macro_rules! builtin_gentype {
  ($funcs:ident, $fname:expr, ($($aname:ident : $(!$flag:ident)* $atype:ident),*) => $rtype:ident $body:block) => {{
    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #one),*) => $rtype #one $body);

    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #two),*) => $rtype #two {
      [
        { $(index_gentype!(0, $aname : $(!$flag)* $atype);)* $body },
        { $(index_gentype!(1, $aname : $(!$flag)* $atype);)* $body },
      ]
    });

    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #three),*) => $rtype #three {
      [
        { $(index_gentype!(0, $aname : $(!$flag)* $atype);)* $body },
        { $(index_gentype!(1, $aname : $(!$flag)* $atype);)* $body },
        { $(index_gentype!(2, $aname : $(!$flag)* $atype);)* $body },
      ]
    });

    builtin!($funcs, $fname, ($($aname : $(!$flag)* $atype #four),*) => $rtype #four {
      [
        { $(index_gentype!(0, $aname : $(!$flag)* $atype);)* $body },
        { $(index_gentype!(1, $aname : $(!$flag)* $atype);)* $body },
        { $(index_gentype!(2, $aname : $(!$flag)* $atype);)* $body },
        { $(index_gentype!(3, $aname : $(!$flag)* $atype);)* $body },
      ]
    });
  }}
}

// TODO: Do a lazy static thing
pub fn all() -> HashMap<Atom, Vec<(FunctionPrototype, Statement)>> {
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
    if let &Value::Float(x) = vars.get(&"x".into()) {
      vars.get_mut(&"i".into()).set(Value::Float(x.trunc()));
      Value::Float(x.fract())
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "modf", (x: vec2, i: !out vec2) => vec2 - |vars: &mut Vars| {
    if let &Value::Vec2(x) = vars.get(&"x".into()) {
      vars.get_mut(&"i".into()).set(Value::Vec2([x[0].trunc(), x[1].trunc()]));
      Value::Vec2([x[0].fract(), x[1].fract()])
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "modf", (x: vec3, i: !out vec3) => vec3 - |vars: &mut Vars| {
    if let &Value::Vec3(x) = vars.get(&"x".into()) {
      vars.get_mut(&"i".into()).set(Value::Vec3([x[0].trunc(), x[1].trunc(), x[2].trunc()]));
      Value::Vec3([x[0].fract(), x[1].fract(), x[2].fract()])
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "modf", (x: vec4, i: !out vec4) => vec4 - |vars: &mut Vars| {
    if let &Value::Vec4(x) = vars.get(&"x".into()) {
      vars.get_mut(&"i".into()).set(Value::Vec4([x[0].trunc(), x[1].trunc(), x[2].trunc(), x[3].trunc()]));
      Value::Vec4([x[0].fract(), x[1].fract(), x[2].fract(), x[3].fract()])
    } else { unreachable!() }
  });

  builtin_gentype!(funcs, "isnan", (f: genftype) => genbtype {
    f.is_nan() as u32
  });

  builtin_gentype!(funcs, "isinf", (f: genftype) => genbtype {
    f.is_infinite() as u32
  });

  builtin_gentype!(funcs, "floatBitsToInt", (f: genftype) => genitype {
    f.to_bits() as i32
  });
  builtin_gentype!(funcs, "floatBitsToUint", (f: genftype) => genutype {
    f.to_bits()
  });

  fn frexp(x: f32) -> (f32, i32) {
    if x == 0.0 { return (0.0, 0); }

    let bits = x.to_bits();

    let exp = ((bits >> 23) & 0xff) as i32 - 126;
    let s = (bits | 0x3f000000) & !0x40800000;
    (f32::from_bits(s), exp)
  }
  builtin_raw!(funcs, "frexp", (x: float, exp: !out int) => float - |vars: &mut Vars| {
    if let &Value::Float(x) = vars.get(&"x".into()) {
      let(s, e) = frexp(x);

      vars.get_mut(&"exp".into()).set(Value::Int(e));
      Value::Float(s)
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "frexp", (x: vec2, exp: !out ivec2) => vec2 - |vars: &mut Vars| {
    if let &Value::Vec2(x) = vars.get(&"x".into()) {
      let(s0, e0) = frexp(x[0]);
      let(s1, e1) = frexp(x[1]);

      vars.get_mut(&"exp".into()).set(Value::IVec2([e0, e1]));
      Value::Vec2([s0, s1])
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "frexp", (x: vec3, exp: !out ivec3) => vec3 - |vars: &mut Vars| {
    if let &Value::Vec3(x) = vars.get(&"x".into()) {
      let(s0, e0) = frexp(x[0]);
      let(s1, e1) = frexp(x[1]);
      let(s2, e2) = frexp(x[2]);

      vars.get_mut(&"exp".into()).set(Value::IVec3([e0, e1, e2]));
      Value::Vec3([s0, s1, s2])
    } else { unreachable!() }
  });
  builtin_raw!(funcs, "frexp", (x: vec4, exp: !out ivec4) => vec4 - |vars: &mut Vars| {
    if let &Value::Vec4(x) = vars.get(&"x".into()) {
      let(s0, e0) = frexp(x[0]);
      let(s1, e1) = frexp(x[1]);
      let(s2, e2) = frexp(x[2]);
      let(s3, e3) = frexp(x[3]);

      vars.get_mut(&"exp".into()).set(Value::IVec4([e0, e1, e2, e3]));
      Value::Vec4([s0, s1, s2, s3])
    } else { unreachable!() }
  });

  builtin_gentype!(funcs, "ldexp", (x: genftype, exp: genitype) => genftype {
    if x == 0.0 {
      0.0
    } else {
      let bits = x.to_bits();
      let x_exp = ((bits >> 23) & 0xff) as i32 - 126;
      let bits = (bits & !0x7f800000) | ((exp + 126 + x_exp) as u32 & 0xff) << 23;

      f32::from_bits(bits)
    }
  });

  builtin_gentype!(funcs, "fma", (a: genftype, b: genftype, c: genftype) => genftype {
    a.mul_add(b, c)
  });

  builtin_gentype!(funcs, "intBitsToFloat", (i: genitype) => genftype {
    f32::from_bits(i as u32)
  });
  builtin_gentype!(funcs, "uintBitsToFloat", (u: genutype) => genftype {
    f32::from_bits(u)
  });

  // TODO: gentype macro functions with `out` params
  builtin_raw!(funcs, "uaddCarry", (x: uint, y: uint, carry: !out uint) => uint - |vars: &mut Vars| {
    if let (&Value::Uint(x),
            &Value::Uint(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (sum, carry) = x.overflowing_add(y);

        vars.get_mut(&"carry".into()).set(Value::Uint(carry as u32));
        Value::Uint(sum)
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "uaddCarry", (x: uvec2, y: uvec2, carry: !out uvec2) => uvec2 - |vars: &mut Vars| {
    if let (&Value::UVec2(x),
            &Value::UVec2(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (sum0, carry0) = x[0].overflowing_add(y[0]);
        let (sum1, carry1) = x[1].overflowing_add(y[1]);

        vars.get_mut(&"carry".into()).set(Value::UVec2([carry0 as u32, carry1 as u32]));
        Value::UVec2([sum0, sum1])
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "uaddCarry", (x: uvec3, y: uvec3, carry: !out uvec3) => uvec3 - |vars: &mut Vars| {
    if let (&Value::UVec3(x),
            &Value::UVec3(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (sum0, carry0) = x[0].overflowing_add(y[0]);
        let (sum1, carry1) = x[1].overflowing_add(y[1]);
        let (sum2, carry2) = x[2].overflowing_add(y[2]);

        vars.get_mut(&"carry".into()).set(Value::UVec3([carry0 as u32, carry1 as u32, carry2 as u32]));
        Value::UVec3([sum0, sum1, sum2])
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "uaddCarry", (x: uvec4, y: uvec4, carry: !out uvec4) => uvec4 - |vars: &mut Vars| {
    if let (&Value::UVec4(x),
            &Value::UVec4(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (sum0, carry0) = x[0].overflowing_add(y[0]);
        let (sum1, carry1) = x[1].overflowing_add(y[1]);
        let (sum2, carry2) = x[2].overflowing_add(y[2]);
        let (sum3, carry3) = x[3].overflowing_add(y[3]);

        vars.get_mut(&"carry".into()).set(Value::UVec4([carry0 as u32, carry1 as u32, carry2 as u32, carry3 as u32]));
        Value::UVec4([sum0, sum1, sum2, sum3])
      } else { unreachable!() }
  });

  builtin_raw!(funcs, "usubBorrow", (x: uint, y: uint, borrow: !out uint) => uint - |vars: &mut Vars| {
    if let (&Value::Uint(x),
            &Value::Uint(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (diff, borrow) = x.overflowing_sub(y);

        vars.get_mut(&"borrow".into()).set(Value::Uint(borrow as u32));
        Value::Uint(diff)
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "usubBorrow", (x: uvec2, y: uvec2, borrow: !out uvec2) => uvec2 - |vars: &mut Vars| {
    if let (&Value::UVec2(x),
            &Value::UVec2(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (diff0, borrow0) = x[0].overflowing_sub(y[0]);
        let (diff1, borrow1) = x[1].overflowing_sub(y[1]);

        vars.get_mut(&"borrow".into()).set(Value::UVec2([borrow0 as u32, borrow1 as u32]));
        Value::UVec2([diff0, diff1])
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "usubBorrow", (x: uvec3, y: uvec3, borrow: !out uvec3) => uvec3 - |vars: &mut Vars| {
    if let (&Value::UVec3(x),
            &Value::UVec3(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (diff0, borrow0) = x[0].overflowing_sub(y[0]);
        let (diff1, borrow1) = x[1].overflowing_sub(y[1]);
        let (diff2, borrow2) = x[2].overflowing_sub(y[2]);

        vars.get_mut(&"borrow".into()).set(Value::UVec3([borrow0 as u32, borrow1 as u32, borrow2 as u32]));
        Value::UVec3([diff0, diff1, diff2])
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "usubBorrow", (x: uvec4, y: uvec4, borrow: !out uvec4) => uvec4 - |vars: &mut Vars| {
    if let (&Value::UVec4(x),
            &Value::UVec4(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let (diff0, borrow0) = x[0].overflowing_sub(y[0]);
        let (diff1, borrow1) = x[1].overflowing_sub(y[1]);
        let (diff2, borrow2) = x[2].overflowing_sub(y[2]);
        let (diff3, borrow3) = x[3].overflowing_sub(y[3]);

        vars.get_mut(&"borrow".into()).set(Value::UVec4([borrow0 as u32, borrow1 as u32, borrow2 as u32, borrow3 as u32]));
        Value::UVec4([diff0, diff1, diff2, diff3])
      } else { unreachable!() }
  });

  builtin_raw!(funcs, "umulExtended", (x: uint, y: uint, msb: !out uint, lsb: !out uint) => void - |vars: &mut Vars| {
    if let (&Value::Uint(x),
            &Value::Uint(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product: [u32; 2] = unsafe{ ::std::mem::transmute(x as u64 * y as u64) };

        vars.get_mut(&"msb".into()).set(Value::Uint(product[1] as u32));
        vars.get_mut(&"lsb".into()).set(Value::Uint(product[0] as u32));
        Value::Void
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "umulExtended", (x: uvec2, y: uvec2, msb: !out uvec2, lsb: !out uvec2) => void - |vars: &mut Vars| {
    if let (&Value::UVec2(x),
            &Value::UVec2(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product0: [u32; 2] = unsafe{ ::std::mem::transmute(x[0] as u64 * y[0] as u64) };
        let product1: [u32; 2] = unsafe{ ::std::mem::transmute(x[1] as u64 * y[1] as u64) };

        vars.get_mut(&"msb".into()).set(Value::UVec2([product0[1] as u32, product1[1] as u32]));
        vars.get_mut(&"lsb".into()).set(Value::UVec2([product0[0] as u32, product1[0] as u32]));
        Value::Void
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "umulExtended", (x: uvec3, y: uvec3, msb: !out uvec3, lsb: !out uvec3) => void - |vars: &mut Vars| {
    if let (&Value::UVec3(x),
            &Value::UVec3(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product0: [u32; 2] = unsafe{ ::std::mem::transmute(x[0] as u64 * y[0] as u64) };
        let product1: [u32; 2] = unsafe{ ::std::mem::transmute(x[1] as u64 * y[1] as u64) };
        let product2: [u32; 2] = unsafe{ ::std::mem::transmute(x[2] as u64 * y[2] as u64) };

        vars.get_mut(&"msb".into()).set(Value::UVec3([product0[1] as u32, product1[1] as u32, product2[1] as u32]));
        vars.get_mut(&"lsb".into()).set(Value::UVec3([product0[0] as u32, product1[0] as u32, product2[0] as u32]));
        Value::Void
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "umulExtended", (x: uvec4, y: uvec4, msb: !out uvec4, lsb: !out uvec4) => void - |vars: &mut Vars| {
    if let (&Value::UVec4(x),
            &Value::UVec4(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product0: [u32; 2] = unsafe{ ::std::mem::transmute(x[0] as u64 * y[0] as u64) };
        let product1: [u32; 2] = unsafe{ ::std::mem::transmute(x[1] as u64 * y[1] as u64) };
        let product2: [u32; 2] = unsafe{ ::std::mem::transmute(x[2] as u64 * y[2] as u64) };
        let product3: [u32; 2] = unsafe{ ::std::mem::transmute(x[3] as u64 * y[3] as u64) };

        vars.get_mut(&"msb".into()).set(Value::UVec4([product0[1] as u32, product1[1] as u32, product2[1] as u32, product3[1] as u32]));
        vars.get_mut(&"lsb".into()).set(Value::UVec4([product0[0] as u32, product1[0] as u32, product2[0] as u32, product3[0] as u32]));
        Value::Void
      } else { unreachable!() }
  });

  builtin_raw!(funcs, "imulExtended", (x: int, y: int, msb: !out int, lsb: !out int) => void - |vars: &mut Vars| {
    if let (&Value::Int(x),
            &Value::Int(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product: [i32; 2] = unsafe{ ::std::mem::transmute(x as i64 * y as i64) };

        vars.get_mut(&"msb".into()).set(Value::Int(product[1] as i32));
        vars.get_mut(&"lsb".into()).set(Value::Int(product[0] as i32));
        Value::Void
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "imulExtended", (x: ivec2, y: ivec2, msb: !out ivec2, lsb: !out ivec2) => void - |vars: &mut Vars| {
    if let (&Value::IVec2(x),
            &Value::IVec2(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product0: [i32; 2] = unsafe{ ::std::mem::transmute(x[0] as i64 * y[0] as i64) };
        let product1: [i32; 2] = unsafe{ ::std::mem::transmute(x[1] as i64 * y[1] as i64) };

        vars.get_mut(&"msb".into()).set(Value::IVec2([product0[1] as i32, product1[1] as i32]));
        vars.get_mut(&"lsb".into()).set(Value::IVec2([product0[0] as i32, product1[0] as i32]));
        Value::Void
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "imulExtended", (x: ivec3, y: ivec3, msb: !out ivec3, lsb: !out ivec3) => void - |vars: &mut Vars| {
    if let (&Value::IVec3(x),
            &Value::IVec3(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product0: [i32; 2] = unsafe{ ::std::mem::transmute(x[0] as i64 * y[0] as i64) };
        let product1: [i32; 2] = unsafe{ ::std::mem::transmute(x[1] as i64 * y[1] as i64) };
        let product2: [i32; 2] = unsafe{ ::std::mem::transmute(x[2] as i64 * y[2] as i64) };

        vars.get_mut(&"msb".into()).set(Value::IVec3([product0[1] as i32, product1[1] as i32, product2[1] as i32]));
        vars.get_mut(&"lsb".into()).set(Value::IVec3([product0[0] as i32, product1[0] as i32, product2[0] as i32]));
        Value::Void
      } else { unreachable!() }
  });
  builtin_raw!(funcs, "imulExtended", (x: ivec4, y: ivec4, msb: !out ivec4, lsb: !out ivec4) => void - |vars: &mut Vars| {
    if let (&Value::IVec4(x),
            &Value::IVec4(y))
      = (vars.get(&"x".into()),
         vars.get(&"y".into())) {

        let product0: [i32; 2] = unsafe{ ::std::mem::transmute(x[0] as i64 * y[0] as i64) };
        let product1: [i32; 2] = unsafe{ ::std::mem::transmute(x[1] as i64 * y[1] as i64) };
        let product2: [i32; 2] = unsafe{ ::std::mem::transmute(x[2] as i64 * y[2] as i64) };
        let product3: [i32; 2] = unsafe{ ::std::mem::transmute(x[3] as i64 * y[3] as i64) };

        vars.get_mut(&"msb".into()).set(Value::IVec4([product0[1] as i32, product1[1] as i32, product2[1] as i32, product3[1] as i32]));
        vars.get_mut(&"lsb".into()).set(Value::IVec4([product0[0] as i32, product1[0] as i32, product2[0] as i32, product3[0] as i32]));
        Value::Void
      } else { unreachable!() }
  });

  builtin_gentype!(funcs, "bitfieldExtract", (value: genitype, offset: int, bits: int) => genitype {
    if bits == 0 {
      0
    } else {
      (value << (32 - offset - bits)) >> (32 - bits)
    }
  });
  builtin_gentype!(funcs, "bitfieldExtract", (value: genutype, offset: int, bits: int) => genutype {
    if bits == 0 {
      0
    } else {
      (value << (32 - offset - bits)) >> (32 - bits)
    }
  });


  builtin_gentype!(funcs, "bitfieldInsert", (value: genitype,
                                             insert: genitype,
                                             offset: int,
                                             bits: int) => genitype {
    if bits == 0 {
      value
    } else {
      let mask = (u32::max_value() >> (32 - bits) << offset) as i32;
      let insert_shifted = insert << offset;

      (value & !mask) | (insert_shifted & mask)
    }
  });
  builtin_gentype!(funcs, "bitfieldInsert", (value: genutype,
                                             insert: genutype,
                                             offset: int,
                                             bits: int) => genutype {
    if bits == 0 {
      value
    } else {
      let mask = u32::max_value() >> (32 - bits) << offset;
      let insert_shifted = insert << offset;

      (value & !mask) | (insert_shifted & mask)
    }
  });


  builtin_gentype!(funcs, "bitfieldReverse", (value: genitype) => genitype {
    let mut value = value;
    let mut res = 0;

    for _ in 0..32 {
      res <<= 1;
      if (value & 1) == 1 {
        res |= 1;
      }
      value >>= 1;
    }
    res
  });
  builtin_gentype!(funcs, "bitfieldReverse", (value: genutype) => genutype {
    let mut value = value;
    let mut res = 0;

    for _ in 0..32 {
      res <<= 1;
      if (value & 1) == 1 {
        res |= 1;
      }
      value >>= 1;
    }
    res
  });


  builtin_gentype!(funcs, "bitCount", (value: genitype) => genitype {
    value.count_ones() as i32
  });
  builtin_gentype!(funcs, "bitCount", (value: genutype) => genitype {
    value.count_ones() as i32
  });

  builtin_gentype!(funcs, "findLSB", (value: genitype) => genitype {
    if value == 0 {
      -1
    } else {
      value.trailing_zeros() as i32
    }
  });
  builtin_gentype!(funcs, "findLSB", (value: genutype) => genitype {
    if value == 0 {
      -1
    } else {
      value.trailing_zeros() as i32
    }
  });

  builtin_gentype!(funcs, "findMSB", (value: genitype) => genitype {
    if value < 0 {
      31 - (!value).leading_zeros() as i32
    } else {
      31 - value.leading_zeros() as i32
    }
  });
  builtin_gentype!(funcs, "findMSB", (value: genutype) => genitype {
    31 - value.leading_zeros() as i32
  });

  builtin!(funcs, "cross", (x: vec3, y: vec3) => vec3 {
    [
      x[1]*y[2] - y[1]*x[2],
      x[2]*y[0] - y[2]*x[0],
      x[0]*y[1] - y[0]*x[1],
    ]
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
    if let &Value::Barrier(ref barrier) = vars.get(&"__barrier".into()) {
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
