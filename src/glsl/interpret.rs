use super::parse::{Statement,Expression,TypeSpecifierNonArray,Comparison,TypeQualifier,StorageQualifier};
use super::Shader;

use std::sync::{Barrier,Arc};
use std::ops::Deref;

use string_cache::DefaultAtom as Atom;

use gl;

const DEBUG: bool = false;

#[derive(Debug,Clone)]
pub enum Value {
  Int(i32),
  IVec2([i32; 2]),
  IVec3([i32; 3]),
  IVec4([i32; 4]),
  Uint(u32),
  UVec2([u32; 2]),
  UVec3([u32; 3]),
  UVec4([u32; 4]),
  Float(f32),
  Vec2([f32; 2]),
  Vec3([f32; 3]),
  Vec4([f32; 4]),
  Bool(u32),
  BVec2([u32; 2]),
  BVec3([u32; 3]),
  BVec4([u32; 4]),
  Void,

  Mat2([[f32; 2]; 2]),
  Mat2x3([[f32; 3]; 2]),
  Mat2x4([[f32; 4]; 2]),
  Mat3x2([[f32; 2]; 3]),
  Mat3([[f32; 3]; 3]),
  Mat3x4([[f32; 4]; 3]),
  Mat4x2([[f32; 2]; 4]),
  Mat4x3([[f32; 3]; 4]),
  Mat4([[f32; 4]; 4]),

  UImage2DUnit(usize),
  UImage2D(Arc<gl::Texture>),

  Buffer(TypeSpecifierNonArray, *mut u8, Option<u32>),

  Array(Vec<Value>),

  Barrier(Arc<Barrier>),

  Ref(*mut Value),
  RefV2(*mut [f32; 2]),
  RefV3(*mut [f32; 3]),
  RefV4(*mut [f32; 4]),

  RefV22(*mut f32, *mut f32),
  RefV32(*mut f32, *mut f32, *mut f32),
  RefV42(*mut f32, *mut f32, *mut f32, *mut f32),
  RefIV22(*mut i32, *mut i32),
  RefIV32(*mut i32, *mut i32, *mut i32),

  // RefI(*mut Value, usize),
}

impl Value {
  pub fn set(&mut self, val: Value) {
    match self {
      &mut Value::Ref(inner) => {
        let inner = unsafe{ &mut *inner };
        inner.set(val);
      },
      &mut Value::RefV2(v) => {
        if let Value::Vec2(ref t) = val {
          unsafe{ *v = *t; }
        } else { unreachable!() }
      },
      &mut Value::RefV22(a, b) => {
        if let Value::Vec2(ref t) = val {
          unsafe {
            *a = t[0];
            *b = t[1];
          }
        } else { unreachable!() }
      },
      &mut Value::RefV32(a, b, c) => {
        if let Value::Vec3(ref t) = val {
          unsafe {
            *a = t[0];
            *b = t[1];
            *c = t[2];
          }
        } else { unreachable!() }
      },
      &mut Value::RefV42(a, b, c, d) => {
        if let Value::Vec4(ref t) = val {
          unsafe {
            *a = t[0];
            *b = t[1];
            *c = t[2];
            *d = t[3];
          }
        } else { unreachable!() }
      },
      &mut Value::RefIV22(a, b) => {
        if let Value::IVec2(ref t) = val {
          unsafe {
            *a = t[0];
            *b = t[1];
          }
        } else { unreachable!() }
      },
      &mut Value::RefIV32(a, b, c) => {
        if let Value::IVec3(ref t) = val {
          unsafe {
            *a = t[0];
            *b = t[1];
            *c = t[2];
          }
        } else { unreachable!() }
      },
      &mut Value::RefV3(v) => {
        if let Value::Vec3(ref t) = val {
          unsafe{ *v = *t; }
        } else { unreachable!() }
      },
      &mut Value::RefV4(v) => {
        if let Value::Vec4(ref t) = val {
          unsafe{ *v = *t; }
        } else { unreachable!() }
      },
      &mut Value::Buffer(ref typ, ptr, _) => {
        match (typ, val) {
          (&TypeSpecifierNonArray::Uint, Value::Uint(u)) => unsafe{ *(ptr as *mut _) = u },
          (&TypeSpecifierNonArray::UVec2, Value::UVec2(u)) => unsafe{ *(ptr as *mut _) = u },
          (&TypeSpecifierNonArray::UVec3, Value::UVec3(u)) => unsafe{ *(ptr as *mut _) = u },
          (&TypeSpecifierNonArray::UVec4, Value::UVec4(u)) => unsafe{ *(ptr as *mut _) = u },
          (&TypeSpecifierNonArray::Int, Value::Int(i)) => unsafe{ *(ptr as *mut _) = i },
          (&TypeSpecifierNonArray::IVec2, Value::IVec2(i)) => unsafe{ *(ptr as *mut _) = i },
          (&TypeSpecifierNonArray::IVec3, Value::IVec3(i)) => unsafe{ *(ptr as *mut _) = i },
          (&TypeSpecifierNonArray::IVec4, Value::IVec4(i)) => unsafe{ *(ptr as *mut _) = i },
          (&TypeSpecifierNonArray::Float, Value::Float(f)) => unsafe{ *(ptr as *mut _) = f },
          (&TypeSpecifierNonArray::Vec2, Value::Vec2(f)) => unsafe{ *(ptr as *mut _) = f },
          (&TypeSpecifierNonArray::Vec3, Value::Vec3(f)) => unsafe{ *(ptr as *mut _) = f },
          (&TypeSpecifierNonArray::Vec4, Value::Vec4(f)) => unsafe{ *(ptr as *mut _) = f },
          (&TypeSpecifierNonArray::Bool, Value::Bool(b)) => unsafe{ *(ptr as *mut _) = b },
          (&TypeSpecifierNonArray::BVec2, Value::BVec2(b)) => unsafe{ *(ptr as *mut _) = b },
          (&TypeSpecifierNonArray::BVec3, Value::BVec3(b)) => unsafe{ *(ptr as *mut _) = b },
          (&TypeSpecifierNonArray::BVec4, Value::BVec4(b)) => unsafe{ *(ptr as *mut _) = b },
          x => unimplemented!("{:?}", x),
        }
      },
      x => *x = val,
    }
  }

  fn get(&self) -> Value {
    match self {
      &Value::Ref(inner) => {
        let inner = unsafe{ &*inner };
        inner.get()
      },
      &Value::RefV2(p) => {
        Value::Vec2(unsafe{ *p })
      },
      &Value::RefV3(p) => {
        Value::Vec3(unsafe{ *p })
      },
      &Value::RefV4(p) => {
        Value::Vec4(unsafe{ *p })
      },
      &Value::RefV22(a, b) => {
        Value::Vec2(unsafe{ [*a, *b] })
      },
      &Value::RefV32(a, b, c) => {
        Value::Vec3(unsafe{ [*a, *b, *c] })
      },
      &Value::RefV42(a, b, c, d) => {
        Value::Vec4(unsafe{ [*a, *b, *c, *d] })
      },
      &Value::RefIV22(a, b) => {
        Value::IVec2(unsafe{ [*a, *b] })
      },
      &Value::RefIV32(a, b, c) => {
        Value::IVec3(unsafe{ [*a, *b, *c] })
      },
      &Value::Buffer(ref typ, ptr, size) => {
        match typ {
          &TypeSpecifierNonArray::Uint => Value::Uint(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::UVec2 => Value::UVec2(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::UVec3 => Value::UVec3(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::UVec4 => Value::UVec4(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::Int => Value::Int(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::IVec2 => Value::IVec2(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::IVec3 => Value::IVec3(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::IVec4 => Value::IVec4(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::Float => Value::Float(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::Vec2 => Value::Vec2(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::Vec3 => Value::Vec3(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::Vec4 => Value::Vec4(unsafe{ *(ptr as *const _) }),
          &TypeSpecifierNonArray::AtomicUint => Value::Buffer(typ.clone(), ptr, size),
          x => unimplemented!("{:?}", x),
        }
      },
      x => x.clone(),
    }
  }

  fn get_ref(&mut self) -> Value {
    match self {
      &mut Value::Ref(_) => self.clone(),
      &mut Value::Buffer(_, _, _) => self.clone(),
      x => Value::Ref(x as *mut Value),
    }
  }
}

pub fn add(a: &Value, b: &Value) -> Value {
  #[inline]
  fn add2(a: [f32; 2], b: [f32; 2]) -> [f32; 2] {
    [
      a[0] + b[0],
      a[1] + b[1],
    ]
  }
  #[inline]
  fn add3(a: [f32; 3], b: [f32; 3]) -> [f32; 3] {
    [
      a[0] + b[0],
      a[1] + b[1],
      a[2] + b[2],
    ]
  }
  #[inline]
  fn add4(a: [f32; 4], b: [f32; 4]) -> [f32; 4] {
    [
      a[0] + b[0],
      a[1] + b[1],
      a[2] + b[2],
      a[3] + b[3],
    ]
  }

  match (a, b) {
    (&Value::Float(a), &Value::Float(b)) => Value::Float(a + b),
    (&Value::Vec2(a), &Value::Vec2(b)) => Value::Vec2(add2(a, b)),
    (&Value::Vec3(a), &Value::Vec3(b)) => Value::Vec3(add3(a, b)),
    (&Value::Vec4(a), &Value::Vec4(b)) => Value::Vec4(add4(a, b)),
    (&Value::Mat2(a), &Value::Mat2(b)) => Value::Mat2([
      add2(a[0], b[0]),
      add2(a[1], b[1]),
    ]),
    (&Value::Mat2x3(a), &Value::Mat2x3(b)) => Value::Mat2x3([
      add3(a[0], b[0]),
      add3(a[1], b[1]),
    ]),
    (&Value::Mat2x4(a), &Value::Mat2x4(b)) => Value::Mat2x4([
      add4(a[0], b[0]),
      add4(a[1], b[1]),
    ]),
    (&Value::Mat3x2(a), &Value::Mat3x2(b)) => Value::Mat3x2([
      add2(a[0], b[0]),
      add2(a[1], b[1]),
      add2(a[2], b[2]),
    ]),
    (&Value::Mat3(a), &Value::Mat3(b)) => Value::Mat3([
      add3(a[0], b[0]),
      add3(a[1], b[1]),
      add3(a[2], b[2]),
    ]),
    (&Value::Mat3x4(a), &Value::Mat3x4(b)) => Value::Mat3x4([
      add4(a[0], b[0]),
      add4(a[1], b[1]),
      add4(a[2], b[2]),
    ]),
    (&Value::Mat4x2(a), &Value::Mat4x2(b)) => Value::Mat4x2([
      add2(a[0], b[0]),
      add2(a[1], b[1]),
      add2(a[2], b[2]),
      add2(a[3], b[3]),
    ]),
    (&Value::Mat4x3(a), &Value::Mat4x3(b)) => Value::Mat4x3([
      add3(a[0], b[0]),
      add3(a[1], b[1]),
      add3(a[2], b[2]),
      add3(a[3], b[3]),
    ]),
    (&Value::Mat4(a), &Value::Mat4(b)) => Value::Mat4([
      add4(a[0], b[0]),
      add4(a[1], b[1]),
      add4(a[2], b[2]),
      add4(a[3], b[3]),
    ]),
    (&Value::Array(ref a), &Value::Array(ref b)) => Value::Array(a.iter().zip(b.iter()).map(|(a, b)| add(a, b)).collect()),
    x => unimplemented!("{:?}", x),
  }

}

pub fn mul(v: &Value, t: f32) -> Value {
  #[inline]
  fn mul2(v: [f32; 2], t: f32) -> [f32; 2] {
    [
      v[0] * t,
      v[1] * t,
    ]
  }
  #[inline]
  fn mul3(v: [f32; 3], t: f32) -> [f32; 3] {
    [
      v[0] * t,
      v[1] * t,
      v[2] * t,
    ]
  }
  #[inline]
  fn mul4(v: [f32; 4], t: f32) -> [f32; 4] {
    [
      v[0] * t,
      v[1] * t,
      v[2] * t,
      v[3] * t,
    ]
  }

  match v {
    &Value::Float(a) => Value::Float(a * t),
    &Value::Vec2(a) => Value::Vec2(mul2(a, t)),
    &Value::Vec3(a) => Value::Vec3(mul3(a, t)),
    &Value::Vec4(a) => Value::Vec4(mul4(a, t)),
    &Value::Mat2(a) => Value::Mat2([
      mul2(a[0], t),
      mul2(a[1], t),
    ]),
    &Value::Mat2x3(a) => Value::Mat2x3([
      mul3(a[0], t),
      mul3(a[1], t),
    ]),
    &Value::Mat2x4(a) => Value::Mat2x4([
      mul4(a[0], t),
      mul4(a[1], t),
    ]),
    &Value::Mat3x2(a) => Value::Mat3x2([
      mul2(a[0], t),
      mul2(a[1], t),
      mul2(a[2], t),
    ]),
    &Value::Mat3(a) => Value::Mat3([
      mul3(a[0], t),
      mul3(a[1], t),
      mul3(a[2], t),
    ]),
    &Value::Mat3x4(a) => Value::Mat3x4([
      mul4(a[0], t),
      mul4(a[1], t),
      mul4(a[2], t),
    ]),
    &Value::Mat4x2(a) => Value::Mat4x2([
      mul2(a[0], t),
      mul2(a[1], t),
      mul2(a[2], t),
      mul2(a[3], t),
    ]),
    &Value::Mat4x3(a) => Value::Mat4x3([
      mul3(a[0], t),
      mul3(a[1], t),
      mul3(a[2], t),
      mul3(a[3], t),
    ]),
    &Value::Mat4(a) => Value::Mat4([
      mul4(a[0], t),
      mul4(a[1], t),
      mul4(a[2], t),
      mul4(a[3], t),
    ]),
    &Value::Array(ref a) => Value::Array(a.iter().map(|a| mul(a, t)).collect()),
    x => unimplemented!("{:?}", x),
  }

}


impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (&Value::Float(a), &Value::Float(b)) => a == b,
      (&Value::Uint(a), &Value::Uint(b)) => a == b,
      (&Value::Int(a), &Value::Int(b)) => a == b,
      (&Value::Vec4(a), &Value::Vec4(b)) => a == b,
      (&Value::UVec3(a), &Value::UVec3(b)) => a == b,
      (&Value::Bool(a), &Value::Bool(b)) => a == b,
      (&Value::Void, &Value::Void) => true,
      x => unreachable!("Can't compare: {:?}", x),
    }
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
    match (self, other) {
      (&Value::Float(a), &Value::Float(b)) => a.partial_cmp(&b),
      (&Value::Uint(a), &Value::Uint(b)) => a.partial_cmp(&b),
      (&Value::Int(a), &Value::Int(b)) => a.partial_cmp(&b),
      x => unreachable!("Can't compare: {:?}", x),
    }
  }
}

unsafe impl Send for Value {}

#[derive(Debug, Clone)]
pub struct Vars {
  scopes: Vec<Vec<(Atom, Value)>>,
}

impl Vars {
  pub fn new() -> Self {
    Self{
      scopes: vec![vec![]],
    }
  }

  pub fn push(&mut self) {
    self.scopes.push(Vec::new());
  }

  fn pop(&mut self) {
    self.scopes.pop().expect("Tried to pop an empty vars");
  }

  pub fn insert(&mut self, ident: Atom, value: Value) {
    self.scopes.last_mut().unwrap().push((ident, value));
  }

  pub fn get(&self, ident: &Atom) -> &Value {
    for scope in self.scopes.iter().rev() {
      for (name, val) in scope.iter() {
        if name == ident {
          return val;
        }
      }
    }

    panic!("Didn't find {} in the vars", ident);
  }

  pub fn take(&mut self, ident: &Atom) -> Value {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(i) = scope.iter().position(|(name, _)| name == ident) {
        return scope.swap_remove(i).1;
      }
    }

    panic!("Didn't find {} in the vars", ident);
  }

  pub fn get_mut<'a>(&'a mut self, ident: &Atom) -> &mut Value {
    for scope in self.scopes.iter_mut().rev() {
      for (name, val) in scope.iter_mut() {
        if name == ident {
          return val;
        }
      }
    }

    panic!("Didn't find {} in the vars", ident);
  }
}

pub fn execute(statement: &Statement, vars: &mut Vars, shader: &Shader) -> Option<Value> {
  if DEBUG { println!("s: {:?}", statement); }

  match statement {
    &Statement::Compound(ref statements) => {
      for statement in statements {
        let val = execute(statement, vars, shader);
        if val.is_some() { return val; }
      }
    },
    &Statement::Expression(ref expression) => {
      eval(expression, vars, shader);
    },
    &Statement::Builtin(ref func) => {
      return Some(func.0(vars))
    },
    &Statement::For(ref init, ref condition, ref iter, ref body) => {
      vars.push();
      execute(init, vars, shader);

      loop {
        let cond = if let Value::Bool(cond) = eval(condition, vars, shader) { cond } else { unreachable!() };

        if cond == 0 { break };

        let res = execute(body, vars, shader);
        if res.is_some() { return res; }

        eval(iter, vars, shader);
      }
    },
    &Statement::Declaration(ref typ, ref name, ref init) => {
      let value = if let &Some(ref init) = init {
        eval(init, vars, shader)
      } else {
        match &((typ.1).0) {
          &TypeSpecifierNonArray::Float => Value::Float(0.0),
          &TypeSpecifierNonArray::Vec2 => Value::Vec2([0.0, 0.0]),
          &TypeSpecifierNonArray::Vec3 => Value::Vec3([0.0, 0.0, 0.0]),
          &TypeSpecifierNonArray::Vec4 => Value::Vec4([0.0, 0.0, 0.0, 0.0]),
          &TypeSpecifierNonArray::Int => Value::Int(0),
          &TypeSpecifierNonArray::IVec2 => Value::IVec2([0, 0]),
          &TypeSpecifierNonArray::IVec3 => Value::IVec3([0, 0, 0]),
          &TypeSpecifierNonArray::IVec4 => Value::IVec4([0, 0, 0, 0]),
          &TypeSpecifierNonArray::Uint => Value::Uint(0),
          &TypeSpecifierNonArray::UVec2 => Value::UVec2([0, 0]),
          &TypeSpecifierNonArray::UVec3 => Value::UVec3([0, 0, 0]),
          &TypeSpecifierNonArray::UVec4 => Value::UVec4([0, 0, 0, 0]),
          &TypeSpecifierNonArray::Bool => Value::Bool(0),
          &TypeSpecifierNonArray::BVec2 => Value::BVec2([0, 0]),
          &TypeSpecifierNonArray::BVec3 => Value::BVec3([0, 0, 0]),
          &TypeSpecifierNonArray::BVec4 => Value::BVec4([0, 0, 0, 0]),
          x => unimplemented!("{:?}", x),
        }
      };

      vars.insert(name.clone(), value);
    },
    &Statement::If(ref cond, ref body, ref else_body) => {
      let cond_res = eval(cond, vars, shader).get();
      let cond_res = if let Value::Bool(cond_res) = cond_res { cond_res } else { unreachable!() };

      if cond_res > 0 {
        return execute(body, vars, shader);
      } else if let &Some(ref else_body) = else_body.deref() {
        return execute(else_body, vars, shader);
      }
    }
  }
  None
}

fn eval(expression: &Expression, vars: &mut Vars, shader: &Shader) -> Value {
  if DEBUG { println!("e: {:?}", expression); }

  match *expression {
    Expression::Assignment(ref lexpr, ref rexpr) => {
      let value = eval(rexpr, vars, shader).get();
      let mut slot = eval(lexpr, vars, shader);
      slot.set(value.clone());
      value
    },
    Expression::AddAssign(ref lexpr, ref rexpr) => {
      let rhs = eval(rexpr, vars, shader).get();
      let mut slot = eval(lexpr, vars, shader);

      let orig_value = slot.get();

      let new_value = match (orig_value, rhs) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a + b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
        (Value::UVec3(a), Value::UVec3(b)) => Value::UVec3([a[0] + b[0],
                                                            a[1] + b[1],
                                                            a[2] + b[2]]),
        (Value::Vec2(a), Value::Vec2(b)) => Value::Vec2([a[0] + b[0],
                                                         a[1] + b[1]]),
        (Value::Vec4(a), Value::Vec4(b)) => Value::Vec4([a[0] + b[0],
                                                         a[1] + b[1],
                                                         a[2] + b[2],
                                                         a[3] + b[3]]),
        x => unimplemented!("{:?}", x),
      };

      slot.set(new_value.clone());
      new_value
    },
    Expression::FunctionCall(ref name, ref args) => {
      let args = args.iter().map(|a| eval(a, vars, shader)).collect::<Vec<_>>();

      if name == "uint"
        || name == "uvec2"
        || name == "uvec3"
        || name == "uvec4"
        || name == "int"
        || name == "ivec2"
        || name == "ivec3"
        || name == "ivec4"
        || name == "float"
        || name == "vec2"
        || name == "vec3"
        || name == "vec4"
      {
        return convert(name, &args)
      }

      let fs = shader.functions.get(name).expect(&format!("Didn't find function '{}'", name));
      let &(ref func, ref body) = fs.iter().find(|&&(ref f,_)| {
        args.len() == f.params.len() && args.iter().zip(f.params.iter()).all(|(a,p)| {
          match (a.get(), &((p.0).1).0) {
            (Value::Uint(_), &TypeSpecifierNonArray::Uint) => true,
            (Value::UVec2(_), &TypeSpecifierNonArray::UVec2) => true,
            (Value::UVec3(_), &TypeSpecifierNonArray::UVec3) => true,
            (Value::UVec4(_), &TypeSpecifierNonArray::UVec4) => true,
            (Value::Int(_), &TypeSpecifierNonArray::Int) => true,
            (Value::IVec2(_), &TypeSpecifierNonArray::IVec2) => true,
            (Value::IVec3(_), &TypeSpecifierNonArray::IVec3) => true,
            (Value::IVec4(_), &TypeSpecifierNonArray::IVec4) => true,
            (Value::Float(_), &TypeSpecifierNonArray::Float) => true,
            (Value::Vec2(_), &TypeSpecifierNonArray::Vec2) => true,
            (Value::Vec3(_), &TypeSpecifierNonArray::Vec3) => true,
            (Value::Vec4(_), &TypeSpecifierNonArray::Vec4) => true,
            (Value::UImage2D(_), &TypeSpecifierNonArray::UImage2D) => true,
            (Value::Buffer(ref t, _, _), t2) if t == t2 => true,
            _ => false,
          }
        })
      }).expect(&format!("Didn't find matching function signature for {}", name));

      vars.push();
      for (arg, param) in args.into_iter().zip(func.params.iter()) {
        let arg = if (param.0).0.iter().any(|q| {
          q == &TypeQualifier::Storage(StorageQualifier::Out)
            || q == &TypeQualifier::Storage(StorageQualifier::Inout)
        }) {
          arg
        } else {
          arg.get()
        };

        vars.insert(param.1.as_ref().unwrap().0.clone(), arg);
      }

      let res = execute(&body, vars, shader);
      let res = if (func.typ.1).0 == TypeSpecifierNonArray::Void {
        Value::Void
      } else {
        res.unwrap()
      };

      vars.pop();

      res
    },
    Expression::Variable(ref name) => { vars.get_mut(name).get_ref() },
    Expression::FloatConstant(f) => { Value::Float(f) },
    Expression::IntConstant(i) => { Value::Int(i) },
    Expression::UintConstant(u) => { Value::Uint(u) },
    Expression::Comparison(ref left, ref op, ref right) => {
      let left = eval(left, vars, shader).get();
      let right = eval(right, vars, shader).get();

      let result = match *op {
        Comparison::Less => left < right,
        Comparison::Greater => left > right,
        Comparison::Equal => left == right,
        Comparison::NotEqual => left != right,
        Comparison::LessEqual => left <= right,
        Comparison::GreaterEqual => left >= right,
      };
      Value::Bool(result as u32)
    },
    // TODO: omg unify these maths
    Expression::Add(ref left, ref right) => {
      let left = eval(left, vars, shader).get();
      let right = eval(right, vars, shader).get();

      match (left, right) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a + b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
        (Value::UVec3(a), Value::UVec3(b)) => Value::UVec3([a[0] + b[0],
                                                            a[1] + b[1],
                                                            a[2] + b[2]]),
        (Value::Vec4(a), Value::Vec4(b)) => Value::Vec4([a[0] + b[0],
                                                         a[1] + b[1],
                                                         a[2] + b[2],
                                                         a[3] + b[3]]),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::Sub(ref left, ref right) => {
      let left = eval(left, vars, shader).get();
      let right = eval(right, vars, shader).get();

      match (left, right) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a - b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
        (Value::UVec3(a), Value::UVec3(b)) => Value::UVec3([a[0] - b[0],
                                                            a[1] - b[1],
                                                            a[2] - b[2]]),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::Multiply(ref left, ref right) => {
      let left = eval(left, vars, shader).get();
      let right = eval(right, vars, shader).get();

      match (left, right) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a * b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
        (Value::UVec3(a), Value::UVec3(b)) => Value::UVec3([a[0] * b[0],
                                                            a[1] * b[1],
                                                            a[2] * b[2]]),
        (Value::Vec3(a), Value::Float(b)) => Value::Vec3([a[0] * b,
                                                          a[1] * b,
                                                          a[2] * b]),
        (Value::Float(a), Value::Vec3(b)) => Value::Vec3([a * b[0],
                                                          a * b[1],
                                                          a * b[2]]),
        (Value::Float(a), Value::Vec2(b)) => Value::Vec2([a * b[0],
                                                          a * b[1]]),
        (Value::Vec4(a), Value::Float(b)) => Value::Vec4([a[0] * b,
                                                          a[1] * b,
                                                          a[2] * b,
                                                          a[3] * b]),
        (Value::Vec2(a), Value::Vec2(b)) => Value::Vec2([a[0] * b[0],
                                                         a[1] * b[1]]),
        (Value::Vec3(a), Value::Vec3(b)) => Value::Vec3([a[0] * b[0],
                                                         a[1] * b[1],
                                                         a[2] * b[2]]),
        (Value::Vec4(a), Value::Vec4(b)) => Value::Vec4([a[0] * b[0],
                                                         a[1] * b[1],
                                                         a[2] * b[2],
                                                         a[3] * b[3]]),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::Divide(ref left, ref right) => {
      let left = eval(left, vars, shader).get();
      let right = eval(right, vars, shader).get();

      match (left, right) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a / b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::Modulo(ref left, ref right) => {
      let left = eval(left, vars, shader).get();
      let right = eval(right, vars, shader).get();

      match (left, right) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a % b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::Index(ref container, ref index) => {
      let mut container = eval(container, vars, shader);
      let mut container = &mut container;

      while let &mut Value::Ref(p) = container {
        container = unsafe{ &mut *p };
      };


      let index = eval(index, vars, shader).get();
      let index = match index {
        Value::Int(i) => i as isize,
        Value::Uint(u) => u as isize,
        x => unreachable!("{:?}", x),
      };

      match container {
        &mut Value::Buffer(ref s, p, _len) => {
          let stride = super::stride_of(s, Some(&shader.types));

          Value::Buffer(s.clone(),
                        unsafe{ p.offset(index * stride as isize) },
                        None)
        },
        &mut Value::Mat2(ref mut v) => Value::RefV2(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat2x3(ref mut v) => Value::RefV3(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat2x4(ref mut v) => Value::RefV4(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat3x2(ref mut v) => Value::RefV2(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat3(ref mut v) => Value::RefV3(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat3x4(ref mut v) => Value::RefV4(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat4x2(ref mut v) => Value::RefV2(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat4x3(ref mut v) => Value::RefV3(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Mat4(ref mut v) => Value::RefV4(unsafe{ v.as_mut_ptr().offset(index) }),
        &mut Value::Array(ref mut a) => a[index as usize].get_ref(),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::FieldSelection(ref container, ref field) => {
      let mut container = eval(container, vars, shader);
      let mut container = &mut container;

      while let &mut Value::Ref(p) = container {
        container = unsafe{ &mut *p };
      };

      match container {
        &mut Value::Buffer(ref typ, ref p, ref len) => {
          if field == "length" {
            return Value::Uint(len.unwrap())
          }

          let typ = if let &TypeSpecifierNonArray::Custom(ref t) = typ { t } else { unreachable!() };

          let field = if let Some(custom_type) = shader.types.get(typ) {
            custom_type.fields.iter().find(|v| &v.name == field).unwrap()
          } else {
            // TODO: maybe should use the types table for these as well?
            let info = shader.interfaces.iter().filter_map(|iface| match iface {
              &super::Interface::ShaderStorageBlock(ref info)
                | &super::Interface::UniformBlock(ref info)
                => if &info.name == typ { Some(info) } else { None },
              &super::Interface::Uniform(_) => None,
              x => unimplemented!("{:?}", x),
            }).next().unwrap();

            info.active_variables.iter().find(|v| &v.name == field).expect("2")
          };

          let length = Some(if super::array_size(&field.array) > 0 { super::array_size(&field.array) } else { len.unwrap() });

          Value::Buffer(field.type_.clone(), unsafe{ p.offset(field.offset as isize) }, length)
        },
        // TODO: unhardcoded version of these
        &mut Value::UVec3(ref mut v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
            "y" => Value::Uint(v[1]),
            "z" => Value::Uint(v[2]),
            "xy" => Value::UVec2([v[0], v[1]]),
            x => unimplemented!("{}", x),
          }
        },
        &mut Value::UVec2(ref mut v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
            "y" => Value::Uint(v[1]),
            "xy" => Value::UVec2([v[0], v[1]]),
            x => unimplemented!("{}", x),
          }
        },
        &mut Value::UVec4(ref mut v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
            x => unimplemented!("{}", x),
          }
        },
        &mut Value::Vec2(ref mut v) => {
          match field.as_ref() {
            "x" => Value::Float(v[0]),
            "y" => Value::Float(v[1]),
            "xy" => Value::Vec2([v[0], v[1]]),
            "xxyy" => Value::Vec4([v[0], v[0], v[1], v[1]]),
            x => unimplemented!("{}", x),
          }
        },
        &mut Value::Vec3(ref mut v) => unsafe {
          match field.as_ref() {
            "xy" => Value::RefV22(v.as_mut_ptr().offset(0),
                                  v.as_mut_ptr().offset(1)),
            "rg" => Value::RefV22(v.as_mut_ptr().offset(0),
                                  v.as_mut_ptr().offset(1)),
            "rgb" => Value::RefV32(v.as_mut_ptr().offset(0),
                                   v.as_mut_ptr().offset(1),
                                   v.as_mut_ptr().offset(2)),
            "xyz" => Value::RefV32(v.as_mut_ptr().offset(0),
                                   v.as_mut_ptr().offset(1),
                                   v.as_mut_ptr().offset(2)),
            "xyzx" => Value::RefV42(v.as_mut_ptr().offset(0),
                                    v.as_mut_ptr().offset(1),
                                    v.as_mut_ptr().offset(2),
                                    v.as_mut_ptr().offset(0)),
            x => unimplemented!("{}", x),
          }
        },
        &mut Value::IVec3(ref mut v) => unsafe {
          match field.as_ref() {
            "xy" => Value::RefIV22(v.as_mut_ptr().offset(0),
                                   v.as_mut_ptr().offset(1)),
            "rg" => Value::RefIV22(v.as_mut_ptr().offset(0),
                                   v.as_mut_ptr().offset(1)),
            "rgb" => Value::RefIV32(v.as_mut_ptr().offset(0),
                                    v.as_mut_ptr().offset(1),
                                    v.as_mut_ptr().offset(2)),
            "xyz" => Value::RefIV32(v.as_mut_ptr().offset(0),
                                    v.as_mut_ptr().offset(1),
                                    v.as_mut_ptr().offset(2)),
            x => unimplemented!("{}", x),
          }
        },
        &mut Value::Vec4(ref mut v) => unsafe {
          match field.as_ref() {
            "xy" => Value::RefV22(v.as_mut_ptr().offset(0),
                                  v.as_mut_ptr().offset(1)),
            "zw" => Value::RefV22(v.as_mut_ptr().offset(2),
                                  v.as_mut_ptr().offset(3)),
            "rg" => Value::RefV22(v.as_mut_ptr().offset(0),
                                  v.as_mut_ptr().offset(1)),
            "rgb" => Value::RefV32(v.as_mut_ptr().offset(0),
                                   v.as_mut_ptr().offset(1),
                                   v.as_mut_ptr().offset(2)),
            "xyz" => Value::RefV32(v.as_mut_ptr().offset(0),
                                   v.as_mut_ptr().offset(1),
                                   v.as_mut_ptr().offset(2)),
            x => unimplemented!("{}", x),
          }
        },
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::PostInc(ref expr) => {
      let mut lval = eval(expr, vars, shader);
      let result = lval.get();

      let incred = match result {
        Value::Uint(u) => Value::Uint(u + 1),
        Value::Int(u) => Value::Int(u + 1),
        x => unimplemented!("{:?}", x),
      };
      lval.set(incred);

      result
    },
    Expression::BinaryNot(ref expr) => {
      let v = eval(expr, vars, shader).get();
      match v {
        Value::Uint(u) => Value::Uint(!u),
        x => unimplemented!("{:?}", x),
      }
    },
    ref x => unimplemented!("{:?}", x),
  }
}

fn convert(name: &str, args: &[Value]) -> Value {
  let (unit, num) = match name {
    "uint" => ("uint", 1),
    "uvec2" => ("uint", 2),
    "uvec3" => ("uint", 3),
    "uvec4" => ("uint", 4),
    "int" => ("int", 1),
    "ivec2" => ("int", 2),
    "ivec3" => ("int", 3),
    "ivec4" => ("int", 4),
    "float" => ("float", 1),
    "vec2" => ("float", 2),
    "vec3" => ("float", 3),
    "vec4" => ("float", 4),
    x => unimplemented!("{}", x),
  };

  match unit {
    "uint" => {
      let mut vals = args.iter().map(Value::get).flat_map(|v| into_uint(&v));

      let a = vals.next().unwrap_or(0);
      let b = vals.next().unwrap_or(0);
      let c = vals.next().unwrap_or(0);
      let d = vals.next().unwrap_or(1);

      match num {
        1 => Value::Uint(a),
        2 => Value::UVec2([a, b]),
        3 => Value::UVec3([a, b, c]),
        4 => Value::UVec4([a, b, c, d]),
        x => unimplemented!("{}", x),
      }
    },
    "int" => {
      let mut vals = args.iter().map(Value::get).flat_map(|v| into_int(&v));

      let a = vals.next().unwrap_or(0);
      let b = vals.next().unwrap_or(0);
      let c = vals.next().unwrap_or(0);
      let d = vals.next().unwrap_or(1);

      match num {
        1 => Value::Int(a),
        2 => Value::IVec2([a, b]),
        3 => Value::IVec3([a, b, c]),
        4 => Value::IVec4([a, b, c, d]),
        x => unimplemented!("{}", x),
      }
    },
    "float" => {
      let mut vals = args.iter().map(Value::get).flat_map(|v| into_float(&v));

      let a = vals.next().unwrap_or(0.0);
      let b = vals.next().unwrap_or(0.0);
      let c = vals.next().unwrap_or(0.0);
      let d = vals.next().unwrap_or(1.0);

      match num {
        1 => Value::Float(a),
        2 => Value::Vec2([a, b]),
        3 => Value::Vec3([a, b, c]),
        4 => Value::Vec4([a, b, c, d]),
        x => unimplemented!("{}", x),
      }
    },
    x => unimplemented!("{}", x),
  }
}

fn into_uint(val: &Value) -> Vec<u32> {
  match val {
    &Value::Uint(u) => vec![u as u32],
    &Value::IVec2(ref v) => vec![v[0] as u32, v[1] as u32],
    &Value::Float(a) => vec![a as u32],
    &Value::Int(a) => vec![a as u32],
    x => unimplemented!("{:?}", x),
  }
}

fn into_int(val: &Value) -> Vec<i32> {
  match val {
    &Value::Uint(u) => vec![u as i32],
    &Value::IVec2(ref v) => vec![v[0] as i32, v[1] as i32],
    &Value::UVec2(ref v) => vec![v[0] as i32, v[1] as i32],
    &Value::Float(a) => vec![a as i32],
    &Value::Int(a) => vec![a as i32],
    x => unimplemented!("{:?}", x),
  }
}

fn into_float(val: &Value) -> Vec<f32> {
  match val {
    &Value::Uint(u) => vec![u as f32],
    &Value::UVec2(ref v) => vec![v[0] as f32, v[1] as f32],
    &Value::UVec3(ref v) => vec![v[0] as f32, v[1] as f32, v[2] as f32],
    &Value::UVec4(ref v) => vec![v[0] as f32, v[1] as f32, v[2] as f32, v[3] as f32],
    &Value::Int(a) => vec![a as f32],
    &Value::IVec2(ref v) => vec![v[0] as f32, v[1] as f32],
    &Value::IVec3(ref v) => vec![v[0] as f32, v[1] as f32, v[2] as f32],
    &Value::IVec4(ref v) => vec![v[0] as f32, v[1] as f32, v[2] as f32, v[3] as f32],
    &Value::Float(a) => vec![a],
    &Value::Vec2(ref v) => vec![v[0], v[1]],
    &Value::Vec3(ref v) => vec![v[0], v[1], v[2]],
    &Value::Vec4(ref v) => vec![v[0], v[1], v[2], v[3]],
    x => unimplemented!("{:?}", x),
  }
}

#[test]
fn test_basic() {
  let source = r"
    in float c;
    out vec4 color;

    void main() {
      color = vec4(c, c, c, 1.0);
    }
  ";


  let shader = super::compile(source.as_bytes(), ::consts::GL_FRAGMENT_SHADER).unwrap();

  let input = 0.3;
  let output = [0.0; 4];

  let mut vars = Vars::new();
  {
    vars.insert("c".into(), Value::Float(input));
    vars.insert("color".into(), Value::Vec4(output));

    let main = &shader.functions[&"main".into()][0];

    execute(&main.1, &mut vars, &shader);
  }

  assert_eq!(vars.get(&"c".into()), &Value::Float(0.3));
  assert_eq!(vars.get(&"color".into()), &Value::Vec4([0.3, 0.3, 0.3, 1.0]));
}

#[test]
fn test_buffer() {
    let source = r"#version 310 es
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;
layout(binding = 0) buffer Input {
  uint values[5];
} sb_in;
layout (binding = 1) buffer Output {
  uint values[5];
} sb_out;
void main (void) {
  for (uint i = 0; i < 5; i++) {
    sb_out.values[i] = sb_in.values[i] * 2u;
  }
}";

  let mut input: Vec<u32> = vec![1, 2, 3, 4, 5];
  let mut output: Vec<u32> = vec![0, 0, 0, 0, 0];

  let shader = super::compile(source.as_bytes(), ::consts::GL_COMPUTE_SHADER).unwrap();

  let mut vars = Vars::new();
  {
    vars.insert("sb_in".into(), Value::Buffer(TypeSpecifierNonArray::Custom("Input".into()), input.as_mut_ptr() as *mut _, None));
    vars.insert("sb_out".into(), Value::Buffer(TypeSpecifierNonArray::Custom("Output".into()), output.as_mut_ptr() as *mut _, None));

    let main = &shader.functions[&"main".into()][0];

    execute(&main.1, &mut vars, &shader);
  }

  assert_eq!(input, vec![1, 2, 3, 4, 5]);
  assert_eq!(output, vec![2, 4, 6, 8, 10]);
}

#[test]
fn test_out() {
  let source = r"
    out vec4 color;

    void increase(inout float f, float amount) {
      f += amount;
    }

    void main() {
      float c = 0.2;
      increase(c, 0.1);
      color = vec4(c, c, c, 1.0);
    }
  ";


  let shader = super::compile(source.as_bytes(), ::consts::GL_FRAGMENT_SHADER).unwrap();

  let output = [0.0; 4];

  let mut vars = Vars::new();
  {
    vars.insert("color".into(), Value::Vec4(output));

    let main = &shader.functions[&"main".into()][0];

    execute(&main.1, &mut vars, &shader);
  }

  assert_eq!(vars.get(&"color".into()), &Value::Vec4([0.3, 0.3, 0.3, 1.0]));
}
