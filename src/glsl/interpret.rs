use super::parse::{Statement,Expression,FunctionPrototype,TypeSpecifierNonArray,Comparison,TypeQualifier,StorageQualifier};
use super::Shader;

use std::collections::HashMap;
use std::sync::{Barrier,Arc};
use std::ops::Deref;

use gl;

const DEBUG: bool = false;

#[derive(Debug,Clone)]
pub enum Value {
  Int(i32),
  Uint(u32),
  Vec4([f32; 4]),
  UVec4([u32; 4]),
  UVec3([u32; 3]),
  UVec2([u32; 2]),
  IVec2([i32; 2]),
  Float(f32),
  Bool(bool),
  Void,

  UImage2DUnit(usize),
  UImage2D(Arc<gl::Texture>),

  Buffer(TypeSpecifierNonArray, *mut u8, Option<u32>),
  Barrier(Arc<Barrier>),

  Ref(*mut Value),
}

impl Value {
  fn set(&mut self, val: Value) {
    match self {
      &mut Value::Ref(inner) => {
        let inner = unsafe{ &mut *inner };
        inner.set(val);
      },
      &mut Value::Buffer(ref typ, ptr, _) => {
        match (typ, val) {
          (&TypeSpecifierNonArray::Uint, Value::Uint(u)) => unsafe{ *(ptr as *mut u32) = u },
          (&TypeSpecifierNonArray::UVec3, Value::UVec3(u)) => unsafe{ *(ptr as *mut [u32; 3]) = u },
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
      &Value::Buffer(ref typ, ptr, size) => {
        match typ {
          &TypeSpecifierNonArray::Uint => Value::Uint(unsafe{ *(ptr as *const u32) }),
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

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum BuiltinFunc {
  MemoryBarrierBuffer,
  Barrier,
  AtomicAddUint,
  ImageLoadU2D,
  ImageStoreU2D,
  ImageAtomicAddU2D,
  AtomicCounterIncrement,
}

impl BuiltinFunc {
  fn eval(&self, vars: &mut Vars) -> Value {
    use self::Value::*;

    match *self {
      BuiltinFunc::ImageLoadU2D => {
        if let (&UImage2D(ref i),
                &IVec2(ref t))
          = (vars.get(&"a".to_string()),
             vars.get(&"b".to_string())) {

            let (x, y) = (t[0] as usize, t[1] as usize);
            let texel = y * i.width + x;

            let p = i.buffer.as_ptr() as *const u32;
            let r = unsafe{ *p.offset(texel as isize) };

            UVec4([r, 0, 0, 1])
          } else { unreachable!() }
      },
      BuiltinFunc::ImageStoreU2D => {
        if let (&UImage2D(ref i),
                &IVec2(ref t),
                &UVec4(ref v))
          = (vars.get(&"a".to_string()),
             vars.get(&"b".to_string()),
             vars.get(&"c".to_string())) {

            let (x, y) = (t[0] as usize, t[1] as usize);
            let texel = y * i.width + x;

            let p = i.buffer.as_ptr() as *mut u32;

            unsafe{ *p.offset(texel as isize) = v[0] };
            Void
          } else { unreachable!() }
      },
      BuiltinFunc::ImageAtomicAddU2D => {
        use std::sync::atomic::{AtomicU32,Ordering};

        if let (&UImage2D(ref i),
                &IVec2(ref t),
                &Uint(d))
          = (vars.get(&"a".to_string()),
             vars.get(&"b".to_string()),
             vars.get(&"c".to_string())) {

            let (x, y) = (t[0] as usize, t[1] as usize);
            let texel = y * i.width + x;

            let p = i.buffer.as_ptr() as *const AtomicU32;
            let p = unsafe{ &*p.offset(texel as isize) };

            let old = p.fetch_add(d, Ordering::AcqRel);
            Value::Uint(old)
          } else { unreachable!() }
      },
      BuiltinFunc::MemoryBarrierBuffer => { Value::Void },
      BuiltinFunc::Barrier => {
        if let &Value::Barrier(ref barrier) = vars.get(&"__barrier".to_string()) {
          barrier.wait();
        } else { unreachable!() }

        Value::Void
      },
      BuiltinFunc::AtomicAddUint => {
        use std::sync::atomic::{AtomicU32,Ordering};

        match (vars.get(&"mem".to_string()), vars.get(&"data".to_string())) {
          (&Value::Buffer(TypeSpecifierNonArray::Uint, p, _), &Value::Uint(v)) => {
            let a = unsafe{ &*(p as *const AtomicU32) };

            let old = a.fetch_add(v, Ordering::AcqRel);
            Value::Uint(old)
          },
          _ => unreachable!(),
        }
      },
      BuiltinFunc::AtomicCounterIncrement => {
        use std::sync::atomic::{AtomicU32,Ordering};

        if let &Value::Buffer(ref _typ, p, _) = vars.get(&"mem".to_string()) {
          let a = unsafe{ &*(p as *const AtomicU32) };

          let old = a.fetch_add(1, Ordering::AcqRel);
          Value::Uint(old)
        } else { unreachable!() }
      },
    }
  }

  // Do a lazy static thing
  pub fn all() -> HashMap<String, Vec<(FunctionPrototype, Statement)>> {
    let mut funcs = HashMap::new();

    let uvec4 = (vec![], (TypeSpecifierNonArray::UVec4, vec![]));
    let ivec2 = (vec![], (TypeSpecifierNonArray::IVec2, vec![]));
    let uint = (vec![], (TypeSpecifierNonArray::Uint, vec![]));
    let atomic_uint = (vec![], (TypeSpecifierNonArray::AtomicUint, vec![]));
    let uint_inout = (vec![TypeQualifier::Storage(StorageQualifier::Inout)], (TypeSpecifierNonArray::Uint, vec![]));
    let uimage2d = (vec![], (TypeSpecifierNonArray::UImage2D, vec![]));
    let void = (vec![], (TypeSpecifierNonArray::Void, vec![]));

    let memory_barrier_buffer = FunctionPrototype{
      typ: void.clone(),
      name: "memoryBarrierBuffer".to_string(),
      params: vec![],
    };
    funcs.insert("memoryBarrierBuffer".to_string(), vec![(memory_barrier_buffer, Statement::Builtin(BuiltinFunc::MemoryBarrierBuffer))]);

    let barrier = FunctionPrototype{
      typ: void.clone(),
      name: "barrier".to_string(),
      params: vec![],
    };
    funcs.insert("barrier".to_string(), vec![(barrier, Statement::Builtin(BuiltinFunc::Barrier))]);


    let imageload_u2d = FunctionPrototype{
      typ: uvec4.clone(),
      name: "imageLoad".to_string(),
      params: vec![
        (uimage2d.clone(), Some(("a".to_string(), vec![]))),
        (ivec2.clone(), Some(("b".to_string(), vec![]))),
      ],
    };
    funcs.insert("imageLoad".to_string(), vec![(imageload_u2d, Statement::Builtin(BuiltinFunc::ImageLoadU2D))]);

    let imagestore_u2d = FunctionPrototype{
      typ: void.clone(),
      name: "imageStore".to_string(),
      params: vec![
        (uimage2d.clone(), Some(("a".to_string(), vec![]))),
        (ivec2.clone(), Some(("b".to_string(), vec![]))),
        (uvec4.clone(), Some(("c".to_string(), vec![]))),
      ],
    };
    funcs.insert("imageStore".to_string(), vec![(imagestore_u2d, Statement::Builtin(BuiltinFunc::ImageStoreU2D))]);

    let imageatomicadd_u2d = FunctionPrototype{
      typ: uint.clone(),
      name: "imageAtomicAdd".to_string(),
      params: vec![
        (uimage2d.clone(), Some(("a".to_string(), vec![]))),
        (ivec2.clone(), Some(("b".to_string(), vec![]))),
        (uint.clone(), Some(("c".to_string(), vec![]))),
      ],
    };
    funcs.insert("imageAtomicAdd".to_string(), vec![(imageatomicadd_u2d, Statement::Builtin(BuiltinFunc::ImageAtomicAddU2D))]);

    let atomic_add_uint = FunctionPrototype{
      typ: uint.clone(),
      name: "atomicAdd".to_string(),
      params: vec![
        (uint_inout.clone(), Some(("mem".to_string(), vec![]))),
        (uint.clone(), Some(("data".to_string(), vec![]))),
      ],
    };
    funcs.insert("atomicAdd".to_string(), vec![(atomic_add_uint, Statement::Builtin(BuiltinFunc::AtomicAddUint))]);

    let atomic_counter_increment = FunctionPrototype{
      typ: uint.clone(),
      name: "atomicCounterIncrement".to_string(),
      params: vec![
        (atomic_uint.clone(), Some(("mem".to_string(), vec![]))),
      ],
    };
    funcs.insert("atomicCounterIncrement".to_string(), vec![(atomic_counter_increment, Statement::Builtin(BuiltinFunc::AtomicCounterIncrement))]);

    funcs
  }
}

#[derive(Debug)]
pub struct Vars {
  scopes: Vec<HashMap<String, Value>>,
}

impl Vars {
  pub fn new() -> Self {
    Self{
      scopes: vec![HashMap::new()],
    }
  }

  pub fn push(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn pop(&mut self) {
    self.scopes.pop().expect("Tried to pop an empty vars");
  }

  pub fn insert(&mut self, ident: String, value: Value) {
    self.scopes.last_mut().unwrap().insert(ident, value);
  }

  fn get(&self, ident: &String) -> &Value {
    for scope in self.scopes.iter().rev() {
      if let Some(val) = scope.get(ident) {
        return val
      }
    }

    panic!("Didn't find {} in the vars", ident);
  }

  fn get_mut<'a>(&'a mut self, ident: &String) -> &mut Value {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(val) = scope.get_mut(ident) {
        return val;
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
      return Some(func.eval(vars))
    },
    &Statement::For(ref init, ref condition, ref iter, ref body) => {
      vars.push();
      execute(init, vars, shader);

      loop {
        let cond = if let Value::Bool(cond) = eval(condition, vars, shader) { cond } else { unreachable!() };

        if !cond { break };

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
          x => unimplemented!("{:?}", x),
        }
      };

      vars.insert(name.clone(), value);
    },
    &Statement::If(ref cond, ref body, ref else_body) => {
      let cond_res = eval(cond, vars, shader).get();
      let cond_res = if let Value::Bool(cond_res) = cond_res { cond_res } else { unreachable!() };

      if cond_res {
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
        x => unimplemented!("{:?}", x),
      };

      slot.set(new_value.clone());
      new_value
    },
    Expression::FunctionCall(ref name, ref args) => {
      let args = args.iter().map(|a| eval(a, vars, shader)).collect::<Vec<_>>();

      if name == "uint"
        || name == "int"
        || name == "float"
        || name == "vec4"
        || name == "ivec2"
        || name == "uvec4"
      {
        return convert(name, &args)
      }

      let fs = shader.functions.get(name).expect(&format!("Didn't find function '{}'", name));
      let &(ref func, ref body) = fs.iter().find(|&&(ref f,_)| {
        args.len() == f.params.len() && args.iter().zip(f.params.iter()).all(|(a,p)| {
          match (a.get(), &((p.0).1).0) {
            (Value::Float(_), &TypeSpecifierNonArray::Float) => true,
            (Value::Uint(_), &TypeSpecifierNonArray::Uint) => true,
            (Value::UVec2(_), &TypeSpecifierNonArray::UVec2) => true,
            (Value::UVec4(_), &TypeSpecifierNonArray::UVec4) => true,
            (Value::IVec2(_), &TypeSpecifierNonArray::IVec2) => true,
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
      Value::Bool(result)
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
      let container = eval(container, vars, shader);
      let index = eval(index, vars, shader).get();
      let index = match index {
        Value::Int(i) => i as isize,
        Value::Uint(u) => u as isize,
        x => unreachable!("{:?}", x),
      };

      match container {
        Value::Buffer(ref s, p, _len) => {
          let size = super::size_of(s, Some(&shader.types));

          Value::Buffer(s.clone(),
                        unsafe{ p.offset(index as isize * size as isize) },
                        None)
        },
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::FieldSelection(ref container, ref field) => {
      let mut container = eval(container, vars, shader);

      while let Value::Ref(p) = container {
        container = unsafe{ ::std::ptr::read(p) };
      };

      match container {
        Value::Buffer(typ, p, len) => {
          // TODO: maybe should use a separate types table instead of borrowing the interface info
          if field == "length" {
            return Value::Uint(len.unwrap())
          }

          let info = shader.interfaces.iter().filter_map(|iface| match iface {
            &super::Interface::ShaderStorageBlock(ref info)
              | &super::Interface::UniformBlock(ref info)
              => if TypeSpecifierNonArray::Custom(info.name.clone()) == typ { Some(info) } else { None },
            &super::Interface::Uniform(_) => None,
            x => unimplemented!("{:?}", x),
          }).next().unwrap();

          let field = info.active_variables.iter().find(|v| &v.name == field).expect("2");

          let length = Some(if field.array_size > 0 { field.array_size } else { len.unwrap() });

          Value::Buffer(field.type_.clone(), unsafe{ p.offset(field.offset as isize) }, length)
        },
        Value::UVec3(v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
            "y" => Value::Uint(v[1]),
            "z" => Value::Uint(v[2]),
            "xy" => Value::UVec2([v[0], v[1]]),
            x => unimplemented!("{}", x),
          }
        },
        Value::UVec2(v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
            "y" => Value::Uint(v[1]),
            "xy" => Value::UVec2([v[0], v[1]]),
            x => unimplemented!("{}", x),
          }
        },
        Value::UVec4(v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
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
      let _c = vals.next().unwrap_or(0);
      let _d = vals.next().unwrap_or(1);

      match num {
        1 => Value::Int(a),
        2 => Value::IVec2([a, b]),
        // 3 => Value::IVec3([a, b, c]),
        // 4 => Value::IVec4([a, b, c, d]),
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
        // 2 => Value::Vec2([a, b]),
        // 3 => Value::Vec3([a, b, c]),
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
    &Value::IVec2(ref v) => vec![v[0] as f32, v[1] as f32],
    &Value::Float(a) => vec![a as f32],
    &Value::Int(a) => vec![a as f32],
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


  let shader = super::compile(source.as_bytes(), ::gl::GL_FRAGMENT_SHADER).unwrap();

  let input = 0.3;
  let output = [0.0; 4];

  let mut vars = Vars::new();
  {
    vars.insert("c".to_string(), Value::Float(input));
    vars.insert("color".to_string(), Value::Vec4(output));

    let main = &shader.functions[&"main".to_string()][0];

    execute(&main.1, &mut vars, &shader);
  }

  assert_eq!(vars.get(&"c".to_string()), &Value::Float(0.3));
  assert_eq!(vars.get(&"color".to_string()), &Value::Vec4([0.3, 0.3, 0.3, 1.0]));
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

  let shader = super::compile(source.as_bytes(), ::gl::GL_COMPUTE_SHADER).unwrap();

  let mut vars = Vars::new();
  {
    vars.insert("sb_in".to_string(), Value::Buffer("Input".to_string(), input.as_mut_ptr() as *mut _, None));
    vars.insert("sb_out".to_string(), Value::Buffer("Output".to_string(), output.as_mut_ptr() as *mut _, None));

    let main = &shader.functions[&"main".to_string()][0];

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


  let shader = super::compile(source.as_bytes(), ::gl::GL_FRAGMENT_SHADER).unwrap();

  let output = [0.0; 4];

  let mut vars = Vars::new();
  {
    vars.insert("color".to_string(), Value::Vec4(output));

    let main = &shader.functions[&"main".to_string()][0];

    execute(&main.1, &mut vars, &shader);
  }

  assert_eq!(vars.get(&"color".to_string()), &Value::Vec4([0.3, 0.3, 0.3, 1.0]));
}
