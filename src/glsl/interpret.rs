use super::parse::{Statement,Expression,FunctionPrototype,TypeSpecifierNonArray,Comparison,TypeQualifier,StorageQualifier};
use super::Shader;

use std::collections::HashMap;
use std::mem;
use std::sync::{Barrier,Arc};

const DEBUG: bool = false;

#[derive(Debug,Clone)]
pub enum Value {
  Int(i32),
  Uint(u32),
  Vec4([f32; 4]),
  UVec3([u32; 3]),
  Float(f32),
  Bool(bool),
  Void,

  Buffer(String, *mut u8, Option<u32>),
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
        match (typ.as_str(), val) {
          ("uint", Value::Uint(u)) => unsafe{ *(ptr as *mut u32) = u },
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
      &Value::Buffer(ref typ, ptr, _) => {
        match typ.as_str() {
          "uint" => Value::Uint(unsafe{ *(ptr as *const u32) }),
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
  Vec4Float4,
  UintUint,
  MemoryBarrierBuffer,
  Barrier,
  AtomicAddUint,
}

impl BuiltinFunc {
  fn eval(&self, vars: &mut Vars) -> Value {
    use self::Value::*;

    match *self {
      BuiltinFunc::Vec4Float4 => {
        if let (&Float(a), &Float(b), &Float(c), &Float(d)) = (
          vars.get(&"a".to_string()),
          vars.get(&"b".to_string()),
          vars.get(&"c".to_string()),
          vars.get(&"d".to_string()),
        ) {
          Vec4([a, b, c, d])
        } else { unreachable!() }
      },
      BuiltinFunc::UintUint => {
        vars.get(&"a".to_string()).clone()
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
          (&Value::Buffer(ref typ, p, _), &Value::Uint(v)) if typ == "uint" => {
            let a = unsafe{ &*(p as *const AtomicU32) };

            let old = a.fetch_add(v, Ordering::AcqRel);
            Value::Uint(old + v)
          },
          _ => unreachable!(),
        }
      },
    }
  }

  pub fn all() -> HashMap<String, Vec<(FunctionPrototype, Statement)>> {
    let mut funcs = HashMap::new();

    let vec4 = (vec![], (TypeSpecifierNonArray::Vec4, vec![]));
    let uint = (vec![], (TypeSpecifierNonArray::Uint, vec![]));
    let uint_inout = (vec![TypeQualifier::Storage(StorageQualifier::Inout)], (TypeSpecifierNonArray::Uint, vec![]));
    let float_ = (vec![], (TypeSpecifierNonArray::Float, vec![]));
    let void = (vec![], (TypeSpecifierNonArray::Void, vec![]));

    let vec4_float4 = FunctionPrototype{
      typ: vec4,
      name: "vec4".to_string(),
      params: vec![
        (float_.clone(), Some(("a".to_string(), vec![]))),
        (float_.clone(), Some(("b".to_string(), vec![]))),
        (float_.clone(), Some(("c".to_string(), vec![]))),
        (float_.clone(), Some(("d".to_string(), vec![]))),
      ],
    };

    let uint_uint = FunctionPrototype{
      typ: uint.clone(),
      name: "uint".to_string(),
      params: vec![
        (uint.clone(), Some(("a".to_string(), vec![]))),
      ],
    };

    let memory_barrier_buffer = FunctionPrototype{
      typ: void.clone(),
      name: "memoryBarrierBuffer".to_string(),
      params: vec![],
    };

    let barrier = FunctionPrototype{
      typ: void.clone(),
      name: "barrier".to_string(),
      params: vec![],
    };


    let atomic_add_uint = FunctionPrototype{
      typ: uint.clone(),
      name: "atomicAdd".to_string(),
      params: vec![
        (uint_inout.clone(), Some(("mem".to_string(), vec![]))),
        (uint.clone(), Some(("data".to_string(), vec![]))),
      ],
    };

    funcs.insert("vec4".to_string(), vec![(vec4_float4, Statement::Builtin(BuiltinFunc::Vec4Float4))]);
    funcs.insert("uint".to_string(), vec![(uint_uint, Statement::Builtin(BuiltinFunc::UintUint))]);
    funcs.insert("memoryBarrierBuffer".to_string(), vec![(memory_barrier_buffer, Statement::Builtin(BuiltinFunc::MemoryBarrierBuffer))]);
    funcs.insert("barrier".to_string(), vec![(barrier, Statement::Builtin(BuiltinFunc::Barrier))]);
    funcs.insert("atomicAdd".to_string(), vec![(atomic_add_uint, Statement::Builtin(BuiltinFunc::AtomicAddUint))]);

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
    &Statement::Declaration(_, ref name, ref init) => {
      let value = eval(init, vars, shader);

      vars.insert(name.clone(), value);
    },
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
      let fs = shader.functions.get(name).expect(&format!("Didn't find function '{}'", name));
      let &(ref func, ref body) = fs.iter().find(|&&(ref f,_)| {
        args.len() == f.params.len() && args.iter().zip(f.params.iter()).all(|(a,p)| {
          match (a.get(), &((p.0).1).0) {
            (Value::Float(_), &TypeSpecifierNonArray::Float) => true,
            (Value::Uint(_), &TypeSpecifierNonArray::Uint) => true,
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
        Value::Buffer(ref s, p, _len) if s == "uint" => {
          Value::Buffer("uint".to_string(), unsafe{ p.offset((index as usize * mem::size_of::<u32>() / mem::size_of::<u8>()) as isize) }, None)
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
              => if info.name == typ { Some(info) } else { None },
            x => unimplemented!("{:?}", x),
          }).next().unwrap();

          let field = info.active_variables.iter().find(|v| &v.name == field).expect("2");
          let new_type = match field.type_ {
            ::gl::GL_UNSIGNED_INT => "uint".to_string(),
            x => unimplemented!("{:x}", x),
          };

          let length = Some(if field.array_size > 0 { field.array_size } else { len.unwrap() });

          Value::Buffer(new_type, unsafe{ p.offset(field.offset as isize) }, length)
        },
        Value::UVec3(v) => {
          match field.as_ref() {
            "x" => Value::Uint(v[0]),
            "y" => Value::Uint(v[1]),
            "z" => Value::Uint(v[2]),
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
