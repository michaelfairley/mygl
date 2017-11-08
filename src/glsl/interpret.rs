use super::parse::{Statement,Expression,FunctionPrototype,TypeSpecifierNonArray,Comparison};
use super::Shader;

use std::collections::HashMap;
use std::mem;

#[derive(Debug,PartialEq,Clone,PartialOrd)]
pub enum Value {
  Int(i32),
  Uint(u32),
  Vec4([f32; 4]),
  UVec3([u32; 3]),
  Float(f32),
  Bool(bool),

  Buffer(String, *mut u8, Option<u32>),
}

#[derive(Debug)]
pub enum LValue<'a> {
  Value(&'a mut Value),
  Buffer(String, *mut u8),
  Item(String, *mut u8),
}

impl<'a> LValue<'a> {
  fn set(&mut self, val: Value) {
    match self {
      &mut LValue::Value(ref mut slot) => **slot = val,
      &mut LValue::Buffer(ref _typ, _ptr) => unimplemented!(),
      &mut LValue::Item(ref typ, ptr) => {
        match (typ.as_str(), val) {
          ("uint", Value::Uint(u)) => unsafe{ *(ptr as *mut u32) = u },
          x => unimplemented!("{:?}", x),
        }
      },
    }
  }
}

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum BuiltinFunc {
  Vec4Float4,
  UintUint,
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
      }
    }
  }

  pub fn all() -> HashMap<String, Vec<(FunctionPrototype, Statement)>> {
    let mut funcs = HashMap::new();

    let vec4 = (vec![], (TypeSpecifierNonArray::Vec4, vec![]));
    let uint = (vec![], (TypeSpecifierNonArray::Uint, vec![]));
    let float_ = (vec![], (TypeSpecifierNonArray::Float, vec![]));

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

    funcs.insert("vec4".to_string(), vec![(vec4_float4, Statement::Builtin(BuiltinFunc::Vec4Float4))]);
    funcs.insert("uint".to_string(), vec![(uint_uint, Statement::Builtin(BuiltinFunc::UintUint))]);

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

  fn get_lvalue<'a>(&'a mut self, ident: &String) -> LValue<'a> {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(val) = scope.get_mut(ident) {
        return LValue::Value(val)
      }
    }

    panic!("Didn't find {} in the vars", ident);
  }
}

pub fn execute(statement: &Statement, vars: &mut Vars, shader: &Shader) -> Option<Value> {
  match statement {
    &Statement::Compound(ref statements) => for statement in statements {
      let val = execute(statement, vars, shader);
      if val.is_some() { return val; }
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
  match *expression {
    Expression::Assignment(ref lexpr, ref rexpr) => {
      let value = eval(rexpr, vars, shader);
      let mut slot = eval_lvalue(lexpr, vars, shader);
      slot.set(value.clone());
      value
    },
    Expression::FunctionCall(ref name, ref args) => {
      let args = args.iter().map(|a| eval(a, vars, shader)).collect::<Vec<_>>();
      let fs = shader.functions.get(name).expect(&format!("Didn't find function '{}'", name));
      let &(ref func, ref body) = fs.iter().find(|&&(ref f,_)| {
        args.len() == f.params.len() && args.iter().zip(f.params.iter()).all(|(a,p)| {
          match (a, &((p.0).1).0) {
            (&Value::Float(_), &TypeSpecifierNonArray::Float) => true,
            (&Value::Uint(_), &TypeSpecifierNonArray::Uint) => true,
            _ => false,
          }
        })
      }).expect(&format!("Didn't find matching function signature for {}", name));

      vars.push();
      for (arg, param) in args.iter().zip(func.params.iter()) {
        vars.insert(param.1.as_ref().unwrap().0.clone(), arg.clone());
      }

      let res = execute(&body, vars, shader).unwrap();

      vars.pop();

      res
    },
    Expression::Variable(ref name) => { vars.get(name).clone() },
    Expression::FloatConstant(f) => { Value::Float(f) },
    Expression::IntConstant(i) => { Value::Int(i) },
    Expression::UintConstant(u) => { Value::Uint(u) },
    Expression::Comparison(ref left, ref op, ref right) => {
      let left = eval(left, vars, shader);
      let right = eval(right, vars, shader);

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
    Expression::Add(ref left, ref right) => {
      let left = eval(left, vars, shader);
      let right = eval(right, vars, shader);

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
      let left = eval(left, vars, shader);
      let right = eval(right, vars, shader);

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
      let left = eval(left, vars, shader);
      let right = eval(right, vars, shader);

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
      let left = eval(left, vars, shader);
      let right = eval(right, vars, shader);

      match (left, right) {
        (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a / b),
        (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::Index(ref container, ref index) => {
      let container = eval(container, vars, shader);
      let index = eval(index, vars, shader);
      let index = match index {
        Value::Int(i) => i as isize,
        Value::Uint(u) => u as isize,
        x => unreachable!("{:?}", x),
      };

      match container {
        Value::Buffer(ref s, p, _len) if s == "uint" => unsafe{ Value::Uint(*(p as *mut u32).offset(index)) },
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::FieldSelection(ref container, ref field) => {
      let container = eval(container, vars, shader);

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
      let mut lval = eval_lvalue(expr, vars, shader);
      let result: Value = if let LValue::Value(ref v) = lval { (*v).clone() } else { unimplemented!() };

      let incred = match result {
        Value::Uint(u) => Value::Uint(u + 1),
        Value::Int(u) => Value::Int(u + 1),
        x => unimplemented!("{:?}", x),
      };
      lval.set(incred);

      result
    },
    Expression::BinaryNot(ref expr) => {
      let v = eval(expr, vars, shader);
      match v {
        Value::Uint(u) => Value::Uint(!u),
        x => unimplemented!("{:?}", x),
      }
    },
    ref x => unimplemented!("{:?}", x),
  }
}

fn eval_lvalue<'a>(expression: &Expression, vars: &'a mut Vars, shader: &Shader) -> LValue<'a> {
  match *expression {
    Expression::Variable(ref name) => {
      vars.get_lvalue(name)
    },
    Expression::Index(ref container, ref index) => {
      let index = eval(index, vars, shader);
      let index = match index {
        Value::Int(i) => i as isize,
        Value::Uint(u) => u as isize,
        x => unreachable!("{:?}", x),
      };

      let container = eval_lvalue(container, vars, shader);

      match container {
        LValue::Buffer(ref s, p) if s == "uint" => unsafe{ LValue::Item("uint".to_string(), p.offset((index as usize * mem::size_of::<u32>() / mem::size_of::<u8>()) as isize)) },
        x => unimplemented!("{:?}", x),
      }
    },
    Expression::FieldSelection(ref container, ref field) => {
      let container = eval(container, vars, shader);

      match container {
        Value::Buffer(typ, p, _len) => {
          let info = shader.interfaces.iter().filter_map(|iface| match iface {
            &super::Interface::ShaderStorageBlock(ref info)
              | &super::Interface::UniformBlock(ref info)
              => if info.name == typ { Some(info) } else { None },
            x => unimplemented!("{:?}", x),
          }).next().unwrap();

          let field = info.active_variables.iter().find(|v| &v.name == field).unwrap();
          let new_type = match field.type_ {
            ::gl::GL_UNSIGNED_INT => "uint".to_string(),
            x => unimplemented!("{:x}", x),
          };

          LValue::Buffer(new_type, unsafe{ p.offset(field.offset as isize) })
        },
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
