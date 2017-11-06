use super::parse::{Statement,Expression,FunctionPrototype,TypeSpecifierNonArray};

use std::collections::HashMap;

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum Value {
  Int(i32),
  Uint(u32),
  Vec4([f32; 4]),
  Float(f32),
}

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum BuiltinFunc {
  Vec4Float4,
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
    }
  }

  fn all() -> Funcs {
    let mut funcs = HashMap::new();

    let vec4 = (vec![], (TypeSpecifierNonArray::Vec4, vec![]));
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

    funcs.insert("vec4".to_string(), vec![(vec4_float4, Statement::Builtin(BuiltinFunc::Vec4Float4))]);

    funcs
  }
}

type Funcs = HashMap<String, Vec<(FunctionPrototype, Statement)>>;

pub struct Vars {
  scopes: Vec<HashMap<String, Value>>,
}

impl Vars {
  fn new() -> Self {
    Self{
      scopes: vec![HashMap::new()],
    }
  }

  fn push(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn pop(&mut self) {
    self.scopes.pop().expect("Tried to pop an empty vars");
  }

  fn update(&mut self, ident: String, value: Value) {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(val) = scope.get_mut(&ident) {
        *val = value;
        return;
      }
    }
  }

  fn insert(&mut self, ident: String, value: Value) {
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

  fn get_mut(&mut self, ident: &String) -> &mut Value {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(val) = scope.get_mut(ident) {
        return val
      }
    }

    panic!("Didn't find {} in the vars", ident);
  }
}

fn execute(statement: &Statement, vars: &mut Vars, funcs: &Funcs) -> Option<Value> {
  match statement {
    &Statement::Compound(ref statements) => for statement in statements {
      let val = execute(statement, vars, funcs);
      if val.is_some() { return val; }
    },
    &Statement::Expression(ref expression) => {
      eval(expression, vars, funcs);
    },
    &Statement::Builtin(ref func) => {
      return Some(func.eval(vars))
    }
    ref x => unimplemented!("{:?}", x),
  }
  None
}

fn eval(expression: &Expression, vars: &mut Vars, funcs: &Funcs) -> Value {
  match *expression {
    Expression::Assignment(ref lexpr, ref rexpr) => {
      let value = eval(rexpr, vars, funcs);
      let slot = eval_lvalue(lexpr, vars, funcs);
      *slot = value;
      value
    },
    Expression::FunctionCall(ref name, ref args) => {
      let args = args.iter().map(|a| eval(a, vars, funcs)).collect::<Vec<_>>();
      let fs = funcs.get(name).unwrap();
      let &(ref func, ref body) = fs.iter().find(|&&(ref f,_)| {
        args.len() == f.params.len() && args.iter().zip(f.params.iter()).all(|(a,p)| {
          match (a, &((p.0).1).0) {
            (&Value::Float(_), &TypeSpecifierNonArray::Float) => true,
            _ => false,
          }
        })
      }).unwrap();

      vars.push();
      for (arg, param) in args.iter().zip(func.params.iter()) {
        vars.insert(param.1.as_ref().unwrap().0.clone(), *arg);
      }

      let res = execute(&body, vars, funcs).unwrap();

      vars.pop();

      res
    },
    Expression::Variable(ref name) => { *vars.get(name) },
    Expression::FloatConstant(f) => { Value::Float(f) },
    ref x => unimplemented!("{:?}", x),
  }
}

fn eval_lvalue<'a>(expression: &Expression, vars: &'a mut Vars, _funcs: &Funcs) -> &'a mut Value {
  match *expression {
    Expression::Variable(ref name) => {
      vars.get_mut(name)
    },
    ref x => unimplemented!("{:?}", x),
  }
}

#[test]
fn test_basic() {
  let s = r"
    in float c;
    out vec4 color;

    void main() {
      color = vec4(c, c, c, 1.0);
    }
  ";


  let shader = super::compile(s.as_bytes(), ::gl::GL_FRAGMENT_SHADER).unwrap();

  let input = 0.3;
  let output = [0.0; 4];

  let mut vars = Vars::new();
  let mut funcs = BuiltinFunc::all();
  {
    vars.insert("c".to_string(), Value::Float(input));
    vars.insert("color".to_string(), Value::Vec4(output));

    let main = shader.functions.iter().find(|f| f.0.name == "main").unwrap();

    execute(&main.1, &mut vars, &funcs);
  }

  assert_eq!(vars.get(&"c".to_string()), &Value::Float(0.3));
  assert_eq!(vars.get(&"color".to_string()), &Value::Vec4([0.3, 0.3, 0.3, 1.0]));
}
