use types::*;
use consts::*;

use std::mem;

use glsl::interpret::{Vars};
use glsl;
use std::sync::Arc;
use std::cell::{Ref};
use string_cache::DefaultAtom as Atom;
use std::collections::HashMap;
use std::os::raw::*;
use gl::{self,Context,Rect,parse_variable_name,fixed_to_float};


pub enum Primitive<'a> {
  Point(&'a Vars),
  Line(&'a Vars, &'a Vars),
  Triangle(&'a Vars, &'a Vars, &'a Vars),
}

impl<'a> Primitive<'a> {
  pub fn iter(&'a self) -> VertIter<'a> {
    VertIter{
      primitive: self,
      i: 0,
    }
  }
}

pub struct VertIter<'a> {
  primitive: &'a Primitive<'a>,
  i: usize,
}

impl<'a> Iterator for VertIter<'a> {
  type Item = &'a Vars;

  fn next(&mut self) -> Option<Self::Item> {
    let vars = match (self.primitive, self.i) {
      (&Primitive::Point(ref v), 0) => v,
      (&Primitive::Point(_), _) => return None,
      (&Primitive::Line(ref v, _), 0) => v,
      (&Primitive::Line(_, ref v), 1) => v,
      (&Primitive::Line(_, _), _) => return None,
      (&Primitive::Triangle(ref v, _, _), 0) => v,
      (&Primitive::Triangle(_, ref v, _), 1) => v,
      (&Primitive::Triangle(_, _, ref v), 2) => v,
      (&Primitive::Triangle(_, _, _), _) => return None,
    };

    self.i += 1;
    Some(vars)
  }
}

pub struct PrimitivePump<I: Iterator<Item=Vars>> {
  vertex_iter: I,
  mode: Mode,
}

enum Mode {
  Points(Option<Vars>),
  Lines(Option<Vars>, Option<Vars>),
  LineStrip(Option<Vars>, Option<Vars>),
  LineLoop(Option<Vars>, Option<Vars>, Option<Vars>),
  Triangles(Option<Vars>, Option<Vars>, Option<Vars>),
  TriangleFan(Option<Vars>, Option<Vars>, Option<Vars>),
  TriangleStrip(Option<Vars>, Option<Vars>, Option<Vars>, bool),
}

impl<I: Iterator<Item=Vars>> PrimitivePump<I> {
  pub fn new(
    vertex_iter: I,
    mode: GLenum,
  ) -> Self {
    let mode = match mode {
      GL_POINTS => Mode::Points(None),
      GL_LINES => Mode::Lines(None, None),
      GL_LINE_STRIP => Mode::LineStrip(None, None),
      GL_LINE_LOOP => Mode::LineLoop(None, None, None),
      GL_TRIANGLES => Mode::Triangles(None, None, None),
      GL_TRIANGLE_FAN => Mode::TriangleFan(None, None, None),
      GL_TRIANGLE_STRIP => Mode::TriangleStrip(None, None, None, true),
      x => unimplemented!("{:x}", x),
    };

    Self{
      vertex_iter,
      mode,
    }
  }

  pub fn next<'a>(&'a mut self) -> Option<Primitive<'a>> {
    match self.mode {
      Mode::Points(ref mut a) => {
        *a = self.vertex_iter.next();

        a.as_ref().map(|a| Primitive::Point(a))
      },
      Mode::Lines(ref mut a, ref mut b) => {
        *a = self.vertex_iter.next();
        *b = self.vertex_iter.next();

        if let (&mut Some(ref a), &mut Some(ref b)) = (a, b) {
          Some(Primitive::Line(a, b))
        } else { None }
      },
      Mode::LineStrip(ref mut a, ref mut b) => {
        if a.is_none() {
          *a = self.vertex_iter.next();
          *b = self.vertex_iter.next();
        } else {
          *a = mem::replace(b, self.vertex_iter.next());
        }

        if let (&mut Some(ref a), &mut Some(ref b)) = (a, b) {
          Some(Primitive::Line(a, b))
        } else { None }
      },
      Mode::LineLoop(ref mut first, ref mut a, ref mut b) => {
        if a.is_none() {
          *a = self.vertex_iter.next();
          *b = self.vertex_iter.next();
        } else {
          if first.is_none() {
            *first = mem::replace(a, mem::replace(b, self.vertex_iter.next()));
          } else {
            *a = mem::replace(b, self.vertex_iter.next());
          }
        }

        match (first, a, b) {
          (_, &mut Some(ref a), &mut Some(ref b)) => Some(Primitive::Line(a, b)),
          (&mut Some(ref first), &mut Some(ref a), None) => Some(Primitive::Line(a, first)),
          _ => None,
        }
      },
      Mode::Triangles(ref mut a, ref mut b, ref mut c) => {
        *a = self.vertex_iter.next();
        *b = self.vertex_iter.next();
        *c = self.vertex_iter.next();

        if let (&mut Some(ref a), &mut Some(ref b), &mut Some(ref c)) = (a, b, c) {
          Some(Primitive::Triangle(a, b, c))
        } else { None }
      },
      Mode::TriangleFan(ref mut a, ref mut b, ref mut c) => {
        if a.is_none() {
          *a = self.vertex_iter.next();
          *b = self.vertex_iter.next();
          *c = self.vertex_iter.next();
        } else {
          *b = mem::replace(c, self.vertex_iter.next());
        }

        if let (&mut Some(ref a), &mut Some(ref b), &mut Some(ref c)) = (a, b, c) {
          Some(Primitive::Triangle(a, b, c))
        } else { None }
      },
      Mode::TriangleStrip(ref mut a, ref mut b, ref mut c, ref mut write_a) => {
        if a.is_none() {
          *a = self.vertex_iter.next();
          *b = self.vertex_iter.next();
          *c = self.vertex_iter.next();
        } else {
          if *write_a {
            *a = mem::replace(c, self.vertex_iter.next());
          } else {
            *b = mem::replace(c, self.vertex_iter.next());
          }
          *write_a = !*write_a;
        }

        if let (&mut Some(ref a), &mut Some(ref b), &mut Some(ref c)) = (a, b, c) {
          Some(Primitive::Triangle(a, b, c))
        } else { None }
      },
    }
  }
}

#[allow(non_snake_case)]
pub(crate) fn glDrawElementsOneInstance(
  current: &'static mut Context,
  mode: GLenum,
  count: GLsizei,
  type_: GLenum,
  indices: *const c_void,
  instance: GLint,
  basevertex: GLuint,
  baseinstance: GLuint,
) {
  assert_eq!(instance, 0);
  assert_eq!(baseinstance, 0);

  let mut vertex_ids = vec![];

  {
    let buffers = &current.buffers;

    let buffer_pointer = if current.element_array_buffer == 0 {
      None
    } else {
      Some(buffers.get(&current.element_array_buffer).unwrap().as_ref().unwrap().as_ptr())
    };

    for i in 0..count {
      let i = i as usize;

      let pointer = if let Some(bp) = buffer_pointer {
        unsafe{ bp.add(indices as usize) }
      } else {
        indices as *const u8
      };

      let vertex_id = match type_ {
        GL_UNSIGNED_BYTE => {
          let pointer = pointer as *const GLubyte;
          unsafe{ *(pointer.add(i)) as i32 }
        },
        GL_UNSIGNED_SHORT => {
          let pointer = pointer as *const GLushort;
          unsafe{ *(pointer.add(i)) as i32 }
        },
        GL_UNSIGNED_INT => {
          let pointer = pointer as *const GLuint;
          unsafe{ *(pointer.add(i)) as i32 }
        },
        x => unimplemented!("{:x}", x),
      };

      let vertex_id = vertex_id + basevertex as i32;

      vertex_ids.push(vertex_id);
    }
  }

  draw_one(
    current,
    mode,
    vertex_ids,
    instance,
    baseinstance,
  );
}


#[allow(non_snake_case)]
pub(crate) fn glDrawArraysOneInstance(
  current: &'static mut Context,
  mode: GLenum,
  first: GLint,
  count: GLsizei,
  instance: GLint,
  baseinstance: GLuint,
) {
  draw_one(
    current,
    mode,
    (first..(first+count)).collect(),
    instance,
    baseinstance,
  );
}

fn draw_one(
  current: &'static mut Context,
  mode: GLenum,
  vertex_ids: Vec<GLint>,
  instance: GLint,
  baseinstance: GLuint,
) {
  use draw::*;

  use std::ops::Deref;

  use glsl::interpret::{self,Vars,Value};
  use glsl::{TypeSpecifierNonArray,Interface};

  assert_eq!(baseinstance, 0);

  fn init_out_vars(vars: &mut Vars, out_vars: &[glsl::Variable]) {
    for var in out_vars {
      let value = match &var.type_ {
        &TypeSpecifierNonArray::Float => Value::Float(Default::default()),
        &TypeSpecifierNonArray::Vec2 => Value::Vec2(Default::default()),
        &TypeSpecifierNonArray::Vec3 => Value::Vec3(Default::default()),
        &TypeSpecifierNonArray::Vec4 => Value::Vec4(Default::default()),
        &TypeSpecifierNonArray::Mat2 => Value::Mat2(Default::default()),
        &TypeSpecifierNonArray::Mat2x3 => Value::Mat2x3(Default::default()),
        &TypeSpecifierNonArray::Mat2x4 => Value::Mat2x4(Default::default()),
        &TypeSpecifierNonArray::Mat3x2 => Value::Mat3x2(Default::default()),
        &TypeSpecifierNonArray::Mat3 => Value::Mat3(Default::default()),
        &TypeSpecifierNonArray::Mat3x4 => Value::Mat3x4(Default::default()),
        &TypeSpecifierNonArray::Mat4x2 => Value::Mat4x2(Default::default()),
        &TypeSpecifierNonArray::Mat4x3 => Value::Mat4x3(Default::default()),
        &TypeSpecifierNonArray::Mat4 => Value::Mat4(Default::default()),
        &TypeSpecifierNonArray::Int => Value::Int(Default::default()),
        &TypeSpecifierNonArray::IVec2 => Value::IVec2(Default::default()),
        &TypeSpecifierNonArray::IVec3 => Value::IVec3(Default::default()),
        &TypeSpecifierNonArray::IVec4 => Value::IVec4(Default::default()),
        &TypeSpecifierNonArray::Uint => Value::Uint(Default::default()),
        &TypeSpecifierNonArray::UVec2 => Value::UVec2(Default::default()),
        &TypeSpecifierNonArray::UVec3 => Value::UVec3(Default::default()),
        &TypeSpecifierNonArray::UVec4 => Value::UVec4(Default::default()),

        x => unimplemented!("{:?}", x),
      };

      let value = var.array.iter().rev().fold(value, |val, &size| {
        let size = size as usize;
        let mut v = Vec::with_capacity(size);
        v.resize(size, val);
        Value::Array(v)
      });

      vars.insert(var.name.clone(), value);
    }
  }


  let primitive = match mode {
    GL_POINTS => GL_POINTS,
    GL_LINE_STRIP | GL_LINE_LOOP | GL_LINES => GL_LINES,
    GL_TRIANGLE_STRIP | GL_TRIANGLE_FAN | GL_TRIANGLES => GL_TRIANGLES,
    x => unimplemented!("{:x}", x),
  };

  let program = current.programs.get(&current.program).unwrap();

  let vert_shader = program.shaders.iter().find(|s| s.type_ == GL_VERTEX_SHADER).unwrap();
  let vert_compiled = Arc::clone(Ref::map(vert_shader.compiled.borrow(), |s| s.as_ref().unwrap()).deref());

  let frag_shader = program.shaders.iter().find(|s| s.type_ == GL_FRAGMENT_SHADER).unwrap();
  let frag_compiled = Arc::clone(Ref::map(frag_shader.compiled.borrow(), |s| s.as_ref().unwrap()).deref());

  let vert_in_vars = vert_compiled.interfaces.iter().filter_map(|i| if let &glsl::Interface::Input(ref v) = i { Some(v.clone()) } else { None }).collect::<Vec<_>>();
  let vert_out_vars = vert_compiled.interfaces.iter().filter_map(|i| if let &glsl::Interface::Output(ref v) = i { Some(v.clone()) } else { None }).collect::<Vec<_>>();

  let frag_in_vars = frag_compiled.interfaces.iter().filter_map(|i| if let &glsl::Interface::Input(ref v) = i { Some(v.clone()) } else { None }).collect::<Vec<_>>();
  let frag_out_vars = frag_compiled.interfaces.iter().filter_map(|i| if let &glsl::Interface::Output(ref v) = i { Some(v.clone()) } else { None }).collect::<Vec<_>>();


  // TODO: do something about these
  let mut vert_uniforms = HashMap::new();
  for iface in &vert_compiled.interfaces {
    match iface {
      &Interface::Uniform(ref info) => {
        let index = program.uniforms.iter().position(|i| i.name == info.name).unwrap();
        let val = &program.uniform_values[index];
        let val = match val {
          &Value::UImage2DUnit(unit) => {
            let unit_target = current.images[unit];
            let texture = current.textures.get(&unit_target).expect("No texture for unit");
            let texture = texture.as_ref().expect("No texture in slot");

            Value::UImage2D(Arc::clone(texture))
          },
          x => x.clone(),
        };
        if info.typ == GL_UNSIGNED_INT_ATOMIC_COUNTER { continue; }
        vert_uniforms.insert(info.name.clone(), val);
      },
      _ => {},
    }
  }
  let mut frag_uniforms = HashMap::new();
  for iface in &frag_compiled.interfaces {
    match iface {
      &Interface::Uniform(ref info) => {
        let index = program.uniforms.iter().position(|i| i.name == info.name).unwrap();
        let val = &program.uniform_values[index];
        let val = match val {
          &Value::UImage2DUnit(unit) => {
            let unit_target = current.images[unit];
            let texture = current.textures.get(&unit_target).expect("No texture for unit");
            let texture = texture.as_ref().expect("No texture in slot");

            Value::UImage2D(Arc::clone(texture))
          },
          x => x.clone(),
        };
        if info.typ == GL_UNSIGNED_INT_ATOMIC_COUNTER { continue; }
        frag_uniforms.insert(info.name.clone(), val);
      },
      _ => {},
    }
  }

  assert_eq!(current.vertex_array, 0);

  let vertex_array = current.vertex_arrays.get(&current.vertex_array).unwrap().as_ref().unwrap();
  let current_vertex_attrib = &current.current_vertex_attrib;

  // TODO: thread
  gl::glFinish();

  let buffers = &current.buffers;

  let vertex_results = vertex_ids.into_iter().map(|i| {
    let mut vars = Vars::new();

    for (loc, name) in program.attrib_locations.iter().enumerate().filter_map(|(i, ref n)| n.as_ref().map(|ref n| (i, n.clone()))) {
      let in_var = vert_in_vars.iter().find(|v| &v.name == name).unwrap();
      let type_ = &in_var.type_;

      let attrib = &vertex_array.attribs[loc as usize];
      let binding = &vertex_array.bindings[loc as usize];

      let value = if attrib.enabled {
        let buffer_pointer = if binding.buffer == 0 {
          None
        } else {
          Some(buffers.get(&binding.buffer).unwrap().as_ref().unwrap().as_ptr())
        };

        let attrib_index = if binding.divisor == 0 {
          i
        } else {
          instance / binding.divisor as i32
        };

        let p = if let Some(abp) = buffer_pointer {
          unsafe{ abp.offset(attrib.pointer as isize).offset((binding.stride * attrib_index) as isize + binding.offset) as *const c_void }
        } else {
          unsafe{ attrib.pointer.offset((binding.stride * attrib_index) as isize) }
        };

        #[derive(Debug)]
        enum AttribValue {
          Float([GLfloat; 4]),
          Int([GLint; 4]),
          Uint([GLuint; 4]),
          Short([GLshort; 4]),
          Byte([GLbyte; 4]),
          Fixed([GLfixed; 4]),
        }

        let attrib_value = match attrib.type_ {
          GL_FLOAT => {
            let p = p as *const GLfloat;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0.0, 0.0, 1.0],
              2 => [p.read(), p.add(1).read(), 0.0, 1.0],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), 1.0],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Float(v)
          },
          GL_INT => {
            let p = p as *const GLint;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, 1],
              2 => [p.read(), p.add(1).read(), 0, 1],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), 1],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Int(v)
          },
          GL_UNSIGNED_INT => {
            let p = p as *const GLuint;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, 1],
              2 => [p.read(), p.add(1).read(), 0, 1],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), 1],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Uint(v)
          },
          GL_SHORT => {
            let p = p as *const GLshort;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, 1],
              2 => [p.read(), p.add(1).read(), 0, 1],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), 1],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Short(v)
          },
          GL_BYTE => {
            let p = p as *const GLbyte;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, 1],
              2 => [p.read(), p.add(1).read(), 0, 1],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), 1],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Byte(v)
          },
          GL_FIXED => {
            let p = p as *const GLfixed;
            let one = 0x0001_0000;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Fixed(v)
          },
          x => unimplemented!("{:x}", x),
        };

        match (attrib_value, type_) {
          (AttribValue::Float(f), &TypeSpecifierNonArray::Float) => Value::Float(f[0]),
          (AttribValue::Float(f), &TypeSpecifierNonArray::Vec2) => Value::Vec2([f[0], f[1]]),
          (AttribValue::Float(f), &TypeSpecifierNonArray::Vec3) => Value::Vec3([f[0], f[1], f[2]]),
          (AttribValue::Float(f), &TypeSpecifierNonArray::Vec4) => Value::Vec4([f[0], f[1], f[2], f[3]]),
          (AttribValue::Int(i), &TypeSpecifierNonArray::Int) => Value::Int(i[0]),
          (AttribValue::Int(i), &TypeSpecifierNonArray::IVec2) => Value::IVec2([i[0], i[1]]),
          (AttribValue::Int(i), &TypeSpecifierNonArray::IVec3) => Value::IVec3([i[0], i[1], i[2]]),
          (AttribValue::Int(i), &TypeSpecifierNonArray::IVec4) => Value::IVec4([i[0], i[1], i[2], i[3]]),
          (AttribValue::Uint(u), &TypeSpecifierNonArray::Uint) => Value::Uint(u[0]),
          (AttribValue::Uint(u), &TypeSpecifierNonArray::UVec2) => Value::UVec2([u[0], u[1]]),
          (AttribValue::Uint(u), &TypeSpecifierNonArray::UVec3) => Value::UVec3([u[0], u[1], u[2]]),
          (AttribValue::Uint(u), &TypeSpecifierNonArray::UVec4) => Value::UVec4([u[0], u[1], u[2], u[3]]),
          (AttribValue::Short(s), &TypeSpecifierNonArray::Vec4) => Value::Vec4([s[0].into(), s[1].into(), s[2].into(), s[3].into()]),
          (AttribValue::Byte(b), &TypeSpecifierNonArray::Vec4) => Value::Vec4([b[0].into(), b[1].into(), b[2].into(), b[3].into()]),
          (AttribValue::Fixed(f), &TypeSpecifierNonArray::Vec4) => Value::Vec4([fixed_to_float(f[0]), fixed_to_float(f[1]), fixed_to_float(f[2]), fixed_to_float(f[3])]),
          (v, t) => unimplemented!("{:?} {:?}", v, t),
        }
      } else {
        let attrib = current_vertex_attrib[loc as usize];

        // TODO: non-float types
        Value::Vec4(attrib)
      };

      vars.insert(name.clone(), value);
    }

    init_out_vars(&mut vars, &vert_out_vars);

    for (ref name, ref data) in &vert_uniforms {
      vars.insert((*name).clone(), (*data).clone());
    }

    let main = &vert_compiled.functions[&"main".into()][0];

    vars.push();
    interpret::execute(&main.1, &mut vars, &vert_compiled);

    vars
  });

  // TODO: tesselation
  // TODO: geometry

  let tf = if Some(primitive) == current.transform_feedback_capture && !current.transform_feedback_paused {
    let tfv = &program.transform_feedback.as_ref().unwrap();

    let separate = tfv.1;

    let ps = if separate {
      let mut ps = vec![];

      for i in 0..tfv.0.len() {
        let binding = &current.indexed_transform_feedback_buffer[i];
        ps.push(unsafe{ current.buffers.get(&binding.buffer).unwrap().as_ref().unwrap().as_ptr().offset(binding.offset) as *mut _});
      }

        ps
    } else {
      let binding = &current.indexed_transform_feedback_buffer[0];
      vec![unsafe{ current.buffers.get(&binding.buffer).unwrap().as_ref().unwrap().as_ptr().offset(binding.offset) as *mut _}]
    };

    Some(ps)
  } else { None };

  let mut pump = PrimitivePump::new(vertex_results, mode);

  while let Some(prim) = pump.next() {
    if let Some(ref ps) = tf {
      let tf = current.transform_feedbacks.get_mut(&current.transform_feedback).unwrap().as_mut().unwrap();

      if let Some((id, target)) = current.query {
        if target == GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN {
          current.queries.get_mut(&id).unwrap().as_mut().unwrap().value += 1;
        }
      }
      let tfv = &program.transform_feedback.as_ref().unwrap();

      let tf_vars = &tfv.0;
      let separate = tfv.1;

      for vert in prim.iter() {
        for (i, var) in tf_vars.iter().enumerate() {
          let index = if separate { i } else { 0 };

          let p = ps[index];
          let offset = &mut tf.offsets[index];

          fn push<T: Copy>(v: &T, p: *mut u8, offset: &mut isize) {
            unsafe {
              *(p.offset(*offset) as *mut T) = *v;
            }
            *offset += mem::size_of::<T>() as isize;
          }

          fn push_all(value: &Value, p: *mut u8, offset: &mut isize) {
            match value {
              &Value::Float(ref f) => push(f, p, offset),
              &Value::Vec2(ref v) => push(v, p, offset),
              &Value::Vec3(ref v) => push(v, p, offset),
              &Value::Vec4(ref v) => push(v, p, offset),
              &Value::Mat2(ref v) => push(v, p, offset),
              &Value::Mat2x3(ref v) => push(v, p, offset),
              &Value::Mat2x4(ref v) => push(v, p, offset),
              &Value::Mat3x2(ref v) => push(v, p, offset),
              &Value::Mat3(ref v) => push(v, p, offset),
              &Value::Mat3x4(ref v) => push(v, p, offset),
              &Value::Mat4x2(ref v) => push(v, p, offset),
              &Value::Mat4x3(ref v) => push(v, p, offset),
              &Value::Mat4(ref v) => push(v, p, offset),
              &Value::Int(ref i) => push(i, p, offset),
              &Value::IVec2(ref i) => push(i, p, offset),
              &Value::IVec3(ref i) => push(i, p, offset),
              &Value::IVec4(ref i) => push(i, p, offset),
              &Value::Uint(ref u) => push(u, p, offset),
              &Value::UVec2(ref u) => push(u, p, offset),
              &Value::UVec3(ref u) => push(u, p, offset),
              &Value::UVec4(ref u) => push(u, p, offset),

              &Value::Array(ref a) => for i in a {
                push_all(i, p, offset);
              },

              x => unimplemented!("{:?}", x),
            }
          }

          let (name, array_indices) = parse_variable_name(var);

          let value = vert.get(&name.into());
          let value = array_indices.iter().fold(value, |v, &i| {
            if let &Value::Array(ref a) = v {
              &a[i as usize]
            } else { unreachable!() }
          });


          push_all(&value, p, offset);

        }
      }
    } // transform feedback

    // RASTERIZATION

    fn window_coords(vars: &Vars, viewport: Rect, depth_range: (GLfloat, GLfloat)) -> [f32; 3] {
        let clip_coords = if let &Value::Vec4(ref v) = vars.get(&"gl_Position".into()) {
          v
        } else { unreachable!() };
        let ndc = [clip_coords[0] / clip_coords[3],
                   clip_coords[1] / clip_coords[3],
                   clip_coords[2] / clip_coords[3]];

        let px = viewport.2 as f32;
        let py = viewport.3 as f32;
        let ox = viewport.0 as f32 + px / 2.0;
        let oy = viewport.1 as f32 + py / 2.0;
        let (n, f) = depth_range;

        let window_coords = [
          px / 2.0 * ndc[0] + ox,
          py / 2.0 * ndc[1] + oy,
          (f-n) / 2.0 * ndc[2] + (n+f) / 2.0,
        ];

      window_coords
    }

    fn do_fragment(
      context: &Context,
      (x, y): (i32, i32),
      z: f32,
      mut vert_vars: Vars,
      frag_in_vars: &Vec<glsl::Variable>,
      frag_out_vars: &Vec<glsl::Variable>,
      frag_uniforms: &HashMap<Atom, Value>,
      frag_compiled: &Arc<glsl::Shader>,
    ) {
      let mut draw_surface = unsafe{ context.draw_surface.as_mut() };
      let draw_surface = draw_surface.as_mut().unwrap();

      // TODO: real clipping
      if x < 0 || x >= draw_surface.width || y < 0 || y >= draw_surface.height {
        return;
      }

      let draw_framebuffer = context.draw_framebuffer;
      let color_mask = context.color_mask;


      if context.depth_test {
        let old_z = draw_surface.get_depth(x, y);

        if !context.depth_func.cmp(old_z, z) {
          return;
        }
      }

      let color;
      {
        let mut vars = Vars::new();

        init_out_vars(&mut vars, frag_out_vars);

        for var in frag_in_vars {
          vars.insert(var.name.clone(), vert_vars.take(&var.name));
        }

        for (ref name, ref data) in frag_uniforms {
          vars.insert((*name).clone(), (*data).clone());
        }

        let main = &frag_compiled.functions[&"main".into()][0];

        vars.push();
        interpret::execute(&main.1, &mut vars, &frag_compiled);

        if let &Value::Vec4(c) = vars.get(&frag_out_vars[0].name) {
          color = (c[0], c[1], c[2], c[3]);
        } else { unreachable!() }
      } // Fragment

      assert_eq!(draw_framebuffer, 0);

      draw_surface.set_pixel(x, y, color, color_mask);
      draw_surface.set_depth(x, y, z);
    }

    match prim {
      Primitive::Point(p) => {
        let window_coords = window_coords(p, current.viewport, current.depth_range);

        let x = window_coords[0] as i32;
        let y = window_coords[1] as i32;

        do_fragment(
          current,
          (x, y),
          window_coords[2],
          p.clone(),
          &frag_in_vars,
          &frag_out_vars,
          &frag_uniforms,
          &frag_compiled,
        );
      },
      Primitive::Line(a, b) => {
        let p_a = window_coords(a, current.viewport, current.depth_range);
        let p_b = window_coords(b, current.viewport, current.depth_range);

        let delta_x = p_b[0]-p_a[0];
        let delta_y = p_b[1]-p_a[1];
        let slope = delta_y / delta_x;

        let x_major = slope >= -1.0 && slope <= 1.0;

        let mut x = p_a[0];
        let mut y = p_a[1];
        let mut t = 1.0;


        fn interp_vars(frag_in_vars: &Vec<glsl::Variable>, a: &Vars, b: &Vars, t: f32) -> Vars {
          let mut vars = Vars::new();

          for var in frag_in_vars {
            let a_val = a.get(&var.name).clone();
            let b_val = b.get(&var.name).clone();

            let val = if var.flat {
              a_val
            } else {
              interpret::add(&interpret::mul(&a_val, t), &interpret::mul(&b_val, 1.0 - t))
            };

            vars.insert(var.name.clone(), val);
          }

          vars
        }
        fn lerp(a: f32, b: f32, t: f32) -> f32 {
          a * t + b * (1.0-t)
        }


        if x_major {
          let idx = 1.0 / delta_x.abs();

          if p_a[0] < p_b[0] {
            while x < p_b[0] {
              let frag_vals = interp_vars(&frag_in_vars, a, b, t);
              let z = lerp(p_a[2], p_b[2], t);

              do_fragment(
                current,
                (x.round() as i32, y.round() as i32),
                z,
                frag_vals,
                &frag_in_vars,
                &frag_out_vars,
                &frag_uniforms,
                &frag_compiled,
              );

              y += slope;
              x += 1.0;
              t -= idx;
            }
          } else {
            while x > p_b[0] {
              let frag_vals = interp_vars(&frag_in_vars, a, b, t);
              let z = lerp(p_a[2], p_b[2], t);

              do_fragment(
                current,
                (x.round() as i32, y.round() as i32),
                z,
                frag_vals,
                &frag_in_vars,
                &frag_out_vars,
                &frag_uniforms,
                &frag_compiled,
              );

              y -= slope;
              x -= 1.0;
              t -= idx;
            }
          }
        } else {
          let idy = 1.0 / delta_y.abs();
          let islope = delta_x / delta_y;

          if p_a[1] < p_b[1] {
            while y < p_b[1] {
              let frag_vals = interp_vars(&frag_in_vars, a, b, t);
              let z = lerp(p_a[2], p_b[2], t);

              do_fragment(
                current,
                (x.round() as i32, y.round() as i32),
                z,
                frag_vals,
                &frag_in_vars,
                &frag_out_vars,
                &frag_uniforms,
                &frag_compiled,
              );

              x += islope;
              y += 1.0;
              t -= idy;
            }
          } else {
            while y > p_b[1] {
              let frag_vals = interp_vars(&frag_in_vars, a, b, t);
              let z = lerp(p_a[2], p_b[2], t);

              do_fragment(
                current,
                (x.round() as i32, y.round() as i32),
                z,
                frag_vals,
                &frag_in_vars,
                &frag_out_vars,
                &frag_uniforms,
                &frag_compiled,
              );

              x -= islope;
              y -= 1.0;
              t -= idy;
            }
          }
        }
      },
      Primitive::Triangle(a, b, c) => {
        let p_a = window_coords(a, current.viewport, current.depth_range);
        let p_b = window_coords(b, current.viewport, current.depth_range);
        let p_c = window_coords(c, current.viewport, current.depth_range);

        let front_facing: bool = [(&p_a, &p_b), (&p_b, &p_c), (&p_c, &p_a)].iter()
          .map(|(i, j)| {
            i[0] * j[1] - j[0] * i[1]
          }).sum::<f32>() > 0.0;
        let front_facing = front_facing ^ current.front_face_cw;

        if current.culling {
          if current.cull_face == GL_FRONT_AND_BACK
            || (current.cull_face == GL_FRONT && front_facing)
            || (current.cull_face == GL_BACK && !front_facing) {
              continue;
            }
        }

        let (mut left, mut middle, mut right) = (p_a, p_b, p_c);
        if middle[0] < left[0] { mem::swap(&mut left, &mut middle) };
        if right[0] < middle[0] { mem::swap(&mut middle, &mut right) };
        if middle[0] < left[0] { mem::swap(&mut left, &mut middle) };
        let (left, middle, right) = (left, middle, right);

        let slope_lm = (middle[1]-left[1]) / (middle[0]-left[0]);
        let slope_mr = (right[1]-middle[1]) / (right[0]-middle[0]);
        let slope_lr = (right[1]-left[1]) / (right[0]-left[0]);

        let lr_top = slope_lr > slope_lm;

        let mut x = left[0];
        let mut y_lr = left[1];
        let mut y_lmr = left[1];

        let first_hop = 1.0 - (x + 0.5).fract();

        x += first_hop;
        y_lr += first_hop * slope_lr;
        y_lmr += first_hop * slope_lm;

        if left[0] == middle[0] {
          y_lmr = middle[1];
        }

        #[inline]
        fn barycentric(x: f32, y: f32, a: [f32; 3], b: [f32; 3], c: [f32; 3]) -> (f32, f32, f32) {
          let [x1, y1, _] = a;
          let [x2, y2, _] = b;
          let [x3, y3, _] = c;

          // From https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates
          let bary_a = ((y2 - y3)*(x - x3) + (x3 - x2)*(y - y3)) / ((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3));
          let bary_b = ((y3 - y1)*(x - x3) + (x1 - x3)*(y - y3)) / ((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3));
          let bary_c = 1.0 - bary_a - bary_b;
          (bary_a, bary_b, bary_c)
        }

        fn bary_interp_vars(frag_in_vars: &Vec<glsl::Variable>, a: &Vars, b: &Vars, c: &Vars, (bary_a, bary_b, bary_c): (f32, f32, f32)) -> Vars {
          let mut vars = Vars::new();

          for var in frag_in_vars {
            let a_val = a.get(&var.name);
            let b_val = b.get(&var.name);
            let c_val = c.get(&var.name);

            let val = if var.flat {
              a_val.clone()
            } else {
              interpret::add(
                &interpret::mul(&a_val, bary_a),
                &interpret::add(
                  &interpret::mul(&b_val, bary_b),
                  &interpret::mul(&c_val, bary_c),
                )
              )
            };

            vars.insert(var.name.clone(), val);
          }

          vars
        }


        while x < right[0] {
          let (bottom, top) = if lr_top { (y_lmr, y_lr) } else { (y_lr, y_lmr) };
          let mut y = bottom.round() + 0.5;
          while y < top.round() {
            let barys = barycentric(x, y, p_a, p_b, p_c);

            let frag_vals = bary_interp_vars(&frag_in_vars, a, b, c, barys);

            let z = p_a[2] * barys.0
              + p_b[2] * barys.1
              + p_c[2] * barys.2;

            do_fragment(
              current,
              (x.floor() as i32, y.floor() as i32),
              z,
              frag_vals,
              &frag_in_vars,
              &frag_out_vars,
              &frag_uniforms,
              &frag_compiled,
            );
            y += 1.0;
          }

          if x < middle[0] && x + 1.0 > middle[0] {
            let hop_to_mid = middle[0] - x;
            let hop_from_mid = 1.0 - hop_to_mid;
            y_lmr += slope_lm * hop_to_mid + slope_mr * hop_from_mid;

          } else if x < middle[0] {
            y_lmr += slope_lm;
          } else {
            y_lmr += slope_mr;
          }

          x += 1.0;
          y_lr += slope_lr;
        }
      },
    }

  }
}
