use types::*;
use consts::*;
use conversions::*;

use std::mem;
use std::cmp;

use glsl::interpret::{Vars,self};
use glsl;
use std::sync::Arc;
use std::cell::{Ref};
use string_cache::DefaultAtom as Atom;
use std::collections::HashMap;
use std::os::raw::*;
use gl::{self,Context,Rect,parse_variable_name};


pub enum Primitive {
  Point(Vars),
  Line(Vars, Vars),
  Triangle(Vars, Vars, Vars),
}

impl Primitive {
  pub fn iter(&self) -> VertIter {
    VertIter{
      primitive: self,
      i: 0,
    }
  }
}

pub struct VertIter<'a> {
  primitive: &'a Primitive,
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
  Points,
  Lines,
  LineStrip(Vars),
  LineLoop(Vars, Vars, LineLoopState),
  Triangles,
  TriangleFan(Vars, Vars),
  TriangleStrip(Vars, Vars, bool),
  Empty,
}

enum LineLoopState {
  Pre,
  Running,
  Done,
}

impl<I: Iterator<Item=Vars>> PrimitivePump<I> {
  pub fn new(
    mut vertex_iter: I,
    mode: GLenum,
  ) -> Self {
    let mode = match mode {
      GL_POINTS => Mode::Points,
      GL_LINES => Mode::Lines,
      GL_LINE_STRIP => if let Some(prev) = vertex_iter.next() {
        Mode::LineStrip(prev)
      } else {
        Mode::Empty
      },
      GL_LINE_LOOP => if let (Some(first), Some(prev)) = (vertex_iter.next(), vertex_iter.next()) {
        Mode::LineLoop(first, prev, LineLoopState::Pre)
      } else {
        Mode::Empty
      },
      GL_TRIANGLES => Mode::Triangles,
      GL_TRIANGLE_FAN => if let (Some(first), Some(prev)) = (vertex_iter.next(), vertex_iter.next()) {
        Mode::TriangleFan(first, prev)
      } else {
        Mode::Empty
      },
      GL_TRIANGLE_STRIP => if let (Some(prev1), Some(prev2)) = (vertex_iter.next(), vertex_iter.next()) {
        Mode::TriangleStrip(prev1, prev2, true)
      } else {
        Mode::Empty
      },
      x => unimplemented!("{:x}", x),
    };

    Self{
      vertex_iter,
      mode,
    }
  }
}

impl<I: Iterator<Item=Vars>> Iterator for PrimitivePump<I> {
  type Item = Primitive;

  fn next(&mut self) -> Option<Primitive> {
    match self.mode {
      Mode::Empty => None,
      Mode::Points => {
        if let Some(p) = self.vertex_iter.next() {
          Some(Primitive::Point(p))
        } else { None }
      },
      Mode::Lines => {
        let a = self.vertex_iter.next();
        let b = self.vertex_iter.next();

        if let (Some(a), Some(b)) = (a, b) {
          Some(Primitive::Line(a, b))
        } else { None }
      },
      Mode::LineStrip(ref mut prev) => {
        if let Some(new) = self.vertex_iter.next() {
          let b = new.clone();
          let a = mem::replace(prev, new);

          Some(Primitive::Line(a, b))
        } else { None }
      },
      Mode::LineLoop(ref first, ref mut prev, ref mut state) => {
        match *state {
          LineLoopState::Pre => {
            *state = LineLoopState::Running;

            Some(Primitive::Line(first.clone(), prev.clone()))
          },
          LineLoopState::Running => {
            if let Some(new) = self.vertex_iter.next() {
              let b = new.clone();
              let a = mem::replace(prev, new);

              Some(Primitive::Line(a, b))
            } else {
              let a = prev.clone();
              let b = first.clone();

              *state = LineLoopState::Done;

              Some(Primitive::Line(a, b))
            }
          },
          LineLoopState::Done => { None }
        }
      },
      Mode::Triangles => {
        let a = self.vertex_iter.next();
        let b = self.vertex_iter.next();
        let c = self.vertex_iter.next();

        if let (Some(a), Some(b), Some(c)) = (a, b, c) {
          Some(Primitive::Triangle(a, b, c))
        } else { None }
      },
      Mode::TriangleFan(ref mut first, ref mut prev) => {
        if let Some(new) = self.vertex_iter.next() {
          let a = first.clone();
          let c = new.clone();
          let b = mem::replace(prev, new);

          Some(Primitive::Triangle(a, b, c))
        } else { None }
      },
      Mode::TriangleStrip(ref mut prev1, ref mut prev2, ref mut write_1) => {
        if let Some(new) = self.vertex_iter.next() {
          let c = new.clone();
          let (a, b) = if *write_1 {
            (mem::replace(prev1, new), prev2.clone())
          } else {
            (prev1.clone(), mem::replace(prev2, new))
          };
          *write_1 = !*write_1;

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
          unsafe{ abp.offset(binding.offset as isize).offset((binding.stride * attrib_index) as isize) as *const c_void }
        } else {
          unsafe{ attrib.pointer.offset((binding.stride * attrib_index) as isize) }
        };

        #[derive(Debug)]
        enum AttribValue {
          Float([GLfloat; 4]),
          Int([GLint; 4]),
          Uint([GLuint; 4]),
          Short([GLshort; 4]),
          Ushort([GLushort; 4]),
          Byte([GLbyte; 4]),
          Ubyte([GLubyte; 4]),
          Fixed([GLfixed; 4]),
          HalfFloat([GLhalf; 4]),
          I2_10_10_10([i32; 4]),
          U2_10_10_10([u32; 4]),
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
            let one = if attrib.normalized { 0x7F_FF_FF_FF } else { 1 };

            let p = p as *const GLint;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Int(v)
          },
          GL_UNSIGNED_INT => {
            let one = if attrib.normalized { 0x7F_FF_FF_FFu32 } else { 1 };

            let p = p as *const GLuint;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Uint(v)
          },
          GL_SHORT => {
            let one = if attrib.normalized { 0x7F_FF } else { 1 };

            let p = p as *const GLshort;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Short(v)
          },
          GL_UNSIGNED_SHORT => {
            let one = if attrib.normalized { 0xFF_FF } else { 1 };

            let p = p as *const GLushort;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Ushort(v)
          },
          GL_BYTE => {
            let one = if attrib.normalized { 0x7F } else { 1 };

            let p = p as *const GLbyte;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Byte(v)
          },
          GL_UNSIGNED_BYTE => {
            let one = if attrib.normalized { 0xFF } else { 1 };

            let p = p as *const GLubyte;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Ubyte(v)
          },
          GL_FIXED => {
            let one = 0x0001_0000;

            let p = p as *const GLfixed;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, one],
              2 => [p.read(), p.add(1).read(), 0, one],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), one],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::Fixed(v)
          },
          GL_HALF_FLOAT => {
            let p = p as *const GLhalf;
            let v = unsafe { match attrib.size {
              1 => [p.read(), 0, 0, 1],
              2 => [p.read(), p.add(1).read(), 0, 1],
              3 => [p.read(), p.add(1).read(), p.add(2).read(), 1],
              4 => [p.read(), p.add(1).read(), p.add(2).read(), p.add(3).read()],
              x => unimplemented!("{}", x),
            }};
            AttribValue::HalfFloat(v)
          },
          GL_INT_2_10_10_10_REV => {
            assert_eq!(attrib.size, 4);
            let p = p as *const i32;
            let raw = unsafe{ p.read() };

            let w = raw >> 30;
            let z = (raw << 2) >> 22;
            let y = (raw << 12) >> 22;
            let x = (raw << 22) >> 22;

            AttribValue::I2_10_10_10([x, y, z, w])
          },
          GL_UNSIGNED_INT_2_10_10_10_REV => {
            assert_eq!(attrib.size, 4);
            let p = p as *const u32;
            let raw = unsafe{ p.read() };

            let w = raw >> 30;
            let z = (raw >> 20) & 0x3_FF;
            let y = (raw >> 10) & 0x3_FF;
            let x = (raw >>  0) & 0x3_FF;

            AttribValue::U2_10_10_10([x, y, z, w])
          },
          x => unimplemented!("{:x}", x),
        };

        let target_type = match type_ {
          TypeSpecifierNonArray::Float | TypeSpecifierNonArray::Vec2 | TypeSpecifierNonArray::Vec3 | TypeSpecifierNonArray::Vec4 => TypeSpecifierNonArray::Vec4,
          TypeSpecifierNonArray::Int | TypeSpecifierNonArray::IVec2 | TypeSpecifierNonArray::IVec3 | TypeSpecifierNonArray::IVec4 => TypeSpecifierNonArray::IVec4,
          TypeSpecifierNonArray::Uint | TypeSpecifierNonArray::UVec2 | TypeSpecifierNonArray::UVec3 | TypeSpecifierNonArray::UVec4 => TypeSpecifierNonArray::UVec4,
          x => unimplemented!("{:?}", x),
        };

        fn map4<A: Copy, B>(a: [A; 4], f: impl Fn(A) -> B) -> [B; 4] {
          [
            f(a[0]),
            f(a[1]),
            f(a[2]),
            f(a[3]),
          ]
        }

        let intermediate = match (attrib_value, target_type, attrib.normalized) {
          (AttribValue::Float(f), TypeSpecifierNonArray::Vec4, _) => Value::Vec4(f),
          (AttribValue::Int(i), TypeSpecifierNonArray::IVec4, _) => Value::IVec4(i),
          (AttribValue::Uint(u), TypeSpecifierNonArray::UVec4, _) => Value::UVec4(u),

          (AttribValue::Int(i), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(i, |i| i as _)),
          (AttribValue::Int(i), TypeSpecifierNonArray::Vec4, true) => Value::Vec4(map4(i, normalize_i32)),

          (AttribValue::Uint(u), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(u, |u| u as _)),
          (AttribValue::Uint(u), TypeSpecifierNonArray::Vec4, true) => Value::Vec4(map4(u, normalize_u32)),
          (AttribValue::HalfFloat(f), TypeSpecifierNonArray::Vec4, _) => Value::Vec4(map4(f, f16_to_f32)),
          (AttribValue::Fixed(f), TypeSpecifierNonArray::Vec4, _) => Value::Vec4(map4(f, fixed_to_float)),
          (AttribValue::Short(s), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(s, |s| s as _)),
          (AttribValue::Short(s), TypeSpecifierNonArray::Vec4, true) => Value::Vec4(map4(s, normalize_i16)),
          (AttribValue::Short(s), TypeSpecifierNonArray::IVec4, _) => Value::IVec4(map4(s, |s| s as _)),
          (AttribValue::Short(s), TypeSpecifierNonArray::UVec4, _) => Value::UVec4(map4(s, |s| s as _)),
          (AttribValue::Ushort(s), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(s, |s| s as _)),
          (AttribValue::Ushort(s), TypeSpecifierNonArray::Vec4, true) => Value::Vec4(map4(s, normalize_u16)),
          (AttribValue::Ushort(s), TypeSpecifierNonArray::UVec4, _) => Value::UVec4(map4(s, |s| s as _)),
          (AttribValue::Byte(b), TypeSpecifierNonArray::IVec4, _) => Value::IVec4(map4(b, |b| b as _)),
          (AttribValue::Byte(b), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(b, |b| b as _)),
          (AttribValue::Byte(b), TypeSpecifierNonArray::Vec4, true) => Value::Vec4(map4(b, normalize_i8)),
          (AttribValue::Ubyte(b), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(b, |b| b as _)),
          (AttribValue::Ubyte(b), TypeSpecifierNonArray::Vec4, true) => Value::Vec4(map4(b, normalize_u8)),
          (AttribValue::Ubyte(b), TypeSpecifierNonArray::UVec4, _) => Value::UVec4(map4(b, |b| b as _)),
          (AttribValue::I2_10_10_10(i), TypeSpecifierNonArray::Vec4, false) => Value::Vec4(map4(i, |i| i as _)),
          (AttribValue::I2_10_10_10(i), TypeSpecifierNonArray::Vec4, true) => Value::Vec4([
            normalize_i10(i[0]),
            normalize_i10(i[1]),
            normalize_i10(i[2]),
            normalize_i2(i[3]),
          ]),
          (AttribValue::U2_10_10_10(u), TypeSpecifierNonArray::Vec4, true) => Value::Vec4([
            normalize_u10(u[0]),
            normalize_u10(u[1]),
            normalize_u10(u[2]),
            normalize_u2(u[3]),
          ]),
          (v, t, n) => unimplemented!("{:?} {:?} {}", v, t, n),
        };

        match (intermediate, type_) {
          (Value::Vec4(f), TypeSpecifierNonArray::Float) => Value::Float(f[0]),
          (Value::Vec4(f), TypeSpecifierNonArray::Vec2) => Value::Vec2([f[0], f[1]]),
          (Value::Vec4(f), TypeSpecifierNonArray::Vec3) => Value::Vec3([f[0], f[1], f[2]]),
          (Value::Vec4(f), TypeSpecifierNonArray::Vec4) => Value::Vec4([f[0], f[1], f[2], f[3]]),
          (Value::IVec4(i), TypeSpecifierNonArray::Int) => Value::Int(i[0]),
          (Value::IVec4(i), TypeSpecifierNonArray::IVec2) => Value::IVec2([i[0], i[1]]),
          (Value::IVec4(i), TypeSpecifierNonArray::IVec3) => Value::IVec3([i[0], i[1], i[2]]),
          (Value::IVec4(i), TypeSpecifierNonArray::IVec4) => Value::IVec4([i[0], i[1], i[2], i[3]]),
          (Value::UVec4(u), TypeSpecifierNonArray::Uint) => Value::Uint(u[0]),
          (Value::UVec4(u), TypeSpecifierNonArray::UVec2) => Value::UVec2([u[0], u[1]]),
          (Value::UVec4(u), TypeSpecifierNonArray::UVec3) => Value::UVec3([u[0], u[1], u[2]]),
          (Value::UVec4(u), TypeSpecifierNonArray::UVec4) => Value::UVec4([u[0], u[1], u[2], u[3]]),
          (i, t) => unimplemented!("{:?} {:?}", i, t),
        }
      } else {
        let attrib = current_vertex_attrib[loc as usize];

        let val = match type_ {
          TypeSpecifierNonArray::Float => Value::Float(attrib[0]),
          TypeSpecifierNonArray::Vec2 => Value::Vec2([attrib[0], attrib[1]]),
          TypeSpecifierNonArray::Vec3 => Value::Vec3([attrib[0], attrib[1], attrib[2]]),
          TypeSpecifierNonArray::Vec4 => Value::Vec4([attrib[0], attrib[1], attrib[2], attrib[3]]),
          TypeSpecifierNonArray::Int => Value::Int(attrib[0] as _),
          TypeSpecifierNonArray::IVec2 => Value::IVec2([attrib[0] as _, attrib[1] as _]),
          TypeSpecifierNonArray::IVec3 => Value::IVec3([attrib[0] as _, attrib[1] as _, attrib[2] as _]),
          TypeSpecifierNonArray::IVec4 => Value::IVec4([attrib[0] as _, attrib[1] as _, attrib[2] as _, attrib[3] as _]),
          TypeSpecifierNonArray::Uint => Value::Uint(attrib[0] as _),
          TypeSpecifierNonArray::UVec2 => Value::UVec2([attrib[0] as _, attrib[1] as _]),
          TypeSpecifierNonArray::UVec3 => Value::UVec3([attrib[0] as _, attrib[1] as _, attrib[2] as _]),
          TypeSpecifierNonArray::UVec4 => Value::UVec4([attrib[0] as _, attrib[1] as _, attrib[2] as _, attrib[3] as _]),
          t => unimplemented!("{:?}", t),
        };

        val
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

    fn ndc(vars: &Vars) -> [f32; 3] {
      let clip_coords = if let &Value::Vec4(ref v) = vars.get(&"gl_Position".into()) {
        v
      } else { unreachable!() };
      [clip_coords[0] / clip_coords[3],
       clip_coords[1] / clip_coords[3],
       clip_coords[2] / clip_coords[3]]
    }

    fn window_coords(ndc: [f32; 3], viewport: Rect, depth_range: (GLfloat, GLfloat)) -> [f32; 3] {
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
      front_face: bool,
      mut vert_vars: Vars,
      frag_in_vars: &Vec<glsl::Variable>,
      frag_out_vars: &Vec<glsl::Variable>,
      frag_uniforms: &HashMap<Atom, Value>,
      frag_compiled: &Arc<glsl::Shader>,
    ) {
      let mut draw_surface = unsafe{ context.draw_surface.as_mut() };
      let draw_surface = draw_surface.as_mut().unwrap();

      // TODO: real clipping
      if x < context.viewport.0
        || x >= context.viewport.0+context.viewport.2
        || y < context.viewport.1
        || y >= context.viewport.1+context.viewport.3 {
          return;
        }

      let draw_framebuffer = context.draw_framebuffer;
      let color_mask = context.color_mask;

      let stencil_val = draw_surface.get_stencil(x, y);

      let (stencil_func, stencil_ref, stencil_mask) = if front_face { context.stencil_func_front } else { context.stencil_func_back };
      let (sfail, dpfail, dppass) = if front_face { context.stencil_op_front } else { context.stencil_op_back };
      let stencil_write_mask = if front_face { context.stencil_mask_front } else { context.stencil_mask_back };

      let stencil_max = draw_surface.stencil_max();
      let stencil_ref = cmp::min(stencil_ref, stencil_max);
      let stencil_mask = stencil_mask & stencil_max;

      fn set_stencil(
        draw_surface: &mut ::egl::Surface,
        (x, y): (i32, i32),
        op: GLenum,
        val: u32,
        ref_: u32,
        stencil_max: u32,
        stencil_write_mask: u32,
      ) {
          if op != GL_KEEP {
            let new_val = match op {
              GL_REPLACE => ref_,
              GL_INCR => if val == stencil_max { val } else { val + 1 },
              GL_DECR => if val == 0 { 0 } else { val - 1 },
              GL_INCR_WRAP => val.wrapping_add(1) & stencil_max,
              GL_DECR_WRAP => val.wrapping_sub(1) & stencil_max,
              GL_ZERO => 0,
              GL_INVERT => !val,
              x => unimplemented!("{:x}", x),
            };

            draw_surface.set_stencil(x, y, new_val, stencil_write_mask);
          }

      }

      if context.stencil_test {
        if !stencil_func.cmp(stencil_val & stencil_mask, stencil_ref & stencil_mask) {
          set_stencil(draw_surface, (x, y), sfail, stencil_val, stencil_ref, stencil_max, stencil_write_mask);

          return;
        }
      }

      if context.depth_test {
        let old_z = draw_surface.get_depth(x, y);

        let z = draw_surface.encode_depth(z);

        if !context.depth_func.cmp(old_z, z) {
          if context.stencil_test {
            set_stencil(draw_surface, (x, y), dpfail, stencil_val, stencil_ref, stencil_max, stencil_write_mask);
          }

          return;
        }

        if context.depth_mask {
          draw_surface.set_depth(x, y, z);
        }
      }

      if context.stencil_test {
        set_stencil(draw_surface, (x, y), dppass, stencil_val, stencil_ref, stencil_max, stencil_write_mask);
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
    }

    match prim {
      Primitive::Point(p) => {
        let ndc = ndc(&p);

        if ndc[2] < -1.0 || ndc[2] > 1.0 {
          continue;
        }

        let window_coords = window_coords(ndc, current.viewport, current.depth_range);

        let point_size = if let &Value::Float(f) = p.get(&"gl_PointSize".into()) {
          f
        } else { unreachable!() };
        let point_size = if point_size <= 0.0 { 1.0 } else { point_size };

        let min_x = window_coords[0] - point_size / 2.0;
        let max_x = window_coords[0] + point_size / 2.0;
        let min_y = window_coords[1] - point_size / 2.0;
        let max_y = window_coords[1] + point_size / 2.0;

        let mut x = (min_x + 0.5).ceil() - 0.5;
        let first_y = (min_y + 0.5).ceil() - 0.5;

        while x < max_x {
          let mut y = first_y;

          while y < max_y {
            do_fragment(
              current,
              (x as i32, y as i32),
              window_coords[2],
              true,
              p.clone(),
              &frag_in_vars,
              &frag_out_vars,
              &frag_uniforms,
              &frag_compiled,
            );

            y += 1.0;
          }

          x += 1.0;
        }
      },
      Primitive::Line(a, b) => {
        let ndc_a = ndc(&a);
        let p_a = window_coords(ndc_a, current.viewport, current.depth_range);
        let ndc_b = ndc(&b);
        let p_b = window_coords(ndc_b, current.viewport, current.depth_range);

        let delta_x = p_b[0]-p_a[0];
        let delta_y = p_b[1]-p_a[1];
        let slope = delta_y / delta_x;

        let x_major = slope >= -1.0 && slope <= 1.0;

        let width = current.line_width.round().max(1.0);
        let offset = (width - 1.0) / 2.0;
        let width = width as i32;

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

          y -= offset;

          if p_a[0] < p_b[0] {
            let first_hop = 1.0 - (x + 0.5).fract();
            y += slope * first_hop;
            x += first_hop;
            t -= idx * first_hop;

            while x < p_b[0] {
              let frag_vals = interp_vars(&frag_in_vars, &a, &b, t);
              let z = lerp(p_a[2], p_b[2], t);

              for dy in 0..width {
                do_fragment(
                  current,
                  (x.floor() as i32, y.floor() as i32 + dy),
                  z,
                  true,
                  frag_vals.clone(),
                  &frag_in_vars,
                  &frag_out_vars,
                  &frag_uniforms,
                  &frag_compiled,
                );
              }

              y += slope;
              x += 1.0;
              t -= idx;
            }
          } else {
            let first_hop = 1.0 - (x - 0.5).fract();
            y -= slope * first_hop;
            x -= first_hop;
            t -= idx * first_hop;

            while x > p_b[0] {
              let frag_vals = interp_vars(&frag_in_vars, &a, &b, t);
              let z = lerp(p_a[2], p_b[2], t);

              for dy in 0..width {
                do_fragment(
                  current,
                  (x.floor() as i32, y.floor() as i32 + dy),
                  z,
                  true,
                  frag_vals.clone(),
                  &frag_in_vars,
                  &frag_out_vars,
                  &frag_uniforms,
                  &frag_compiled,
                );
              }

              y -= slope;
              x -= 1.0;
              t -= idx;
            }
          }
        } else {
          x -= offset;

          let idy = 1.0 / delta_y.abs();
          let islope = delta_x / delta_y;

          if p_a[1] < p_b[1] {
            let first_hop = 1.0 - (y + 0.5).fract();
            x += islope * first_hop;
            y += first_hop;
            t -= idy * first_hop;

            while y < p_b[1] {
              let frag_vals = interp_vars(&frag_in_vars, &a, &b, t);
              let z = lerp(p_a[2], p_b[2], t);

              for dx in 0..width {
                do_fragment(
                  current,
                  (x.floor() as i32 + dx, y.floor() as i32),
                  z,
                  true,
                  frag_vals.clone(),
                  &frag_in_vars,
                  &frag_out_vars,
                  &frag_uniforms,
                  &frag_compiled,
                );
              }

              x += islope;
              y += 1.0;
              t -= idy;
            }
          } else {
            let first_hop = 1.0 - (y - 0.5).fract();
            x -= islope * first_hop;
            y -= first_hop;
            t -= idy * first_hop;

            while y > p_b[1] {
              let frag_vals = interp_vars(&frag_in_vars, &a, &b, t);
              let z = lerp(p_a[2], p_b[2], t);

              for dx in 0..width {
                do_fragment(
                  current,
                  (x.floor() as i32 + dx, y.floor() as i32),
                  z,
                  true,
                  frag_vals.clone(),
                  &frag_in_vars,
                  &frag_out_vars,
                  &frag_uniforms,
                  &frag_compiled,
                );
              }

              x -= islope;
              y -= 1.0;
              t -= idy;
            }
          }
        }
      },
      Primitive::Triangle(a, b, c) => {
        let ndc_a = ndc(&a);
        let p_a = window_coords(ndc_a, current.viewport, current.depth_range);
        let ndc_b = ndc(&b);
        let p_b = window_coords(ndc_b, current.viewport, current.depth_range);
        let ndc_c = ndc(&c);
        let p_c = window_coords(ndc_c, current.viewport, current.depth_range);

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


        let first_hop = 1.0 - ((x + 0.5).fract() + 1.0).fract();

        if x == middle[0] {
          y_lmr = middle[1];
        }

        if x + first_hop > middle[0] {
          let hop_to_mid = middle[0] - x;
          let hop_from_mid = first_hop - hop_to_mid;
          if slope_lm.is_infinite() {
            y_lmr += slope_mr * hop_from_mid;
          } else {
            y_lmr += slope_lm * hop_to_mid + slope_mr * hop_from_mid;
          }
        } else {
          y_lmr += first_hop * slope_lm;
        }
        x += first_hop;
        y_lr += first_hop * slope_lr;

        while x < right[0] {
          let (bottom, top) = if lr_top { (y_lmr, y_lr) } else { (y_lr, y_lmr) };
          let mut y = bottom.round() + 0.5;
          while y < top.round() {
            let barys = barycentric(x, y, p_a, p_b, p_c);

            let frag_vals = bary_interp_vars(&frag_in_vars, &a, &b, &c, barys);

            let z = p_a[2] * barys.0
              + p_b[2] * barys.1
              + p_c[2] * barys.2;

            do_fragment(
              current,
              (x.floor() as i32, y.floor() as i32),
              z,
              front_facing,
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

#[inline]
fn barycentric(x: f32, y: f32, a: [f32; 3], b: [f32; 3], c: [f32; 3]) -> (f32, f32, f32) {
  let [x1, y1, _] = a;
  let [x2, y2, _] = b;
  let [x3, y3, _] = c;

  let denom = (y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3);

  let mut bary_a = ((y2 - y3)*(x - x3) + (x3 - x2)*(y - y3)) / denom;
  let bary_b = ((y3 - y1)*(x - x3) + (x1 - x3)*(y - y3)) / denom;
  let bary_c = 1.0 - bary_a - bary_b;

  // TODO: figure out a better way to deal with this precicion issue
  if (bary_a + bary_b + bary_c) != 1.0 {
    bary_a = 1.0 - bary_b - bary_c;
  }

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
