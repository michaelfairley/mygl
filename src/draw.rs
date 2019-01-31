use ::gl::*;
use std::mem;

use glsl::interpret::{Vars};

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
  TriangleStrip(Option<Vars>, Option<Vars>, Option<Vars>),
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
      GL_TRIANGLE_STRIP => Mode::TriangleStrip(None, None, None),
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
      Mode::TriangleStrip(ref mut a, ref mut b, ref mut c) => {
        if a.is_none() {
          *a = self.vertex_iter.next();
          *b = self.vertex_iter.next();
          *c = self.vertex_iter.next();
        } else {
          *a = mem::replace(b, mem::replace(c, self.vertex_iter.next()));
        }

        if let (&mut Some(ref a), &mut Some(ref b), &mut Some(ref c)) = (a, b, c) {
          Some(Primitive::Triangle(a, b, c))
        } else { None }
      },
    }
  }
}
