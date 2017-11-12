use std::collections::VecDeque;

use super::{Version,Result};

#[derive(PartialEq,Debug)]
pub struct FullToken {
  pub line: usize,
  pub col: usize,
  pub typ: Token,
}

#[allow(dead_code)]
#[derive(PartialEq,Debug)]
pub enum Token {
  // Primitives
  Void,
  Bool,
  Float,
  Int,
  Uint,
  // Vectors
  BVec2,
  BVec3,
  BVec4,
  IVec2,
  IVec3,
  IVec4,
  UVec2,
  UVec3,
  UVec4,
  Vec2,
  Vec3,
  Vec4,
  // Matrices
  Mat2,
  Mat3,
  Mat4,
  Mat2x2,
  Mat2x3,
  Mat2x4,
  Mat3x2,
  Mat3x3,
  Mat3x4,
  Mat4x2,
  Mat4x3,
  Mat4x4,
  // Misc types
  Struct,
  AtomicUint,
  // Samplers
  Sampler2D,
  Sampler3d,
  SamplerCube,
  Sampler2DArray,
  Sampler2DShadow,
  SamplerCubeShadow,
  Sampler2DArrayShadow,
  ISampler2D,
  ISampler3d,
  ISamplerCube,
  ISampler2DArray,
  USampler2D,
  USampler3d,
  USamplerCube,
  USampler2DArray,
  Sampler2DMS,
  ISampler2DMS,
  USampler2DMS,
  Sampler2DMSArray,
  ISampler2DMSArray,
  USampler2DMSArray,
  SamplerBuffer,
  ISamplerBuffer,
  USamplerBuffer,
  SamplerCubeArray,
  ISamplerCubeArray,
  USamplerCubeArray,
  SamplerCubeArrayShadow,
  // Images
  Image2D,
  IImage2D,
  UImage2D,
  Image3d,
  IImage3d,
  UImage3d,
  ImageCube,
  IImageCube,
  UImageCube,
  Image2DArray,
  IImage2DArray,
  UImage2DArray,
  ImageBuffer,
  IImageBuffer,
  UImageBuffer,
  UImageCubeArray,
  ImageCubeArray,
  IImageCubeArray,
  // Precision
  Invariant,
  HighPrecision,
  MediumPrecision,
  LowPrecision,
  Precision,
  // Storage qualifiers
  In,
  Out,
  Inout,
  Const,
  Uniform,
  Buffer,
  Shared,
  Coherent,
  Volatile,
  Restrict,
  Readonly,
  Writeonly,
  Flat,
  Smooth,
  Centroid,
  Layout,
  Patch,
  Sample,
  // Control flow
  While,
  Break,
  Continue,
  Do,
  Else,
  For,
  If,
  Discard,
  Return,
  Switch,
  Case,
  Default,
  // Literals
  Ident(String),
  FloatConstant(f32),
  IntConstant(i32),
  UintConstant(u32),
  BoolConstant(bool),
  // Operators
  FieldSelection, // TODO
  LeftOp,
  RightOp,
  IncOp,
  DecOp,
  LeOp,
  GeOp,
  EqOp,
  NeOp,
  AndOp,
  OrOp,
  XorOp,
  MulAssign,
  DivAssign,
  AddAssign,
  ModAssign,
  LeftAssign,
  RightAssign,
  AndAssign,
  XorAssign,
  OrAssign,
  SubAssign,
  // Symbols
  OpenParen,
  CloseParen,
  OpenBracket,
  CloseBracket,
  OpenBrace,
  CloseBrace,
  Dot,
  Comma,
  Colon,
  Equal,
  Semicolon,
  Bang,
  Dash,
  Tilde,
  Plus,
  Star,
  Slash,
  Percent,
  OpenAngle,
  CloseAngle,
  VerticalBar,
  Caret,
  Ampersand,
  Question,
}

impl Token {
  pub fn is_builtin_type(&self) -> bool {
    match *self {
      Token::Void
        | Token::Bool
        | Token::Float
        | Token::Int
        | Token::Uint
        | Token::BVec2
        | Token::BVec3
        | Token::BVec4
        | Token::IVec2
        | Token::IVec3
        | Token::IVec4
        | Token::UVec2
        | Token::UVec3
        | Token::UVec4
        | Token::Vec2
        | Token::Vec3
        | Token::Vec4
        | Token::Mat2
        | Token::Mat3
        | Token::Mat4
        | Token::Mat2x2
        | Token::Mat2x3
        | Token::Mat2x4
        | Token::Mat3x2
        | Token::Mat3x3
        | Token::Mat3x4
        | Token::Mat4x2
        | Token::Mat4x3
        | Token::Mat4x4
        | Token::Struct
        | Token::AtomicUint
        | Token::Sampler2D
        | Token::Sampler3d
        | Token::SamplerCube
        | Token::Sampler2DArray
        | Token::Sampler2DShadow
        | Token::SamplerCubeShadow
        | Token::Sampler2DArrayShadow
        | Token::ISampler2D
        | Token::ISampler3d
        | Token::ISamplerCube
        | Token::ISampler2DArray
        | Token::USampler2D
        | Token::USampler3d
        | Token::USamplerCube
        | Token::USampler2DArray
        | Token::Sampler2DMS
        | Token::ISampler2DMS
        | Token::USampler2DMS
        | Token::Sampler2DMSArray
        | Token::ISampler2DMSArray
        | Token::USampler2DMSArray
        | Token::SamplerBuffer
        | Token::ISamplerBuffer
        | Token::USamplerBuffer
        | Token::SamplerCubeArray
        | Token::ISamplerCubeArray
        | Token::USamplerCubeArray
        | Token::SamplerCubeArrayShadow
        | Token::Image2D
        | Token::IImage2D
        | Token::UImage2D
        | Token::Image3d
        | Token::IImage3d
        | Token::UImage3d
        | Token::ImageCube
        | Token::IImageCube
        | Token::UImageCube
        | Token::Image2DArray
        | Token::IImage2DArray
        | Token::UImage2DArray
        | Token::ImageBuffer
        | Token::IImageBuffer
        | Token::UImageBuffer
        | Token::UImageCubeArray
        | Token::ImageCubeArray
        | Token::IImageCubeArray => true,
      _ => false,
    }
  }
}

pub(super) fn version(source: &str) -> Result<Version> {
  let mut l = LexHelper::new(source);

  l.whitespace();
  if l.next() != Some('#') { return Ok(Version::ES100); }
  l.whitespace();
  if l.until(char::is_whitespace) != "version" { return Ok(Version::ES100); }
  l.whitespace();
  let number = l.until(char::is_whitespace);
  l.whitespace();
  let es = l.until(char::is_whitespace);
  let junk = l.until(|c| c == '\n');
  if !junk.chars().all(char::is_whitespace) { return Err(format!("There's junk on the version line: {}", junk)) }

  match (number.as_str(), es.as_str()) {
    ("310", "es") => Ok(Version::ES310),
    ("320", "es") => Ok(Version::ES320),
    (n, e) => Err(format!("Unknown version: {} {}", n, e)),
  }
}

pub(super) fn tokenize(source: &str, version: Version) -> Result<Vec<FullToken>> {
  let mut l = LexHelper::new(source);
  if version != Version::ES100 {
    l.until(|c| c == '\n');
  }
  l.whitespace();

  let mut result = vec![];

  while let Some(next) = l.peek() {
    let (line, col) = l.loc();
    let typ = match next {
      n if n.is_alphabetic() => {
        let string = l.take(|c| c.is_alphanumeric() || c == '_');
        match string.as_ref() {
          "void" => Some(Token::Void),
          "bool" => Some(Token::Bool),
          "float" => Some(Token::Float),
          "int" => Some(Token::Int),
          "uint" => Some(Token::Uint),
          "bvec2" => Some(Token::BVec2),
          "bvec3" => Some(Token::BVec3),
          "bvec4" => Some(Token::BVec4),
          "ivec2" => Some(Token::IVec2),
          "ivec3" => Some(Token::IVec3),
          "ivec4" => Some(Token::IVec4),
          "uvec2" => Some(Token::UVec2),
          "uvec3" => Some(Token::UVec3),
          "uvec4" => Some(Token::UVec4),
          "vec2" => Some(Token::Vec2),
          "vec3" => Some(Token::Vec3),
          "vec4" => Some(Token::Vec4),
          "mat2" => Some(Token::Mat2),
          "mat3" => Some(Token::Mat3),
          "mat4" => Some(Token::Mat4),
          "mat2x2" => Some(Token::Mat2x2),
          "mat2x3" => Some(Token::Mat2x3),
          "mat2x4" => Some(Token::Mat2x4),
          "mat3x2" => Some(Token::Mat3x2),
          "mat3x3" => Some(Token::Mat3x3),
          "mat3x4" => Some(Token::Mat3x4),
          "mat4x2" => Some(Token::Mat4x2),
          "mat4x3" => Some(Token::Mat4x3),
          "mat4x4" => Some(Token::Mat4x4),
          "struct" => Some(Token::Struct),
          "atomic_uint" => Some(Token::AtomicUint),
          "sampler2D" => Some(Token::Sampler2D),
          "sampler3D" => Some(Token::Sampler3d),
          "samplerCube" => Some(Token::SamplerCube),
          "sampler2DArray" => Some(Token::Sampler2DArray),
          "sampler2DShadow" => Some(Token::Sampler2DShadow),
          "samplerCubeShadow" => Some(Token::SamplerCubeShadow),
          "sampler2DArrayShadow" => Some(Token::Sampler2DArrayShadow),
          "isampler2D" => Some(Token::ISampler2D),
          "isampler3D" => Some(Token::ISampler3d),
          "isamplerCube" => Some(Token::ISamplerCube),
          "isampler2DArray" => Some(Token::ISampler2DArray),
          "usampler2D" => Some(Token::USampler2D),
          "usampler3D" => Some(Token::USampler3d),
          "usamplerCube" => Some(Token::USamplerCube),
          "usampler2DArray" => Some(Token::USampler2DArray),
          "sampler2DMS" => Some(Token::Sampler2DMS),
          "isampler2DMS" => Some(Token::ISampler2DMS),
          "usampler2DMS" => Some(Token::USampler2DMS),
          "sampler2DMSArray" => Some(Token::Sampler2DMSArray),
          "isampler2DMSArray" => Some(Token::ISampler2DMSArray),
          "usampler2DMSArray" => Some(Token::USampler2DMSArray),
          "samplerBuffer" => Some(Token::SamplerBuffer),
          "isamplerBuffer" => Some(Token::ISamplerBuffer),
          "usamplerBuffer" => Some(Token::USamplerBuffer),
          "samplerCubeArray" => Some(Token::SamplerCubeArray),
          "isamplerCubeArray" => Some(Token::ISamplerCubeArray),
          "usamplerCubeArray" => Some(Token::USamplerCubeArray),
          "samplerCubeArrayShadow" => Some(Token::SamplerCubeArrayShadow),
          "image2D" => Some(Token::Image2D),
          "iimage2D" => Some(Token::IImage2D),
          "uimage2D" => Some(Token::UImage2D),
          "image3D" => Some(Token::Image3d),
          "iimage3D" => Some(Token::IImage3d),
          "uimage3D" => Some(Token::UImage3d),
          "imageCube" => Some(Token::ImageCube),
          "iimageCube" => Some(Token::IImageCube),
          "uimageCube" => Some(Token::UImageCube),
          "image2DArray" => Some(Token::Image2DArray),
          "iimage2DArray" => Some(Token::IImage2DArray),
          "uimage2DArray" => Some(Token::UImage2DArray),
          "imageBuffer" => Some(Token::ImageBuffer),
          "iimageBuffer" => Some(Token::IImageBuffer),
          "uimageBuffer" => Some(Token::UImageBuffer),
          "uimageCubeArray" => Some(Token::UImageCubeArray),
          "imageCubeArray" => Some(Token::ImageCubeArray),
          "iimageCubeArray" => Some(Token::IImageCubeArray),
          "invariant" => Some(Token::Invariant),
          "highp" => Some(Token::HighPrecision),
          "mediump" => Some(Token::MediumPrecision),
          "lowp" => Some(Token::LowPrecision),
          "precision" => Some(Token::Precision),
          "in" => Some(Token::In),
          "out" => Some(Token::Out),
          "inout" => Some(Token::Inout),
          "const" => Some(Token::Const),
          "uniform" => Some(Token::Uniform),
          "buffer" => Some(Token::Buffer),
          "shared" => Some(Token::Shared),
          "coherent" => Some(Token::Coherent),
          "volatile" => Some(Token::Volatile),
          "restrict" => Some(Token::Restrict),
          "readonly" => Some(Token::Readonly),
          "writeonly" => Some(Token::Writeonly),
          "flat" => Some(Token::Flat),
          "smooth" => Some(Token::Smooth),
          "centroid" => Some(Token::Centroid),
          "layout" => Some(Token::Layout),
          "patch" => Some(Token::Patch),
          "sample" => Some(Token::Sample),
          "while" => Some(Token::While),
          "break" => Some(Token::Break),
          "continue" => Some(Token::Continue),
          "do" => Some(Token::Do),
          "else" => Some(Token::Else),
          "for" => Some(Token::For),
          "if" => Some(Token::If),
          "discard" => Some(Token::Discard),
          "return" => Some(Token::Return),
          "switch" => Some(Token::Switch),
          "case" => Some(Token::Case),
          "default" => Some(Token::Default),
          _ => None,
        }.unwrap_or(Token::Ident(string)) // TODO: cleanup with NLL
      },
      n if n.is_digit(10) => {
        let string = l.take(|c| c.is_digit(10) || c == '.');

        if string.contains('.') {
          let n = string.parse::<f32>().map_err(|_| format!("Failed to parse {} as a float", string))?;
          Token::FloatConstant(n)
        } else if l.peek() == Some('u') {
          l.advance();
          let n = string.parse::<u32>().map_err(|_| format!("Failed to parse {} as a uint", string))?;
          Token::UintConstant(n)
        } else {
          let n = string.parse::<i32>().map_err(|_| format!("Failed to parse {} as an int", string))?;
          Token::IntConstant(n)
        }
      },
      '(' => { l.advance(); Token::OpenParen },
      ')' => { l.advance(); Token::CloseParen },
      '{' => { l.advance(); Token::OpenBrace },
      '}' => { l.advance(); Token::CloseBrace },
      '=' => {
        l.advance();
        if l.peek() == Some('=') {
          l.advance();
          Token::EqOp
        } else {
          Token::Equal
        }
      },
      ';' => { l.advance(); Token::Semicolon },
      ',' => { l.advance(); Token::Comma },
      '[' => { l.advance(); Token::OpenBracket },
      ']' => { l.advance(); Token::CloseBracket },
      '*' => { l.advance(); Token::Star },
      '.' => { l.advance(); Token::Dot },
      ':' => { l.advance(); Token::Colon },
      '!' => { l.advance(); Token::Bang },
      '-' => { l.advance(); Token::Dash },
      '~' => { l.advance(); Token::Tilde },
      '+' => {
        l.advance();
        if l.peek() == Some('+') {
          l.advance();
          Token::IncOp
        } else if l.peek() == Some('=') {
          l.advance();
          Token::AddAssign
        } else {
          Token::Plus
        }
      },
      '/' => { l.advance(); Token::Slash },
      '%' => { l.advance(); Token::Percent },
      '<' => { l.advance(); Token::OpenAngle },
      '>' => { l.advance(); Token::CloseAngle },
      '|' => { l.advance(); Token::VerticalBar },
      '^' => { l.advance(); Token::Caret },
      '&' => { l.advance(); Token::Ampersand },
      '?' => { l.advance(); Token::Question },

      x => return Err(format!("Don't know how to tokenize {} at {}:{}", x, line, col)),
    };

    let token = FullToken{
      line,
      col,
      typ,
    };

    result.push(token);
    l.whitespace();
  }

  Ok(result)
}


struct LexHelper<'a> {
  c: ::std::str::Chars<'a>,
  line: usize,
  col: usize,
  buf: VecDeque<char>,
}

impl<'a> LexHelper<'a> {
  fn new(s: &'a str) -> Self {
    Self{
      c: s.chars(),
      line: 1,
      col: 1,
      buf: VecDeque::new(),
    }
  }

  fn whitespace(&mut self) {
    while let Some(next) = self.peek() {
      if next.is_whitespace() {
        self.advance();
      } else { break }
    }
  }

  fn take<P>(&mut self, p: P) -> String
    where P: Fn(char) -> bool {

    let mut result = String::new();
    while let Some(next) = self.peek() {
      if !p(next) { break; }
      self.advance();
      result.push(next);
    }

    result
  }


  fn until<P>(&mut self, p: P) -> String
    where P: Fn(char) -> bool {
    self.take(|c| !p(c))
  }

  fn next(&mut self) -> Option<char> {
    let next = self.peek();
    if next.is_some() { self.advance(); }
    next
  }

  fn peek(&mut self) -> Option<char> {
    if let Some(&next) = self.buf.front() {
      Some(next)
    } else if let Some(next) = self.c.next() {
      self.buf.push_back(next);
      Some(next)
    } else { None }
  }

  // TODO: a `consume` (like the parser) would be nice
  fn advance(&mut self) {
    if let Some(next) = self.buf.pop_front() {
      if next == '\n' {
        self.line += 1;
        self.col = 0;
      } else {
        self.col += 1;
      }
    } else {
      panic!("Not yet sure how this would happen");
    }
  }

  fn loc(&self) -> (usize, usize) {
    (self.line, self.col)
  }
}


#[cfg(test)]
#[test]
fn test_version() {
  let source = r"#version 310 es
layout (local_size_x = 1) in;
void main (void) {}
";

  let version = version(source).unwrap();

  assert_eq!(version, Version::ES310);
}

#[cfg(test)]
#[test]
fn test_tokenize() {
  use self::Token::*;

  let source = r"#version 310 es
layout (local_size_x = 1) in;
void main (void) {}
";

  let version = version(source).unwrap();
  let tokens = tokenize(source, version).unwrap();

  assert_eq!(tokens,
             vec![
               FullToken{ line: 2, col: 0,  typ: Layout },
               FullToken{ line: 2, col: 7,  typ: OpenParen },
               FullToken{ line: 2, col: 8,  typ: Ident("local_size_x".to_string()) },
               FullToken{ line: 2, col: 21, typ: Equal },
               FullToken{ line: 2, col: 23, typ: IntConstant(1) },
               FullToken{ line: 2, col: 24, typ: CloseParen },
               FullToken{ line: 2, col: 26, typ: In },
               FullToken{ line: 2, col: 28, typ: Semicolon },
               FullToken{ line: 3, col: 0,  typ: Void },
               FullToken{ line: 3, col: 5,  typ: Ident("main".to_string()) },
               FullToken{ line: 3, col: 10, typ: OpenParen },
               FullToken{ line: 3, col: 11, typ: Void },
               FullToken{ line: 3, col: 15, typ: CloseParen },
               FullToken{ line: 3, col: 17, typ: OpenBrace },
               FullToken{ line: 3, col: 18, typ: CloseBrace },
             ]);
}
