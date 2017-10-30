use std::collections::VecDeque;

use super::{Version,Result};

#[derive(PartialEq,Debug)]
pub struct Token {
  pub line: usize,
  pub col: usize,
  pub typ: TokenType,
}

#[allow(dead_code)]
#[derive(PartialEq,Debug)]
pub enum TokenType {
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

impl TokenType {
  pub fn is_builtin_type(&self) -> bool {
    match *self {
      TokenType::Void
        | TokenType::Bool
        | TokenType::Float
        | TokenType::Int
        | TokenType::Uint
        | TokenType::BVec2
        | TokenType::BVec3
        | TokenType::BVec4
        | TokenType::IVec2
        | TokenType::IVec3
        | TokenType::IVec4
        | TokenType::UVec2
        | TokenType::UVec3
        | TokenType::UVec4
        | TokenType::Vec2
        | TokenType::Vec3
        | TokenType::Vec4
        | TokenType::Mat2
        | TokenType::Mat3
        | TokenType::Mat4
        | TokenType::Mat2x2
        | TokenType::Mat2x3
        | TokenType::Mat2x4
        | TokenType::Mat3x2
        | TokenType::Mat3x3
        | TokenType::Mat3x4
        | TokenType::Mat4x2
        | TokenType::Mat4x3
        | TokenType::Mat4x4
        | TokenType::Struct
        | TokenType::AtomicUint
        | TokenType::Sampler2D
        | TokenType::Sampler3d
        | TokenType::SamplerCube
        | TokenType::Sampler2DArray
        | TokenType::Sampler2DShadow
        | TokenType::SamplerCubeShadow
        | TokenType::Sampler2DArrayShadow
        | TokenType::ISampler2D
        | TokenType::ISampler3d
        | TokenType::ISamplerCube
        | TokenType::ISampler2DArray
        | TokenType::USampler2D
        | TokenType::USampler3d
        | TokenType::USamplerCube
        | TokenType::USampler2DArray
        | TokenType::Sampler2DMS
        | TokenType::ISampler2DMS
        | TokenType::USampler2DMS
        | TokenType::Sampler2DMSArray
        | TokenType::ISampler2DMSArray
        | TokenType::USampler2DMSArray
        | TokenType::SamplerBuffer
        | TokenType::ISamplerBuffer
        | TokenType::USamplerBuffer
        | TokenType::SamplerCubeArray
        | TokenType::ISamplerCubeArray
        | TokenType::USamplerCubeArray
        | TokenType::SamplerCubeArrayShadow
        | TokenType::Image2D
        | TokenType::IImage2D
        | TokenType::UImage2D
        | TokenType::Image3d
        | TokenType::IImage3d
        | TokenType::UImage3d
        | TokenType::ImageCube
        | TokenType::IImageCube
        | TokenType::UImageCube
        | TokenType::Image2DArray
        | TokenType::IImage2DArray
        | TokenType::UImage2DArray
        | TokenType::ImageBuffer
        | TokenType::IImageBuffer
        | TokenType::UImageBuffer
        | TokenType::UImageCubeArray
        | TokenType::ImageCubeArray
        | TokenType::IImageCubeArray => true,
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

pub(super) fn tokenize(source: &str, version: Version) -> Result<Vec<Token>> {
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
          "void" => Some(TokenType::Void),
          "bool" => Some(TokenType::Bool),
          "float" => Some(TokenType::Float),
          "int" => Some(TokenType::Int),
          "uint" => Some(TokenType::Uint),
          "bvec2" => Some(TokenType::BVec2),
          "bvec3" => Some(TokenType::BVec3),
          "bvec4" => Some(TokenType::BVec4),
          "ivec2" => Some(TokenType::IVec2),
          "ivec3" => Some(TokenType::IVec3),
          "ivec4" => Some(TokenType::IVec4),
          "uvec2" => Some(TokenType::UVec2),
          "uvec3" => Some(TokenType::UVec3),
          "uvec4" => Some(TokenType::UVec4),
          "vec2" => Some(TokenType::Vec2),
          "vec3" => Some(TokenType::Vec3),
          "vec4" => Some(TokenType::Vec4),
          "mat2" => Some(TokenType::Mat2),
          "mat3" => Some(TokenType::Mat3),
          "mat4" => Some(TokenType::Mat4),
          "mat2x2" => Some(TokenType::Mat2x2),
          "mat2x3" => Some(TokenType::Mat2x3),
          "mat2x4" => Some(TokenType::Mat2x4),
          "mat3x2" => Some(TokenType::Mat3x2),
          "mat3x3" => Some(TokenType::Mat3x3),
          "mat3x4" => Some(TokenType::Mat3x4),
          "mat4x2" => Some(TokenType::Mat4x2),
          "mat4x3" => Some(TokenType::Mat4x3),
          "mat4x4" => Some(TokenType::Mat4x4),
          "struct" => Some(TokenType::Struct),
          "atomic_uint" => Some(TokenType::AtomicUint),
          "sampler2D" => Some(TokenType::Sampler2D),
          "sampler3D" => Some(TokenType::Sampler3d),
          "samplerCube" => Some(TokenType::SamplerCube),
          "sampler2DArray" => Some(TokenType::Sampler2DArray),
          "sampler2DShadow" => Some(TokenType::Sampler2DShadow),
          "samplerCubeShadow" => Some(TokenType::SamplerCubeShadow),
          "sampler2DArrayShadow" => Some(TokenType::Sampler2DArrayShadow),
          "isampler2D" => Some(TokenType::ISampler2D),
          "isampler3D" => Some(TokenType::ISampler3d),
          "isamplerCube" => Some(TokenType::ISamplerCube),
          "isampler2DArray" => Some(TokenType::ISampler2DArray),
          "usampler2D" => Some(TokenType::USampler2D),
          "usampler3D" => Some(TokenType::USampler3d),
          "usamplerCube" => Some(TokenType::USamplerCube),
          "usampler2DArray" => Some(TokenType::USampler2DArray),
          "sampler2DMS" => Some(TokenType::Sampler2DMS),
          "isampler2DMS" => Some(TokenType::ISampler2DMS),
          "usampler2DMS" => Some(TokenType::USampler2DMS),
          "sampler2DMSArray" => Some(TokenType::Sampler2DMSArray),
          "isampler2DMSArray" => Some(TokenType::ISampler2DMSArray),
          "usampler2DMSArray" => Some(TokenType::USampler2DMSArray),
          "samplerBuffer" => Some(TokenType::SamplerBuffer),
          "isamplerBuffer" => Some(TokenType::ISamplerBuffer),
          "usamplerBuffer" => Some(TokenType::USamplerBuffer),
          "samplerCubeArray" => Some(TokenType::SamplerCubeArray),
          "isamplerCubeArray" => Some(TokenType::ISamplerCubeArray),
          "usamplerCubeArray" => Some(TokenType::USamplerCubeArray),
          "samplerCubeArrayShadow" => Some(TokenType::SamplerCubeArrayShadow),
          "image2D" => Some(TokenType::Image2D),
          "iimage2D" => Some(TokenType::IImage2D),
          "uimage2D" => Some(TokenType::UImage2D),
          "image3D" => Some(TokenType::Image3d),
          "iimage3D" => Some(TokenType::IImage3d),
          "uimage3D" => Some(TokenType::UImage3d),
          "imageCube" => Some(TokenType::ImageCube),
          "iimageCube" => Some(TokenType::IImageCube),
          "uimageCube" => Some(TokenType::UImageCube),
          "image2DArray" => Some(TokenType::Image2DArray),
          "iimage2DArray" => Some(TokenType::IImage2DArray),
          "uimage2DArray" => Some(TokenType::UImage2DArray),
          "imageBuffer" => Some(TokenType::ImageBuffer),
          "iimageBuffer" => Some(TokenType::IImageBuffer),
          "uimageBuffer" => Some(TokenType::UImageBuffer),
          "uimageCubeArray" => Some(TokenType::UImageCubeArray),
          "imageCubeArray" => Some(TokenType::ImageCubeArray),
          "iimageCubeArray" => Some(TokenType::IImageCubeArray),
          "invariant" => Some(TokenType::Invariant),
          "highprecision" => Some(TokenType::HighPrecision),
          "mediumprecision" => Some(TokenType::MediumPrecision),
          "lowprecision" => Some(TokenType::LowPrecision),
          "precision" => Some(TokenType::Precision),
          "in" => Some(TokenType::In),
          "out" => Some(TokenType::Out),
          "inout" => Some(TokenType::Inout),
          "const" => Some(TokenType::Const),
          "uniform" => Some(TokenType::Uniform),
          "buffer" => Some(TokenType::Buffer),
          "shared" => Some(TokenType::Shared),
          "coherent" => Some(TokenType::Coherent),
          "volatile" => Some(TokenType::Volatile),
          "restrict" => Some(TokenType::Restrict),
          "readonly" => Some(TokenType::Readonly),
          "writeonly" => Some(TokenType::Writeonly),
          "flat" => Some(TokenType::Flat),
          "smooth" => Some(TokenType::Smooth),
          "centroid" => Some(TokenType::Centroid),
          "layout" => Some(TokenType::Layout),
          "patch" => Some(TokenType::Patch),
          "sample" => Some(TokenType::Sample),
          "while" => Some(TokenType::While),
          "break" => Some(TokenType::Break),
          "continue" => Some(TokenType::Continue),
          "do" => Some(TokenType::Do),
          "else" => Some(TokenType::Else),
          "for" => Some(TokenType::For),
          "if" => Some(TokenType::If),
          "discard" => Some(TokenType::Discard),
          "return" => Some(TokenType::Return),
          "switch" => Some(TokenType::Switch),
          "case" => Some(TokenType::Case),
          "default" => Some(TokenType::Default),
          _ => None,
        }.unwrap_or(TokenType::Ident(string)) // TODO: cleanup with NLL
      },
      n if n.is_digit(10) => {
        let string = l.take(|c| c.is_digit(10));

        if l.peek() == Some('u') {
          l.advance();
          let n = string.parse::<u32>().map_err(|_| format!("Failed to parse {} as a uint", string))?;
          TokenType::UintConstant(n)
        } else {
          let n = string.parse::<i32>().map_err(|_| format!("Failed to parse {} as an int", string))?;
          TokenType::IntConstant(n)
        }
      },
      '(' => { l.advance(); TokenType::OpenParen },
      ')' => { l.advance(); TokenType::CloseParen },
      '{' => { l.advance(); TokenType::OpenBrace },
      '}' => { l.advance(); TokenType::CloseBrace },
      '=' => { l.advance(); TokenType::Equal },
      ';' => { l.advance(); TokenType::Semicolon },
      ',' => { l.advance(); TokenType::Comma },
      '[' => { l.advance(); TokenType::OpenBracket },
      ']' => { l.advance(); TokenType::CloseBracket },
      '*' => { l.advance(); TokenType::Star },
      '.' => { l.advance(); TokenType::Dot },
      ':' => { l.advance(); TokenType::Colon },
      '!' => { l.advance(); TokenType::Bang },
      '-' => { l.advance(); TokenType::Dash },
      '~' => { l.advance(); TokenType::Tilde },
      '+' => {
        l.advance();
        if l.peek() == Some('+') {
          l.advance();
          TokenType::IncOp
        } else {
          TokenType::Plus
        }
      },
      '/' => { l.advance(); TokenType::Slash },
      '%' => { l.advance(); TokenType::Percent },
      '<' => { l.advance(); TokenType::OpenAngle },
      '>' => { l.advance(); TokenType::CloseAngle },
      '|' => { l.advance(); TokenType::VerticalBar },
      '^' => { l.advance(); TokenType::Caret },
      '&' => { l.advance(); TokenType::Ampersand },
      '?' => { l.advance(); TokenType::Question },

      x => return Err(format!("Don't know how to tokenize {} at {}:{}", x, line, col)),
    };

    let token = Token {
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
  use self::TokenType::*;

  let source = r"#version 310 es
layout (local_size_x = 1) in;
void main (void) {}
";

  let version = version(source).unwrap();
  let tokens = tokenize(source, version).unwrap();

  assert_eq!(tokens,
             vec![
               Token{ line: 2, col: 0,  typ: Layout },
               Token{ line: 2, col: 7,  typ: OpenParen },
               Token{ line: 2, col: 8,  typ: Ident("local_size_x".to_string()) },
               Token{ line: 2, col: 21, typ: Equal },
               Token{ line: 2, col: 23, typ: IntConstant(1) },
               Token{ line: 2, col: 24, typ: CloseParen },
               Token{ line: 2, col: 26, typ: In },
               Token{ line: 2, col: 28, typ: Semicolon },
               Token{ line: 3, col: 0,  typ: Void },
               Token{ line: 3, col: 5,  typ: Ident("main".to_string()) },
               Token{ line: 3, col: 10, typ: OpenParen },
               Token{ line: 3, col: 11, typ: Void },
               Token{ line: 3, col: 15, typ: CloseParen },
               Token{ line: 3, col: 17, typ: OpenBrace },
               Token{ line: 3, col: 18, typ: CloseBrace },
             ]);
}
