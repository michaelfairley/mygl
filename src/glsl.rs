use gl::GLenum;
use std::collections::VecDeque;

pub type Result<T> = ::std::result::Result<T, String>;

#[allow(unused_variables)]
pub fn compile(source: &[u8], type_: GLenum) -> Result<()> {
  let source = String::from_utf8_lossy(source).to_owned();
  // TODO: is there a standard way to mutate this in place?
  let source = source
    .replace("\n\r", "\n")
    .replace("\r\n", "\r")
    .replace("\r", "\n");

  let version = version(&source)?;
  let tokens = tokenize(&source, version)?;
  let parse = parse(&tokens, version)?;

  Ok(())
}

//
// Tokenization
//

fn tokenize(source: &str, version: Version) -> Result<Vec<Token>> {
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

#[derive(PartialEq,Debug)]
struct Token {
  line: usize,
  col: usize,
  typ: TokenType,
}

#[allow(dead_code)]
#[derive(PartialEq,Debug)]
enum TokenType {
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
  fn is_builtin_type(&self) -> bool {
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


#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum Version {
  ES100,
  ES310,
  ES320,
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

fn version(source: &str) -> Result<Version> {
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

//
// Parsing
//

fn parse(tokens: &[Token], version: Version) -> Result<parse::TranslationUnit> {
  parse::Parser::new(tokens, version).parse_translation_unit()
}

mod parse {
  use super::{Result,Token,Version,TokenType};

  pub struct Parser<'a> {
    tokens: &'a [Token],
    _version: Version,
  }

  impl<'a> Parser<'a> {
    pub(super) fn new(tokens: &'a [Token], version: Version) -> Parser<'a> {
      Self{
        tokens,
        _version: version,
      }
    }

    pub fn parse_translation_unit(&mut self) -> Result<TranslationUnit> {
      let mut result = vec![];

      while !self.tokens.is_empty() {
        let root = self.parse_external_declaration()?;
        result.push(root);
      }

      Ok(result)
    }

    fn peek(&mut self) -> Result<&'a TokenType> {
      self.tokens.first().map(|t| &t.typ).ok_or("Ran out of input too soon".to_string())
    }

    fn peekn(&mut self, n:usize) -> Result<&'a TokenType> {
      self.tokens.get(n).map(|t| &t.typ).ok_or("Ran out of input too soon".to_string())
    }

    fn consume(&mut self, typ: &TokenType) -> Result<bool> {
      let (first, rest) = self.tokens.split_first().ok_or(format!("Ran out of input too soon; expected {:?}", typ))?;

      if &first.typ == typ {
        self.tokens = rest;
        Ok(true)
      } else {
        Ok(false)
      }
    }

    fn must_consume(&mut self, typ: &TokenType) -> Result<()> {
      if !self.consume(typ)? {
        let next = self.tokens.first().unwrap();
        Err(format!("Expected {:?}, got {:?} at {}:{}", typ, next.typ, next.line, next.col))
      } else { Ok(()) }
    }

    fn consume_ident(&mut self) -> Result<Option<String>> {
      let (first, rest) = self.tokens.split_first().ok_or("Ran out of input too soon".to_string())?;

      if let TokenType::Ident(ref ident) = first.typ {
        self.tokens = rest;
        Ok(Some(ident.clone()))
      } else {
        Ok(None)
      }
    }

    fn must_consume_ident(&mut self) -> Result<String> {
      let (first, rest) = self.tokens.split_first().ok_or("Ran out of input too soon".to_string())?;

      if let TokenType::Ident(ref ident) = first.typ {
        self.tokens = rest;
        Ok(ident.clone())
      } else {
        let next = self.tokens.first().unwrap();
        Err(format!("Expected ident, got {:?} at {}:{}", next.typ, next.line, next.col))
      }
    }

    fn advance(&mut self) {
      let (_, rest) = self.tokens.split_first().unwrap();
      self.tokens = rest;
    }

    fn unexpected<T>(&mut self) -> Result<T> {
      let token = self.tokens.first().unwrap();

      Err(format!("Unexpected token: {:?} at {}:{}", token.typ, token.line, token.col))
    }

    fn parse_external_declaration(&mut self) -> Result<ExternalDeclaration> {
      let type_qualifiers = self.parse_type_qualifiers()?;

      if self.consume(&TokenType::Semicolon)? {
        return Ok(ExternalDeclaration::TypeQualifier(type_qualifiers));
      }

      // TODO: this section is awkward
      let next = self.peek()?;
      if let &TokenType::Ident(ref _ident) = next {
        let peek1 = self.peekn(1)?;
        if peek1 == &TokenType::Comma || peek1 == &TokenType::Semicolon {
          unimplemented!()
        } else if peek1 == &TokenType::OpenBrace {
          let block_name = self.must_consume_ident()?;
          self.must_consume(&TokenType::OpenBrace)?;

          let member_list = self.parse_member_list()?;

          let instance_name = self.consume_ident()?;
          self.must_consume(&TokenType::Semicolon)?;

          return Ok(ExternalDeclaration::Block(type_qualifiers, block_name, member_list, instance_name));
        }
      }

      let typ = self.parse_type()?;
      let fully_specified_type = (type_qualifiers, typ);

      let name = self.must_consume_ident()?;
      self.must_consume(&TokenType::OpenParen)?;
      let parameters = self.parse_parameter_declarations()?;

      let function_proto = FunctionPrototype{
        typ: fully_specified_type,
        name: name,
        params: parameters,
      };

      if self.consume(&TokenType::Semicolon)? {
        Ok(ExternalDeclaration::FunctionPrototype(function_proto))
      } else if self.peek()? == &TokenType::OpenBrace {
        let body = self.parse_statement()?;
        Ok(ExternalDeclaration::FunctionDefinition(function_proto, body))
      } else { self.unexpected() }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
      if self.consume(&TokenType::OpenBrace)? {
        let mut statements = vec![];

        while !self.consume(&TokenType::CloseBrace)? {
          let statement = self.parse_statement()?;
          statements.push(statement);
        }

        return Ok(Statement::Compound(statements));
      } if self.consume(&TokenType::For)? {
        self.must_consume(&TokenType::OpenParen)?;

        let init = self.parse_statement()?;
        let condition = self.parse_expression()?;
        self.must_consume(&TokenType::Semicolon)?;
        let iter = if self.consume(&TokenType::CloseParen)? {
          Expression::Empty
        } else {
          let expr = self.parse_expression()?;
          self.must_consume(&TokenType::CloseParen)?;
          expr
        };

        let body = self.parse_statement()?;

        return Ok(Statement::For(Box::new(init), condition, iter, Box::new(body)));
      }



      let type_qualifiers = self.parse_type_qualifiers()?;

      let is_declaration = !type_qualifiers.is_empty() || self.peek()?.is_builtin_type();

      if is_declaration {
        let typ = self.parse_type()?;
        let fully_specified_type = (type_qualifiers, typ);

        let name = self.must_consume_ident()?;
        self.must_consume(&TokenType::Equal)?;

        let initializer = self.parse_expression()?;

        self.must_consume(&TokenType::Semicolon)?;

        Ok(Statement::Declaration(fully_specified_type, name, initializer))
      } else {
        let expr = self.parse_expression()?;
        self.must_consume(&TokenType::Semicolon)?;
        Ok(Statement::Expression(expr))
      }
    }

    fn parse_expression(&mut self) -> Result<Expression> {
      self.parse_assignment_expression()
    }
    fn parse_assignment_expression(&mut self) -> Result<Expression> {
      let a = self.parse_conditional_expression()?;

      if self.consume(&TokenType::Equal)? {
        let rhs = self.parse_expression()?;
        Ok(Expression::Assignment(Box::new(a), Box::new(rhs)))
      } else {
        Ok(a)
      }
    }
    fn parse_conditional_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_logical_or_expression()
    }
    fn parse_logical_or_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_logical_xor_expression()
    }
    fn parse_logical_xor_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_logical_and_expression()
    }
    fn parse_logical_and_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_binary_or_expression()
    }
    fn parse_binary_or_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_binary_xor_expression()
    }
    fn parse_binary_xor_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_binary_and_expression()
    }
    fn parse_binary_and_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_equality_expression()
    }
    fn parse_equality_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_relational_expression()
    }
    fn parse_relational_expression(&mut self) -> Result<Expression> {
      // TODO
      let a = self.parse_shift_expression()?;

      if self.consume(&TokenType::OpenAngle)? {
        let b = self.parse_expression()?;
        Ok(Expression::Comparison(Box::new(a), Comparison::Less, Box::new(b)))
      } else if self.consume(&TokenType::CloseAngle)? {
        let b = self.parse_expression()?;
        Ok(Expression::Comparison(Box::new(a), Comparison::Greater, Box::new(b)))
      } else if self.consume(&TokenType::LeOp)? {
        let b = self.parse_expression()?;
        Ok(Expression::Comparison(Box::new(a), Comparison::LessEqual, Box::new(b)))
      } else if self.consume(&TokenType::GeOp)? {
        let b = self.parse_expression()?;
        Ok(Expression::Comparison(Box::new(a), Comparison::GreaterEqual, Box::new(b)))
      } else {
        Ok(a)
      }
    }
    fn parse_shift_expression(&mut self) -> Result<Expression> {
      // TODO
      self.parse_additive_expression()
    }
    fn parse_additive_expression(&mut self) -> Result<Expression> {
      let a = self.parse_multiplicative_expression()?;

      if self.consume(&TokenType::Plus)? {
        let b = self.parse_additive_expression()?;
        Ok(Expression::Add(Box::new(a), Box::new(b)))
      } else {
        Ok(a)
      }
    }
    fn parse_multiplicative_expression(&mut self) -> Result<Expression> {
      let a = self.parse_unary_expression()?;

      if self.consume(&TokenType::Star)? {
        let b = self.parse_multiplicative_expression()?;
        Ok(Expression::Multiply(Box::new(a), Box::new(b)))
      } else if self.consume(&TokenType::Slash)? {
        let b = self.parse_multiplicative_expression()?;
        Ok(Expression::Divide(Box::new(a), Box::new(b)))
      } else {
        Ok(a)
      }
    }
    fn parse_unary_expression(&mut self) -> Result<Expression> {
      if self.consume(&TokenType::Tilde)? {
        let a = self.parse_expression()?;
        Ok(Expression::BinaryNot(Box::new(a)))
      } else {
        self.parse_postfix_expression()
      }
    }
    fn parse_postfix_expression(&mut self) -> Result<Expression> {
      let possible_function_name = match *self.peek()? {
        TokenType::Ident(ref name) => Some(name.clone()),
        TokenType::Uint => Some("uint".to_string()),
        _ => None,
      };

      if let Some(name) = possible_function_name {
        if self.peekn(1)? == &TokenType::OpenParen {
          self.advance();
          self.advance();

          let mut arguments = vec![];
          if !self.consume(&TokenType::Void)? {
            loop {
              let expr = self.parse_expression()?;
              arguments.push(expr);
              if !self.consume(&TokenType::Comma)? { break; }
            }
          }
          self.must_consume(&TokenType::CloseParen)?;

          return Ok(Expression::FunctionCall(name, arguments));
        }
      }

      let mut expr = self.parse_primary_expression()?;
      loop {
        if self.consume(&TokenType::OpenBracket)? {
          let inner = self.parse_expression()?;
          self.must_consume(&TokenType::CloseBracket)?;

          expr = Expression::Index(Box::new(expr), Box::new(inner));
        } else if self.consume(&TokenType::Dot)? {
          let field = self.must_consume_ident()?;

          if field.as_str() == "length" && self.consume(&TokenType::OpenParen)? {
            self.must_consume(&TokenType::CloseParen)?;
          }
          expr = Expression::FieldSelection(Box::new(expr), field);
        } else if self.consume(&TokenType::IncOp)? {
          expr = Expression::PostInc(Box::new(expr))
        } else {
          return Ok(expr)
        }
      }
    }
    fn parse_primary_expression(&mut self) -> Result<Expression> {
      if self.consume(&TokenType::OpenParen)? {
        let expr = self.parse_expression()?;
        self.must_consume(&TokenType::CloseParen)?;
        return Ok(expr)
      }

      let primary = match *self.peek()? {
        TokenType::Ident(ref name) => Expression::Variable(name.clone()),
        TokenType::IntConstant(x) => Expression::IntConstant(x),
        TokenType::UintConstant(x) => Expression::UintConstant(x),
        _ => { self.unexpected()? },
      };
      self.advance();
      Ok(primary)
    }

    fn parse_member_list(&mut self) -> Result<MemberList> {
      if self.consume(&TokenType::CloseBrace)? { return Ok(vec![]); }

      let mut result = vec![];

      loop {
        let qualifiers = self.parse_type_qualifiers()?;
        let typ = self.parse_type()?;

        let fully_specified = (qualifiers, typ);

        let mut declarators = vec![];
        loop {
          let declarator = self.must_consume_ident()?;
          declarators.push((declarator, self.parse_array_specifier()?));
          if !self.consume(&TokenType::Comma)? { break; }
        }
        self.must_consume(&TokenType::Semicolon)?;

        result.push((fully_specified, declarators));

        if self.consume(&TokenType::CloseBrace)? { break; }
      }

      Ok(result)
    }

    fn parse_type(&mut self) -> Result<TypeSpecifier> {
      let typ = if self.consume(&TokenType::Void)? {
        TypeSpecifierNonArray::Void
      } else if self.consume(&TokenType::Uint)? {
        TypeSpecifierNonArray::Uint
      } else if self.consume(&TokenType::UVec3)? {
        TypeSpecifierNonArray::UVec3
      } else { self.unexpected()? };

      Ok((typ, self.parse_array_specifier()?))
    }

    fn parse_type_qualifiers(&mut self) -> Result<Vec<TypeQualifier>> {
      let mut result = vec![];

      loop {
        if self.consume(&TokenType::Layout)? {
          self.must_consume(&TokenType::OpenParen)?;

          let mut layout_qualifier_ids = vec![];

          loop {
            if let &TokenType::Ident(ref name) = self.peek()? {
              self.advance();
              if self.consume(&TokenType::Equal)? {
                if let &TokenType::IntConstant(val) = self.peek()? {
                  self.advance();
                  layout_qualifier_ids.push(LayoutQualifierId::Int(name.clone(), val));
                } else { self.unexpected()? }
              } else {
                layout_qualifier_ids.push(LayoutQualifierId::Ident(name.clone()));
              }
            } else { self.unexpected()? }
            if self.consume(&TokenType::CloseParen)? {
              break;
            } else {
              self.must_consume(&TokenType::Comma)?;
            }
          }

          result.push(TypeQualifier::Layout(layout_qualifier_ids));
        } else if self.consume(&TokenType::In)? {
          result.push(TypeQualifier::Storage(StorageQualifier::In));
        } else if self.consume(&TokenType::Buffer)? {
          result.push(TypeQualifier::Storage(StorageQualifier::Buffer));
        } else {
          return Ok(result)
        }
      }
    }

    fn parse_parameter_declarations(&mut self) -> Result<Vec<ParameterDeclaration>> {
      if self.consume(&TokenType::CloseParen)? {
        return Ok(vec![])
      }

      let mut result = vec![];

      loop {
        let qualifiers = self.parse_type_qualifiers()?;
        let typ = self.parse_type()?;

        let fully_specified = (qualifiers, typ);

        let ident = self.consume_ident()?;
        let ident = if let Some(ident) = ident {
          Some((ident, self.parse_array_specifier()?))
        } else { None };

        result.push((fully_specified, ident));

        if self.consume(&TokenType::CloseParen)? {
          break;
        } else {
          self.must_consume(&TokenType::Comma)?;
        }
      }

      Ok(result)
    }

    fn parse_array_specifier(&mut self) -> Result<ArraySpecifier> {
      let mut result = vec![];

      loop {
        if !self.consume(&TokenType::OpenBracket)? { break; }
        let expr = if self.consume(&TokenType::CloseBracket)? {
          None
        } else {
          let expr = self.parse_constant_expression()?;
          self.must_consume(&TokenType::CloseBracket)?;
          Some(expr)
        };

        result.push(expr);
      }

      Ok(result)
    }

    fn parse_constant_expression(&mut self) -> Result<ConstantExpression> {
      let expr = match *self.peek()? {
        TokenType::IntConstant(i) => ConstantExpression::IntConstant(i),
        TokenType::UintConstant(i) => ConstantExpression::UintConstant(i),
        _ => self.unexpected()?,
      };
      self.advance();
      Ok(expr)
    }
  }

  #[derive(Debug,PartialEq)]
  pub enum ConstantExpression {
    IntConstant(i32),
    UintConstant(u32),
  }

  pub type ArraySpecifier = Vec<Option<ConstantExpression>>;

  pub type Identifier = String;

  pub type TranslationUnit = Vec<ExternalDeclaration>;

  #[derive(Debug,PartialEq)]
  pub enum ExternalDeclaration {
    FunctionPrototype(FunctionPrototype),
    FunctionDefinition(FunctionPrototype, Statement),
    Block(Vec<TypeQualifier>, String, MemberList, Option<String>),
    TypeQualifier(Vec<TypeQualifier>),
  }

  pub type MemberList = Vec<(FullySpecifiedType, Vec<(Identifier, ArraySpecifier)>)>;

  pub type LayoutQualifier = Vec<LayoutQualifierId>;

  #[derive(Debug,PartialEq)]
  pub enum LayoutQualifierId {
    Ident(Identifier),
    Int(Identifier, i32),
    // Uint(Identifier, u32),
    // Shared,
  }

  #[derive(Debug,PartialEq)]
  pub struct FunctionPrototype {
    pub typ: FullySpecifiedType,
    pub name: Identifier,
    pub params: Vec<ParameterDeclaration>,
  }

  pub type FullySpecifiedType = (Vec<TypeQualifier>, TypeSpecifier);

  #[derive(Debug,PartialEq)]
  pub enum TypeQualifier {
    Layout(LayoutQualifier),
    Storage(StorageQualifier),
    // TODO
  }

  #[derive(Debug,PartialEq)]
  pub enum StorageQualifier {
    In,
    Buffer,
    // TODO
  }

  pub type ParameterDeclaration = (FullySpecifiedType,
                                   Option<(Identifier, ArraySpecifier)>);

  pub type TypeSpecifier = (TypeSpecifierNonArray, ArraySpecifier);

  #[derive(Debug,PartialEq)]
  pub enum TypeSpecifierNonArray {
    Void,
    Uint,
    UVec3,
    // TODO
  }

  #[derive(Debug,PartialEq)]
  pub enum Statement {
    Compound(Vec<Statement>),
    Declaration(FullySpecifiedType, String, Expression),
    For(Box<Statement>, Expression, Expression, Box<Statement>),
    Expression(Expression),
  }

  #[derive(Debug,PartialEq)]
  pub enum Expression {
    Empty,
    Variable(String),
    IntConstant(i32),
    UintConstant(u32),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    FieldSelection(Box<Expression>, String),
    Comparison(Box<Expression>, Comparison, Box<Expression>),
    PostInc(Box<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    BinaryNot(Box<Expression>),
  }

  #[derive(Debug,PartialEq)]
  pub enum Comparison {
    Less,
    Greater,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
  }


  #[cfg(test)]
  #[test]
  fn test_order_of_operations() {
    use self::Expression::*;
    { // Add first
      let tokens = vec![
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(1) },
        Token{ line: 0, col: 0, typ: TokenType::Plus },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(2) },
        Token{ line: 0, col: 0, typ: TokenType::Star },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(3) },
        Token{ line: 0, col: 0, typ: TokenType::Semicolon },
      ];

      let mut parser = Parser::new(&tokens, Version::ES310);
      let ast = parser.parse_expression().unwrap();

      assert_eq!(ast,
                 Add(Box::new(IntConstant(1)),
                     Box::new(Multiply(Box::new(IntConstant(2)),
                                       Box::new(IntConstant(3))))));
    }

    { // Multiply first
      let tokens = vec![
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(1) },
        Token{ line: 0, col: 0, typ: TokenType::Star },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(2) },
        Token{ line: 0, col: 0, typ: TokenType::Plus },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(3) },
        Token{ line: 0, col: 0, typ: TokenType::Semicolon },
      ];

      let mut parser = Parser::new(&tokens, Version::ES310);
      let ast = parser.parse_expression().unwrap();

      assert_eq!(ast,
                 Add(Box::new(Multiply(Box::new(IntConstant(1)),
                                       Box::new(IntConstant(2)))),
                     Box::new(IntConstant(3))));
    }

    { // Add first with parens
      let tokens = vec![
        Token{ line: 0, col: 0, typ: TokenType::OpenParen },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(1) },
        Token{ line: 0, col: 0, typ: TokenType::Plus },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(2) },
        Token{ line: 0, col: 0, typ: TokenType::CloseParen },
        Token{ line: 0, col: 0, typ: TokenType::Star },
        Token{ line: 0, col: 0, typ: TokenType::IntConstant(3) },
        Token{ line: 0, col: 0, typ: TokenType::Semicolon },
      ];

      let mut parser = Parser::new(&tokens, Version::ES310);
      let ast = parser.parse_expression().unwrap();

      assert_eq!(ast,
                 Multiply(Box::new(Add(Box::new(IntConstant(1)),
                                       Box::new(IntConstant(2)))),
                          Box::new(IntConstant(3))));
    }
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

#[cfg(test)]
#[test]
fn test_parse() {
  use self::parse::*;

  let source = r"#version 310 es
layout (local_size_x = 1) in;
void main (void);
void main (void) {}
";

  let version = version(source).unwrap();
  let tokens = tokenize(source, version).unwrap();
  let ast = parse(&tokens, version).unwrap();

  assert_eq!(ast,
             vec![
               ExternalDeclaration::TypeQualifier(vec![
                 TypeQualifier::Layout(vec![
                   LayoutQualifierId::Int("local_size_x".to_string(), 1)
                 ]),
                 TypeQualifier::Storage(StorageQualifier::In),
               ]),
               ExternalDeclaration::FunctionPrototype(FunctionPrototype{
                 typ: (vec![], (TypeSpecifierNonArray::Void, vec![])),
                 name: "main".to_string(),
                 params: vec![((vec![], (TypeSpecifierNonArray::Void, vec![])), None)],
               }),
               ExternalDeclaration::FunctionDefinition(
                 FunctionPrototype{
                   typ: (vec![], (TypeSpecifierNonArray::Void, vec![])),
                   name: "main".to_string(),
                   params: vec![((vec![], (TypeSpecifierNonArray::Void, vec![])), None)],
                 },
                 Statement::Compound(vec![]),
               ),
             ]);
}

#[cfg(test)]
#[test]
fn test_parse2() {
  let source = r"#version 310 es
layout (local_size_x = 10, local_size_y = 12, local_size_z = 6) in;
layout(binding = 0) buffer Input {
    uint values[20];
} sb_in;
layout (binding = 1) buffer Output {
    uint values[20];
} sb_out;
void main (void) {
    uvec3 size           = gl_NumWorkGroups * gl_WorkGroupSize;
    uint numValuesPerInv = uint(sb_in.values.length()) / (size.x*size.y*size.z);
    uint groupNdx        = size.x*size.y*gl_GlobalInvocationID.z + size.x*gl_GlobalInvocationID.y + gl_GlobalInvocationID.x;
    uint offset          = numValuesPerInv*groupNdx;

    for (uint ndx = 0u; ndx < numValuesPerInv; ndx++)
        sb_out.values[offset + ndx] = ~sb_in.values[offset + ndx];
}";

  let version = version(source).unwrap();
  let tokens = tokenize(source, version).unwrap();
  parse(&tokens, version).unwrap();
}
