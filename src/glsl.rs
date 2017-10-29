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
          "layout" => Some(TokenType::Layout),
          "in" => Some(TokenType::In),
          _ => None,
        }.unwrap_or(TokenType::Ident(string)) // TODO: cleanup with NLL
      },
      n if n.is_digit(10) => {
        let string = l.take(|c| c.is_digit(10));
        let n = string.parse::<i32>().map_err(|_| format!("Failed to parse {} as an int", string))?;
        TokenType::IntLiteral(n)
      },
      '(' => { l.advance(); TokenType::OpenParen },
      ')' => { l.advance(); TokenType::CloseParen },
      '{' => { l.advance(); TokenType::OpenBrace },
      '}' => { l.advance(); TokenType::CloseBrace },
      '=' => { l.advance(); TokenType::Equal },
      ';' => { l.advance(); TokenType::Semicolon },
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

#[derive(PartialEq,Eq,Debug)]
struct Token {
  line: usize,
  col: usize,
  typ: TokenType,
}

#[derive(PartialEq,Eq,Debug)]
enum TokenType {
  Layout,
  OpenParen,
  Ident(String),
  Equal,
  IntLiteral(i32),
  CloseParen,
  In,
  Semicolon,
  Void,
  OpenBrace,
  CloseBrace,
  Comma,
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
    version: Version,
  }

  impl<'a> Parser<'a> {
    pub(super) fn new(tokens: &'a [Token], version: Version) -> Parser<'a> {
      Self{
        tokens,
        version,
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
      let (first, rest) = self.tokens.split_first().ok_or("Ran out of input too soon".to_string())?;

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

      let next = self.peek()?;
      if let &TokenType::Ident(ref _ident) = next {
        let peek1 = self.peekn(1)?;
        if peek1 == &TokenType::Comma || peek1 == &TokenType::Semicolon {
          unimplemented!()
        } else if peek1 == &TokenType::OpenBrace {
          unimplemented!()
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
      } else if self.consume(&TokenType::OpenBrace)? {
        let statements = vec![];
        self.must_consume(&TokenType::CloseBrace)?;
        Ok(ExternalDeclaration::FunctionDefinition(function_proto, statements))
      } else { self.unexpected() }
    }

    fn parse_type(&mut self) -> Result<TypeSpecifier> {
      if self.consume(&TokenType::Void)? {
        Ok((TypeSpecifierNonArray::Void, None))
      } else { self.unexpected() }
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
                if let &TokenType::IntLiteral(val) = self.peek()? {
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
        let ident = ident.map(|i| (i, None));

        result.push((fully_specified, ident));

        if self.consume(&TokenType::CloseParen)? {
          break;
        } else {
          self.must_consume(&TokenType::Comma)?;
        }
      }

      Ok(result)
    }
  }

  pub type Identifier = String;

  pub type TranslationUnit = Vec<ExternalDeclaration>;

  #[derive(Debug,PartialEq)]
  pub enum ExternalDeclaration {
    FunctionPrototype(FunctionPrototype),
    FunctionDefinition(FunctionPrototype, CompoundStatement),
    // TODO
    TypeQualifier(Vec<TypeQualifier>),
  }

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
    // TODO
  }

  pub type ParameterDeclaration = (FullySpecifiedType,
                                   Option<(Identifier, Option<ArraySpecifier>)>);

  pub type TypeSpecifier = (TypeSpecifierNonArray, Option<ArraySpecifier>);

  #[derive(Debug,PartialEq)]
  pub enum TypeSpecifierNonArray {
    Void,
    // TODO
  }

  pub type CompoundStatement = Vec<Statement>;

  #[derive(Debug,PartialEq)]
  pub struct Statement; // TODO
  #[derive(Debug,PartialEq)]
  pub struct ArraySpecifier; // TODO
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
               Token{ line: 2, col: 23, typ: IntLiteral(1) },
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
                 typ: (vec![], (TypeSpecifierNonArray::Void, None)),
                 name: "main".to_string(),
                 params: vec![((vec![], (TypeSpecifierNonArray::Void, None)), None)],
               }),
               ExternalDeclaration::FunctionDefinition(
                 FunctionPrototype{
                   typ: (vec![], (TypeSpecifierNonArray::Void, None)),
                   name: "main".to_string(),
                   params: vec![((vec![], (TypeSpecifierNonArray::Void, None)), None)],
                 },
                 vec![],
               ),
             ]);
}
