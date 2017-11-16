use super::{Result,Version};
use super::lex::{FullToken,Token};

pub(super) fn parse(tokens: &[FullToken], version: Version) -> Result<TranslationUnit> {
  Parser::new(tokens, version).parse_translation_unit()
}

pub struct Parser<'a> {
  tokens: &'a [FullToken],
  _version: Version,
}

impl<'a> Parser<'a> {
  pub(super) fn new(tokens: &'a [FullToken], version: Version) -> Parser<'a> {
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

  fn peek(&mut self) -> Result<&'a Token> {
    self.tokens.first().map(|t| &t.typ).ok_or("Ran out of input too soon".to_string())
  }

  fn peekn(&mut self, n:usize) -> Result<&'a Token> {
    self.tokens.get(n).map(|t| &t.typ).ok_or("Ran out of input too soon".to_string())
  }

  fn consume(&mut self, typ: Token) -> Result<bool> {
    let (first, rest) = self.tokens.split_first().ok_or(format!("Ran out of input too soon; expected {:?}", typ))?;

    if first.typ == typ {
      self.tokens = rest;
      Ok(true)
    } else {
      Ok(false)
    }
  }

  fn must_consume(&mut self, typ: Token) -> Result<()> {
    if !self.consume(typ.clone())? {
      let next = self.tokens.first().unwrap();
      Err(format!("Expected {:?}, got {:?} at {}:{}", typ, next.typ, next.line, next.col))
    } else { Ok(()) }
  }

  fn consume_ident(&mut self) -> Result<Option<String>> {
    let (first, rest) = self.tokens.split_first().ok_or("Ran out of input too soon".to_string())?;

    if let Token::Ident(ref ident) = first.typ {
      self.tokens = rest;
      Ok(Some(ident.clone()))
    } else {
      Ok(None)
    }
  }

  fn must_consume_ident(&mut self) -> Result<String> {
    let (first, rest) = self.tokens.split_first().ok_or("Ran out of input too soon".to_string())?;

    if let Token::Ident(ref ident) = first.typ {
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
    if self.consume(Token::Precision)? {
      let precision = if self.consume(Token::HighPrecision)? {
        PrecisionQualifier::High
      } else if self.consume(Token::MediumPrecision)? {
        PrecisionQualifier::Medium
      } else if self.consume(Token::LowPrecision)? {
        PrecisionQualifier::Low
      } else {
        self.unexpected()?
      };

      let typ = self.parse_type()?;
      self.must_consume(Token::Semicolon)?;

      return Ok(ExternalDeclaration::Precision(precision, typ));
    }

    let type_qualifiers = self.parse_type_qualifiers()?;

    if self.consume(Token::Semicolon)? {
      return Ok(ExternalDeclaration::TypeQualifier(type_qualifiers));
    }

    // TODO: this section is awkward
    let next = self.peek()?;
    if let &Token::Ident(ref _ident) = next {
      let peek1 = self.peekn(1)?;
      if peek1 == &Token::Comma || peek1 == &Token::Semicolon {
        unimplemented!()
      } else if peek1 == &Token::OpenBrace {
        let block_name = self.must_consume_ident()?;
        self.must_consume(Token::OpenBrace)?;

        let member_list = self.parse_member_list()?;

        let instance_name = self.consume_ident()?;
        self.must_consume(Token::Semicolon)?;

        return Ok(ExternalDeclaration::Block(type_qualifiers, block_name, member_list, instance_name));
      }
    }

    let typ = self.parse_type()?;
    let fully_specified_type = (type_qualifiers, typ);

    if self.consume(Token::Semicolon)? {
      return Ok(ExternalDeclaration::TypeDeclaration(fully_specified_type))
    }

    let name = self.must_consume_ident()?;

    if self.consume(Token::OpenParen)? {
      let parameters = self.parse_parameter_declarations()?;

      let function_proto = FunctionPrototype{
        typ: fully_specified_type,
        name: name,
        params: parameters,
      };

      if self.consume(Token::Semicolon)? {
        Ok(ExternalDeclaration::FunctionPrototype(function_proto))
      } else if self.peek()? == &Token::OpenBrace {
        let body = self.parse_statement()?;
        Ok(ExternalDeclaration::FunctionDefinition(function_proto, body))
      } else { self.unexpected() }
    } else {
      let array_specifier = self.parse_array_specifier()?;

      self.must_consume(Token::Semicolon)?;
      Ok(ExternalDeclaration::Variable(fully_specified_type, name, array_specifier))
    }
  }

  fn parse_statement(&mut self) -> Result<Statement> {
    if self.consume(Token::OpenBrace)? {
      let mut statements = vec![];

      while !self.consume(Token::CloseBrace)? {
        let statement = self.parse_statement()?;
        statements.push(statement);
      }

      return Ok(Statement::Compound(statements));
    } else if self.consume(Token::For)? {
      self.must_consume(Token::OpenParen)?;

      let init = self.parse_statement()?;
      let condition = self.parse_expression()?;
      self.must_consume(Token::Semicolon)?;
      let iter = if self.consume(Token::CloseParen)? {
        Expression::Empty
      } else {
        let expr = self.parse_expression()?;
        self.must_consume(Token::CloseParen)?;
        expr
      };

      let body = self.parse_statement()?;

      return Ok(Statement::For(Box::new(init), condition, iter, Box::new(body)));
    } else if self.consume(Token::If)? {
      self.must_consume(Token::OpenParen)?;

      let condition = self.parse_expression()?;

      self.must_consume(Token::CloseParen)?;

      let body = self.parse_statement()?;

      let else_body = if self.consume(Token::Else)? {
        Some(self.parse_statement()?)
      } else { None };

      return Ok(Statement::If(condition, Box::new(body), Box::new(else_body)));
    }



    let type_qualifiers = self.parse_type_qualifiers()?;

    let is_declaration = !type_qualifiers.is_empty() || self.peek()?.is_builtin_type();

    if is_declaration {
      let typ = self.parse_type()?;
      let fully_specified_type = (type_qualifiers, typ);

      let name = self.must_consume_ident()?;

      let initializer = if self.consume(Token::Equal)? {
        Some(self.parse_expression()?)
      } else { None };
      self.must_consume(Token::Semicolon)?;

      Ok(Statement::Declaration(fully_specified_type, name, initializer))
    } else {
      let expr = self.parse_expression()?;
      self.must_consume(Token::Semicolon)?;
      Ok(Statement::Expression(expr))
    }
  }

  fn parse_expression(&mut self) -> Result<Expression> {
    self.parse_assignment_expression()
  }
  fn parse_assignment_expression(&mut self) -> Result<Expression> {
    let a = self.parse_conditional_expression()?;

    if self.consume(Token::Equal)? {
      let rhs = self.parse_expression()?;
      Ok(Expression::Assignment(Box::new(a), Box::new(rhs)))
    } else if self.consume(Token::AddAssign)? {
      let rhs = self.parse_expression()?;
      Ok(Expression::AddAssign(Box::new(a), Box::new(rhs)))
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
    let mut a = self.parse_relational_expression()?;

    loop {
      if self.consume(Token::EqOp)? {
        let b = self.parse_equality_expression()?;
        a = Expression::Comparison(Box::new(a), Comparison::Equal, Box::new(b))
      } else if self.consume(Token::NeOp)? {
        let b = self.parse_equality_expression()?;
        a = Expression::Comparison(Box::new(a), Comparison::NotEqual, Box::new(b))
      } else { break; }
    }

    Ok(a)
  }
  fn parse_relational_expression(&mut self) -> Result<Expression> {
    let a = self.parse_shift_expression()?;

    if self.consume(Token::OpenAngle)? {
      let b = self.parse_expression()?;
      Ok(Expression::Comparison(Box::new(a), Comparison::Less, Box::new(b)))
    } else if self.consume(Token::CloseAngle)? {
      let b = self.parse_expression()?;
      Ok(Expression::Comparison(Box::new(a), Comparison::Greater, Box::new(b)))
    } else if self.consume(Token::LeOp)? {
      let b = self.parse_expression()?;
      Ok(Expression::Comparison(Box::new(a), Comparison::LessEqual, Box::new(b)))
    } else if self.consume(Token::GeOp)? {
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
    let mut a = self.parse_multiplicative_expression()?;

    loop {
      if self.consume(Token::Plus)? {
        let b = self.parse_multiplicative_expression()?;
        a = Expression::Add(Box::new(a), Box::new(b));
      } else if self.consume(Token::Dash)? {
        let b = self.parse_multiplicative_expression()?;
        a = Expression::Sub(Box::new(a), Box::new(b));
      } else {
        return Ok(a)
      }
    }
  }
  fn parse_multiplicative_expression(&mut self) -> Result<Expression> {
    let mut a = self.parse_unary_expression()?;

    loop {
      if self.consume(Token::Star)? {
        let b = self.parse_multiplicative_expression()?;
        a = Expression::Multiply(Box::new(a), Box::new(b));
      } else if self.consume(Token::Slash)? {
        let b = self.parse_multiplicative_expression()?;
        a = Expression::Divide(Box::new(a), Box::new(b));
      } else if self.consume(Token::Percent)? {
        let b = self.parse_multiplicative_expression()?;
        a = Expression::Modulo(Box::new(a), Box::new(b));
      } else {
        return Ok(a)
      }
    }
  }
  fn parse_unary_expression(&mut self) -> Result<Expression> {
    if self.consume(Token::Tilde)? {
      let a = self.parse_expression()?;
      Ok(Expression::BinaryNot(Box::new(a)))
    } else {
      self.parse_postfix_expression()
    }
  }
  fn parse_postfix_expression(&mut self) -> Result<Expression> {
    // TODO: cleanup
    let possible_function_name = match *self.peek()? {
      Token::Ident(ref name) => Some(name.clone()),
      Token::Uint => Some("uint".to_string()),
      Token::UVec2 => Some("uvec2".to_string()),
      Token::UVec3 => Some("uvec3".to_string()),
      Token::UVec4 => Some("uvec4".to_string()),
      Token::Int => Some("int".to_string()),
      Token::IVec2 => Some("ivec2".to_string()),
      Token::IVec3 => Some("ivec3".to_string()),
      Token::IVec4 => Some("ivec4".to_string()),
      Token::Float => Some("float".to_string()),
      Token::Vec2 => Some("vec2".to_string()),
      Token::Vec3 => Some("vec3".to_string()),
      Token::Vec4 => Some("vec4".to_string()),
      Token::Bool => Some("bool".to_string()),
      Token::BVec2 => Some("bvec2".to_string()),
      Token::BVec3 => Some("bvec3".to_string()),
      Token::BVec4 => Some("bvec4".to_string()),
      _ => None,
    };

    let mut expr = if let Some(name) = possible_function_name {
      if self.peekn(1)? == &Token::OpenParen {
        self.advance();
        self.advance();

        let mut arguments = vec![];

        if self.consume(Token::CloseParen)? {
        } else if self.consume(Token::Void)? {
          self.must_consume(Token::CloseParen)?;
        } else {
          loop {
            let expr = self.parse_expression()?;
            arguments.push(expr);
            if !self.consume(Token::Comma)? { break; }
          }
          self.must_consume(Token::CloseParen)?;
        }

        Expression::FunctionCall(name, arguments)
      } else { self.parse_primary_expression()? }
    } else { self.parse_primary_expression()? };

    loop {
      if self.consume(Token::OpenBracket)? {
        let inner = self.parse_expression()?;
        self.must_consume(Token::CloseBracket)?;

        expr = Expression::Index(Box::new(expr), Box::new(inner));
      } else if self.consume(Token::Dot)? {
        let field = self.must_consume_ident()?;

        if field.as_str() == "length" && self.consume(Token::OpenParen)? {
          self.must_consume(Token::CloseParen)?;
        }
        expr = Expression::FieldSelection(Box::new(expr), field);
      } else if self.consume(Token::IncOp)? {
        expr = Expression::PostInc(Box::new(expr))
      } else {
        return Ok(expr)
      }
    }
  }
  fn parse_primary_expression(&mut self) -> Result<Expression> {
    if self.consume(Token::OpenParen)? {
      let expr = self.parse_expression()?;
      self.must_consume(Token::CloseParen)?;
      return Ok(expr)
    }

    let primary = match *self.peek()? {
      Token::Ident(ref name) => Expression::Variable(name.clone()),
      Token::IntConstant(x) => Expression::IntConstant(x),
      Token::UintConstant(x) => Expression::UintConstant(x),
      Token::FloatConstant(x) => Expression::FloatConstant(x),
      _ => { self.unexpected()? },
    };
    self.advance();
    Ok(primary)
  }

  fn parse_member_list(&mut self) -> Result<MemberList> {
    if self.consume(Token::CloseBrace)? { return Ok(vec![]); }

    let mut result = vec![];

    loop {
      let qualifiers = self.parse_type_qualifiers()?;
      let typ = self.parse_type()?;

      let fully_specified = (qualifiers, typ);

      let mut declarators = vec![];
      loop {
        let declarator = self.must_consume_ident()?;
        declarators.push((declarator, self.parse_array_specifier()?));
        if !self.consume(Token::Comma)? { break; }
      }
      self.must_consume(Token::Semicolon)?;

      result.push((fully_specified, declarators));

      if self.consume(Token::CloseBrace)? { break; }
    }

    Ok(result)
  }

  fn parse_type(&mut self) -> Result<TypeSpecifier> {
    // TODO: cleanup
    let typ = if self.consume(Token::Void)? {TypeSpecifierNonArray::Void
    } else if self.consume(Token::Uint)? { TypeSpecifierNonArray::Uint
    } else if self.consume(Token::UVec2)? { TypeSpecifierNonArray::UVec2
    } else if self.consume(Token::UVec3)? { TypeSpecifierNonArray::UVec3
    } else if self.consume(Token::UVec4)? { TypeSpecifierNonArray::UVec4
    } else if self.consume(Token::Int)? { TypeSpecifierNonArray::Int
    } else if self.consume(Token::IVec2)? { TypeSpecifierNonArray::IVec2
    } else if self.consume(Token::IVec3)? { TypeSpecifierNonArray::IVec3
    } else if self.consume(Token::IVec4)? { TypeSpecifierNonArray::IVec4
    } else if self.consume(Token::Float)? { TypeSpecifierNonArray::Float
    } else if self.consume(Token::Vec2)? { TypeSpecifierNonArray::Vec2
    } else if self.consume(Token::Vec3)? { TypeSpecifierNonArray::Vec3
    } else if self.consume(Token::Vec4)? { TypeSpecifierNonArray::Vec4
    } else if self.consume(Token::Bool)? { TypeSpecifierNonArray::Bool
    } else if self.consume(Token::BVec2)? { TypeSpecifierNonArray::BVec2
    } else if self.consume(Token::BVec3)? { TypeSpecifierNonArray::BVec3
    } else if self.consume(Token::BVec4)? { TypeSpecifierNonArray::BVec4
    } else if self.consume(Token::AtomicUint)? { TypeSpecifierNonArray::AtomicUint
    } else if self.consume(Token::UImage2D)? { TypeSpecifierNonArray::UImage2D
    } else if self.consume(Token::Struct)? {
      let name = self.consume_ident()?;
      self.must_consume(Token::OpenBrace)?;
      let members = self.parse_member_list()?;
      TypeSpecifierNonArray::Struct(name, members)
    } else if let Some(name) = self.consume_ident()? {
      TypeSpecifierNonArray::Custom(name)
    } else { self.unexpected()? };

    Ok((typ, self.parse_array_specifier()?))
  }

  fn parse_type_qualifiers(&mut self) -> Result<Vec<TypeQualifier>> {
    let mut result = vec![];

    loop {
      if self.consume(Token::Layout)? {
        self.must_consume(Token::OpenParen)?;

        let mut layout_qualifier_ids = vec![];

        loop {
          if let &Token::Ident(ref name) = self.peek()? {
            self.advance();
            if self.consume(Token::Equal)? {
              if let &Token::IntConstant(val) = self.peek()? {
                self.advance();
                layout_qualifier_ids.push(LayoutQualifierId::Int(name.clone(), val));
              } else { self.unexpected()? }
            } else {
              layout_qualifier_ids.push(LayoutQualifierId::Ident(name.clone()));
            }
          } else { self.unexpected()? }
          if self.consume(Token::CloseParen)? {
            break;
          } else {
            self.must_consume(Token::Comma)?;
          }
        }

        result.push(TypeQualifier::Layout(layout_qualifier_ids));
      } else if self.consume(Token::In)? {
        result.push(TypeQualifier::Storage(StorageQualifier::In));
      } else if self.consume(Token::Out)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Out));
      } else if self.consume(Token::Inout)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Inout));
      } else if self.consume(Token::Buffer)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Buffer));
      } else if self.consume(Token::Uniform)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Uniform));
      } else if self.consume(Token::Coherent)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Coherent));
      } else if self.consume(Token::Shared)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Shared));
      } else if self.consume(Token::Readonly)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Readonly));
      } else if self.consume(Token::Writeonly)? {
        result.push(TypeQualifier::Storage(StorageQualifier::Writeonly));
      } else if self.consume(Token::HighPrecision)? {
        result.push(TypeQualifier::Precision(PrecisionQualifier::High));
      } else if self.consume(Token::MediumPrecision)? {
        result.push(TypeQualifier::Precision(PrecisionQualifier::Medium));
      } else if self.consume(Token::LowPrecision)? {
        result.push(TypeQualifier::Precision(PrecisionQualifier::Low));
      } else {
        return Ok(result)
      }
    }
  }

  fn parse_parameter_declarations(&mut self) -> Result<Vec<ParameterDeclaration>> {
    if self.consume(Token::CloseParen)? {
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

      if self.consume(Token::CloseParen)? {
        break;
      } else {
        self.must_consume(Token::Comma)?;
      }
    }

    Ok(result)
  }

  fn parse_array_specifier(&mut self) -> Result<ArraySpecifier> {
    let mut result = vec![];

    loop {
      if !self.consume(Token::OpenBracket)? { break; }
      let expr = if self.consume(Token::CloseBracket)? {
        None
      } else {
        let expr = self.parse_constant_expression()?;
        self.must_consume(Token::CloseBracket)?;
        Some(expr)
      };

      result.push(expr);
    }

    Ok(result)
  }

  fn parse_constant_expression(&mut self) -> Result<ConstantExpression> {
    let expr = match *self.peek()? {
      Token::IntConstant(i) => ConstantExpression::IntConstant(i),
      Token::UintConstant(i) => ConstantExpression::UintConstant(i),
      _ => self.unexpected()?,
    };
    self.advance();
    Ok(expr)
  }
}

#[derive(Debug,PartialEq,Clone)]
pub enum ConstantExpression {
  IntConstant(i32),
  UintConstant(u32),
}
impl ConstantExpression {
  pub fn eval(&self) -> u32 {
    match *self {
      ConstantExpression::IntConstant(i) => i as u32,
      ConstantExpression::UintConstant(i) => i,
    }
  }
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
  Variable(FullySpecifiedType, Identifier, ArraySpecifier),
  TypeDeclaration(FullySpecifiedType),
  Precision(PrecisionQualifier, TypeSpecifier),
}

pub type MemberList = Vec<(FullySpecifiedType, Vec<(Identifier, ArraySpecifier)>)>;

pub type LayoutQualifier = Vec<LayoutQualifierId>;

#[derive(Debug,PartialEq,Clone)]
pub enum LayoutQualifierId {
  Ident(Identifier),
  Int(Identifier, i32),
  // Uint(Identifier, u32),
  // Shared,
}

#[derive(Debug,PartialEq,Clone)]
pub struct FunctionPrototype {
  pub typ: FullySpecifiedType,
  pub name: Identifier,
  pub params: Vec<ParameterDeclaration>,
}
pub type ParameterDeclaration = (FullySpecifiedType,
                                 Option<(Identifier, ArraySpecifier)>);


#[derive(Debug,PartialEq,Clone)]
pub enum TypeQualifier {
  Layout(LayoutQualifier),
  Storage(StorageQualifier),
  Precision(PrecisionQualifier),
  // INCOMPLETE
}

#[derive(Debug,PartialEq,Clone)]
pub enum PrecisionQualifier {
  High,
  Medium,
  Low,
}

#[derive(Debug,PartialEq,Clone)]
pub enum StorageQualifier {
  In,
  Out,
  Inout,
  Buffer,
  Uniform,
  Coherent,
  Shared,
  Readonly,
  Writeonly,
  // INCOMPLETE
}

// TODO: this needs to be a struct
pub type FullySpecifiedType = (Vec<TypeQualifier>, TypeSpecifier);

// TODO: either this (or the above) needs a #size method
pub type TypeSpecifier = (TypeSpecifierNonArray, ArraySpecifier);

// TODO: give this the more generic name
#[derive(Debug,PartialEq,Clone)]
pub enum TypeSpecifierNonArray {
  Float,
  Vec2,
  Vec3,
  Vec4,
  Uint,
  UVec2,
  UVec3,
  UVec4,
  Int,
  IVec2,
  IVec3,
  IVec4,
  Bool,
  BVec2,
  BVec3,
  BVec4,
  AtomicUint,
  UImage2D,
  Custom(String),
  Struct(Option<String>, MemberList),
  Void,
  // INCOMPLETE
}

#[derive(Debug,PartialEq,Clone)]
pub enum Statement {
  Compound(Vec<Statement>),
  Declaration(FullySpecifiedType, String, Option<Expression>),
  For(Box<Statement>, Expression, Expression, Box<Statement>),
  Expression(Expression),
  Builtin(super::builtin::Func),
  If(Expression, Box<Statement>, Box<Option<Statement>>),
}

#[derive(Debug,PartialEq,Clone)]
pub enum Expression {
  Empty,
  Variable(String),
  IntConstant(i32),
  UintConstant(u32),
  FloatConstant(f32),
  Multiply(Box<Expression>, Box<Expression>),
  Divide(Box<Expression>, Box<Expression>),
  Modulo(Box<Expression>, Box<Expression>),
  Add(Box<Expression>, Box<Expression>),
  Sub(Box<Expression>, Box<Expression>),
  FunctionCall(String, Vec<Expression>),
  FieldSelection(Box<Expression>, String),
  Comparison(Box<Expression>, Comparison, Box<Expression>),
  PostInc(Box<Expression>),
  Index(Box<Expression>, Box<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
  AddAssign(Box<Expression>, Box<Expression>),
  BinaryNot(Box<Expression>),
}

#[derive(Debug,PartialEq,Clone)]
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
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(1) },
      FullToken{ line: 0, col: 0, typ: Token::Plus },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(2) },
      FullToken{ line: 0, col: 0, typ: Token::Star },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(3) },
      FullToken{ line: 0, col: 0, typ: Token::Semicolon },
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
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(1) },
      FullToken{ line: 0, col: 0, typ: Token::Star },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(2) },
      FullToken{ line: 0, col: 0, typ: Token::Plus },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(3) },
      FullToken{ line: 0, col: 0, typ: Token::Semicolon },
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
      FullToken{ line: 0, col: 0, typ: Token::OpenParen },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(1) },
      FullToken{ line: 0, col: 0, typ: Token::Plus },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(2) },
      FullToken{ line: 0, col: 0, typ: Token::CloseParen },
      FullToken{ line: 0, col: 0, typ: Token::Star },
      FullToken{ line: 0, col: 0, typ: Token::IntConstant(3) },
      FullToken{ line: 0, col: 0, typ: Token::Semicolon },
    ];

    let mut parser = Parser::new(&tokens, Version::ES310);
    let ast = parser.parse_expression().unwrap();

    assert_eq!(ast,
               Multiply(Box::new(Add(Box::new(IntConstant(1)),
                                     Box::new(IntConstant(2)))),
                        Box::new(IntConstant(3))));
  }
}

#[cfg(test)]
#[test]
fn test_parse() {
  let source = r"#version 310 es
layout (local_size_x = 1) in;
void main (void);
void main (void) {}
";

  let version = super::lex::version(source).unwrap();
  let tokens = super::lex::tokenize(source, version).unwrap();
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
fn test_parse_sub() {
  let source = r"#version 310 es
void main (void) {
  int a = 3 - 2 - 1;
}
";

  let version = super::lex::version(source).unwrap();
  let tokens = super::lex::tokenize(source, version).unwrap();
  let ast = parse(&tokens, version).unwrap();

  assert_eq!(ast,
             vec![
               ExternalDeclaration::FunctionDefinition(
                 FunctionPrototype{
                   typ: (vec![], (TypeSpecifierNonArray::Void, vec![])),
                   name: "main".to_string(),
                   params: vec![((vec![], (TypeSpecifierNonArray::Void, vec![])), None)],
                 },
                 Statement::Compound(vec![
                   Statement::Declaration(
                     (vec![], (TypeSpecifierNonArray::Int, vec![])),
                     "a".to_string(),
                     Expression::Sub(
                       Box::new(Expression::Sub(
                         Box::new(Expression::IntConstant(3)),
                         Box::new(Expression::IntConstant(2)))),
                       Box::new(Expression::IntConstant(1)))
                   )
                 ]),
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

  let version = super::lex::version(source).unwrap();
  let tokens = super::lex::tokenize(source, version).unwrap();
  parse(&tokens, version).unwrap();
}
