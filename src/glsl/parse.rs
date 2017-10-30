use super::{Result,Version};
use super::lex::{Token,TokenType};

pub(super) fn parse(tokens: &[Token], version: Version) -> Result<TranslationUnit> {
  Parser::new(tokens, version).parse_translation_unit()
}

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
