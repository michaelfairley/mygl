use gl::{GLenum,GLuint,self};
use std::collections::HashMap;

mod lex;
mod parse;
pub mod interpret;

use self::parse::{FunctionPrototype,Statement};

pub type Result<T> = ::std::result::Result<T, String>;

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Version {
  ES100,
  ES310,
  ES320,
}

#[allow(unused_variables)]
pub fn compile(source: &[u8], type_: GLenum) -> Result<Shader> {
  let source = String::from_utf8_lossy(source).to_owned();
  // TODO: is there a standard way to mutate this in place?
  let source = source
    .replace("\n\r", "\n")
    .replace("\r\n", "\r")
    .replace("\r", "\n");

  let version = lex::version(&source)?;
  let tokens = lex::tokenize(&source, version)?;
  let parse = parse::parse(&tokens, version)?;
  Shader::new(parse, version)
}

#[derive(Debug,Clone,PartialEq)]
pub struct Variable {
  pub name: String,
  pub index: i32,
  pub type_: GLuint,
  pub array_size: GLuint,
  pub offset: GLuint,
}

#[derive(Debug,Clone)]
pub struct BlockInfo {
  pub name: String,
  pub var_name: String,
  pub binding: u32,
  pub size: u32,
  pub active_variables: Vec<Variable>,
}

#[derive(Debug)]
pub enum Interface {
  ShaderStorageBlock(BlockInfo),
  UniformBlock(BlockInfo),
  SomethingElse,
}

#[derive(Debug)]
pub struct Shader {
  pub version: Version,
  pub functions: HashMap<String, Vec<(FunctionPrototype, Statement)>>,
  pub interfaces: Vec<Interface>,
  pub work_group_size: Option<[u32; 3]>,
}

impl Shader {
  pub fn new(translation_unit: parse::TranslationUnit, version: Version) -> Result<Self> {
    use self::parse::*;

    let mut functions = interpret::BuiltinFunc::all();
    let mut interfaces = vec![];

    for decl in &translation_unit {
      if let &ExternalDeclaration::FunctionDefinition(ref proto, ref body) = decl {
        functions.entry(proto.name.clone()).or_insert(vec![]).push(((*proto).clone(), (*body).clone()));
      } else if let &ExternalDeclaration::Block(ref quals, ref name, ref members, ref var_name) = decl {
        let binding = quals.iter().filter_map(|q| if let &TypeQualifier::Layout(ref lqs) = q {
          lqs.iter().filter_map(|lq| if let &LayoutQualifierId::Int(ref name, val) = lq {
            if name == "binding" { Some(val) } else { None }
          } else { None }).next()
        } else { None }).next().unwrap_or(0);

        let mut size = 0;

        let active_variables = members.iter().flat_map(|&(ref type_, ref names)| {
          let type_ = &(type_.1).0;

          let (type_, type_size) = match type_ {
            &TypeSpecifierNonArray::Uint => (gl::GL_UNSIGNED_INT, 4),
            ref x => unimplemented!("{:?}", x),
          };

          names.iter().map(|&(ref name, ref array)| {
            let array_size: u32 = if array.is_empty() {
              1
            } else {
              array.iter().map(|a| a.as_ref().map(|a| a.eval()).unwrap_or(0)).sum()
            };

            let var = Variable{
              name: name.clone(),
              index: -1,
              type_: type_,
              array_size: array_size,
              offset: size,
            };

            size += type_size * array_size;

            var
          }).collect::<Vec<_>>().into_iter()
        }).collect();

        let info = BlockInfo{
          name: name.clone(),
          var_name: var_name.as_ref().unwrap().clone(),
          binding: binding as u32,
          size: size,
          active_variables: active_variables,
        };

        if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Buffer)) {
          interfaces.push(Interface::ShaderStorageBlock(info));
        } else if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Uniform)) {
          interfaces.push(Interface::UniformBlock(info));
        } else { unimplemented!(); }
      }
    }

    let work_group_size = translation_unit.iter().filter_map(|decl| {
      let mut x = 1;
      let mut y = 1;
      let mut z = 1;
      if let &ExternalDeclaration::TypeQualifier(ref quals) = decl {
        if quals.len() == 2 && quals[1] == TypeQualifier::Storage(StorageQualifier::In) {
          if let TypeQualifier::Layout(ref ls) = quals[0] {
            for l in ls {
              match l {
                &LayoutQualifierId::Int(ref s, i) if s == "local_size_x" => { x = i as u32 },
                &LayoutQualifierId::Int(ref s, i) if s == "local_size_y" => { y = i as u32 },
                &LayoutQualifierId::Int(ref s, i) if s == "local_size_z" => { z = i as u32 },
                x => unimplemented!("{:?}", x),
              }
            }
            Some([x, y, z])
          } else { None }
        } else { None }
      } else { None }
    }).next();

    Ok(Shader{
      version,
      functions,
      interfaces,
      work_group_size,
    })
  }
}

#[cfg(test)]
#[test]
fn test_compile() {
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

  let shader = compile(source.as_bytes(), ::gl::GL_COMPUTE_SHADER);
  assert!(shader.is_ok());
}
