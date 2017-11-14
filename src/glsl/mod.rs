use gl::{GLenum,GLuint,self};
use std::collections::HashMap;
use std::mem;

mod lex;
mod parse;
pub mod interpret;

use self::parse::{FunctionPrototype,Statement};

pub use self::parse::TypeSpecifierNonArray;

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
  pub type_: parse::TypeSpecifierNonArray,
  pub array_size: GLuint,
  pub offset: GLuint,
}

#[derive(Debug,Clone)]
pub struct BlockInfo {
  pub name: String,
  pub var_name: Option<String>,
  pub binding: u32,
  pub size: u32,
  pub active_variables: Vec<Variable>,
}

#[derive(Debug,Clone)]
pub struct UniformInfo {
  pub name: String,
  pub typ: GLuint,
  pub binding: usize,
  pub index: i32,
}

#[derive(Debug,Clone)]
pub struct SharedInfo {
  pub name: String,
  pub typ: TypeSpecifierNonArray,
  pub size: usize,
}

#[derive(Debug,Clone)]
pub struct AtomicCounterInfo {
  pub name: String,
  pub size: usize,
  pub binding: usize,
}

#[derive(Debug)]
pub enum Interface {
  ShaderStorageBlock(BlockInfo),
  UniformBlock(BlockInfo),
  Uniform(UniformInfo),
  Shared(SharedInfo),
  AtomicCounter(AtomicCounterInfo),
}

#[derive(Debug,Clone)]
pub struct CustomType {
  size: usize,
}
pub fn size_of(
  type_: &TypeSpecifierNonArray,
  types: Option<&HashMap<String, CustomType>>,
) -> usize {
  match type_ {
    &TypeSpecifierNonArray::Uint => mem::size_of::<u32>(),
    &TypeSpecifierNonArray::UVec2 => mem::size_of::<u32>() * 2,
    &TypeSpecifierNonArray::UVec3 => mem::size_of::<u32>() * 3,
    &TypeSpecifierNonArray::UVec4 => mem::size_of::<u32>() * 4,
    &TypeSpecifierNonArray::Custom(ref n) => types.unwrap().get(n).unwrap().size,
    ref x => unimplemented!("{:?}", x),
  }
}
pub fn gl_type(
  type_: &TypeSpecifierNonArray,
) -> GLenum {
  match type_ {
    &TypeSpecifierNonArray::Uint => ::gl::GL_UNSIGNED_INT,
    &TypeSpecifierNonArray::UVec2 => ::gl::GL_UNSIGNED_INT_VEC2,
    &TypeSpecifierNonArray::UVec3 => ::gl::GL_UNSIGNED_INT_VEC3,
    &TypeSpecifierNonArray::UVec4 => ::gl::GL_UNSIGNED_INT_VEC4,
    &TypeSpecifierNonArray::Float => ::gl::GL_FLOAT,
    // INCOMPLETE
    x => unimplemented!("{:?}", x),
  }
}

#[derive(Debug)]
pub struct Shader {
  pub version: Version,
  pub functions: HashMap<String, Vec<(FunctionPrototype, Statement)>>,
  pub interfaces: Vec<Interface>,
  pub work_group_size: Option<[u32; 3]>,
  pub types: HashMap<String, CustomType>,
}

impl Shader {
  pub fn new(translation_unit: parse::TranslationUnit, version: Version) -> Result<Self> {
    use self::parse::*;

    let mut functions = interpret::BuiltinFunc::all();
    let mut interfaces = vec![];

    let mut types: HashMap<String, CustomType> = HashMap::new();

    for decl in &translation_unit {
      match decl {
        &ExternalDeclaration::FunctionDefinition(ref proto, ref body) => {
          functions.entry(proto.name.clone()).or_insert(vec![]).push(((*proto).clone(), (*body).clone()));
        },
        &ExternalDeclaration::Block(ref quals, ref name, ref members, ref var_name) => {
          let binding = quals.iter().filter_map(|q| if let &TypeQualifier::Layout(ref lqs) = q {
            lqs.iter().filter_map(|lq| if let &LayoutQualifierId::Int(ref name, val) = lq {
              if name == "binding" { Some(val) } else { None }
            } else { None }).next()
          } else { None }).next().unwrap_or(0);

          let mut size = 0;

          let active_variables = members.iter().flat_map(|&(ref type_, ref names)| {
            let type_ = &(type_.1).0;
            let type_size = size_of(type_, Some(&types));

            names.iter().map(|&(ref name, ref array)| {
              let array_size: u32 = if array.is_empty() {
                1
              } else {
                array.iter().map(|a| a.as_ref().map(|a| a.eval()).unwrap_or(0)).sum()
              };

              let var = Variable{
                name: name.clone(),
                index: -1,
                type_: type_.clone(),
                array_size: array_size,
                offset: size,
              };

              size += type_size as u32 * array_size;

              var
            }).collect::<Vec<_>>().into_iter()
          }).collect();

          let info = BlockInfo{
            name: name.clone(),
            var_name: var_name.clone(),
            binding: binding as u32,
            size: size,
            active_variables: active_variables,
          };

          if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Buffer)) {
            interfaces.push(Interface::ShaderStorageBlock(info));
          } else if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Uniform)) {
            interfaces.push(Interface::UniformBlock(info));
          } else { unimplemented!(); }
        },
        &ExternalDeclaration::Variable((ref quals, ref typespec), ref name, ref array_spec) => {
          if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Uniform)) {
            let typ = match typespec.0 {
              TypeSpecifierNonArray::Uint => gl::GL_UNSIGNED_INT,
              TypeSpecifierNonArray::AtomicUint => gl::GL_UNSIGNED_INT_ATOMIC_COUNTER,
              TypeSpecifierNonArray::UImage2D => gl::GL_UNSIGNED_INT_IMAGE_2D,
              TypeSpecifierNonArray::UVec2 => gl::GL_UNSIGNED_INT_VEC2,
              TypeSpecifierNonArray::UVec3 => gl::GL_UNSIGNED_INT_VEC3,
              ref x => unimplemented!("{:?}", x),
            };

            let binding = quals.iter().filter_map(|q| if let &TypeQualifier::Layout(ref lqs) = q {
              lqs.iter().filter_map(|lq| if let &LayoutQualifierId::Int(ref name, val) = lq {
                if name == "binding" { Some(val) } else { None }
              } else { None }).next()
            } else { None }).next().unwrap_or(0);

            if typ == gl::GL_UNSIGNED_INT_ATOMIC_COUNTER {
              let size: u32 = if array_spec.is_empty() {
                1
              } else {
                array_spec.iter().map(|a| a.as_ref().map(|a| a.eval()).unwrap_or(0)).sum()
              };

              let atomic_info = AtomicCounterInfo{
                name: name.clone(),
                size: size as usize,
                binding: binding as usize,
              };
              interfaces.push(Interface::AtomicCounter(atomic_info));
            }

            let info = UniformInfo{
              name: name.clone(),
              binding: binding as usize,
              typ: typ,
              index: -1,
            };
            interfaces.push(Interface::Uniform(info));
          } else if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Shared)) {
            let typ = typespec.0.clone();

            let size: u32 = if array_spec.is_empty() {
              1
            } else {
              array_spec.iter().map(|a| a.as_ref().map(|a| a.eval()).unwrap_or(0)).sum()
            };

            let info = SharedInfo{
              name: name.clone(),
              typ: typ,
              size: size as usize,
            };
            interfaces.push(Interface::Shared(info));
          }
        },
        &ExternalDeclaration::FunctionPrototype(_) => {},
        &ExternalDeclaration::TypeQualifier(_) => {},
        &ExternalDeclaration::TypeDeclaration(ref typ) => {
          if let TypeSpecifierNonArray::Struct(ref name, ref members) = (typ.1).0 {
            let name = name.as_ref().unwrap();

            let size = members.iter().map(|&(ref mtype, ref _names)| {
              match &(mtype.1).0 {
                &TypeSpecifierNonArray::Float => mem::size_of::<f32>(),
                x => unimplemented!("{:?}", x),
              }
            }).sum();

            let custom_type = CustomType{
              size: size,
            };

            types.insert(name.clone(), custom_type);
          } else { unimplemented!() }
        },
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
      types,
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
