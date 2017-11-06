use gl::{GLenum,GLuint,self};

mod lex;
mod parse;
mod interpret;

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
pub struct ShaderStorageBlockInfo {
  pub name: String,
  pub binding: u32,
  pub size: u32,
  pub active_variables: Vec<Variable>,
}

#[derive(Debug)]
pub enum Interface {
  ShaderStorageBlock(ShaderStorageBlockInfo),
  SomethingElse,
}

#[derive(Debug)]
pub struct Shader {
  pub version: Version,
  pub functions: Vec<(FunctionPrototype, Statement)>,
  pub interfaces: Vec<Interface>,
}

impl Shader {
  pub fn new(translation_unit: parse::TranslationUnit, version: Version) -> Result<Self> {
    use self::parse::*;

    let mut functions = vec![];
    let mut interfaces = vec![];

    for decl in translation_unit {
      if let ExternalDeclaration::FunctionDefinition(proto, body) = decl {
        functions.push((proto, body));
      } else if let ExternalDeclaration::Block(quals, name, members, _var_name) = decl {
        if quals.iter().any(|q| q == &TypeQualifier::Storage(StorageQualifier::Buffer)) {
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
              let array_size: u32 = array.iter().map(|a| a.as_ref().map(|a| a.eval()).unwrap_or(1)).sum();

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

          let info = ShaderStorageBlockInfo{
            name: name.clone(),
            binding: binding as u32,
            size: size,
            active_variables: active_variables,
          };
          interfaces.push(Interface::ShaderStorageBlock(info));
        }
      }
    }

    Ok(Shader{
      version,
      functions,
      interfaces,
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
