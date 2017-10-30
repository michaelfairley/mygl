use gl::GLenum;

mod lex;
mod parse;

pub type Result<T> = ::std::result::Result<T, String>;

#[derive(Debug,Clone,Copy,PartialEq)]
pub(super) enum Version {
  ES100,
  ES310,
  ES320,
}

#[allow(unused_variables)]
pub fn compile(source: &[u8], type_: GLenum) -> Result<()> {
  let source = String::from_utf8_lossy(source).to_owned();
  // TODO: is there a standard way to mutate this in place?
  let source = source
    .replace("\n\r", "\n")
    .replace("\r\n", "\r")
    .replace("\r", "\n");

  let version = lex::version(&source)?;
  let tokens = lex::tokenize(&source, version)?;
  let parse = parse::parse(&tokens, version)?;

  Ok(())
}
