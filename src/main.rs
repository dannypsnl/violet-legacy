pub mod ast;
pub mod compile;
pub mod parser;
pub mod tyck;
use miette::Result;

fn main() -> Result<()> {
    let path = "example/hello.ss";
    let mod_file = parser::parse_module_path(path)?;
    tyck::check_module(&mod_file)?;
    compile::compile_module(mod_file)?;
    Ok(())
}
