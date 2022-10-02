pub mod ast;
pub mod parser;
pub mod tyck;
use miette::Result;

fn main() -> Result<()> {
    let path = "example/hello.ss";
    let result = parser::parse_module_path(path)?;
    tyck::check_module(path, result.source.as_str(), &result.top_list)?;
    Ok(())
}
