use miette::Result;

pub mod ast;
pub mod parser;
pub mod tyck;

fn main() -> Result<()> {
    let result = parser::parse_file("example/hello.ss")?;
    tyck::check_module(&result.top_list)?;
    Ok(())
}
