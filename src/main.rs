use miette::Result;

pub mod ast;
pub mod parser;

fn main() -> Result<()> {
    parser::parse_file("example/hello.ss")?;
    Ok(())
}
