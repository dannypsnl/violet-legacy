pub mod ast;
pub mod parser;

fn main() -> std::io::Result<()> {
    parser::parse_file("example/hello.ss")?;
    Ok(())
}
