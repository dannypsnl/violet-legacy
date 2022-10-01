use miette::NamedSource;
use miette::{Diagnostic, IntoDiagnostic, Result, SourceSpan};
use thiserror::Error;

pub mod ast;
pub mod parser;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(
    code(oops::my::bad),
    url(docsrs),
    help("try doing it better next time?")
)]
struct MyBad {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource,
    // Snippets and highlights can be included in the diagnostic!
    #[label("This bit here")]
    bad_bit: SourceSpan,
}

fn main() -> Result<()> {
    let src = "source\n  text\n    here".to_string();

    Err(MyBad {
        src: NamedSource::new("bad_file.rs", src),
        bad_bit: (9, 4).into(),
    })?;

    // parser::parse_file("example/hello.ss")?;
    Ok(())
}
