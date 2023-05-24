import «Violet»
open Violet.Parser (parseFile)
open Violet.Ast.Surface

def handleFile (src : System.FilePath) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile src
  let prog ← parseFile.runFilename src content
  prog.check

def main : IO Unit := do
  match (← handleFile "example/test.vt") with
  | .ok _ => pure ()
  | .error e => IO.eprintln e
