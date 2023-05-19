import «Violet»
open Violet.Parser (parseFile)

def handleFile (src : System.FilePath) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile src
  let result ← parseFile.runFilename src content
  IO.println <| repr result

def main : IO Unit := do
  match (← handleFile "example/test.vt") with
  | .ok _ => pure ()
  | .error e => IO.eprintln e
