import «Violet»
open Violet.Parser (parseFile)
open Violet.Ast.Surface

def handleFile (src : System.FilePath) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile src
  let prog ← parseFile.runFilename src content
  prog.check src

def main (args : List String) : IO Unit := do
  let sourceFile := args.get! 0
  match (← handleFile sourceFile) with
  | .ok _ => pure ()
  | .error e => IO.eprintln e
