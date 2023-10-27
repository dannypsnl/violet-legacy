import Violet
import Violet.CLI
open System
open Lean Meta Elab Command
open Violet.Parser (parseFile)
open Violet.Ast.Surface

register_option violet.verbose : Bool := {
  defValue := false
  descr := "provide verbose output to help debugging"
}

def readFileTo (f : Program → FilePath → Violet.CmdM Unit) (src : FilePath) :
  Violet.CmdM Unit := do
  let content ← IO.FS.readFile src
  match parseFile.runFilename src content with
    | .error ε => throw <| .tmp ε
    | .ok prog => f prog src

def makeContext : StateT (List String) IO Violet.Context := do
  let mut opts := default

  if ←getOptionArg ["-v", "--verbose"] then opts := opts.setBool `violet.verbose true

  return {
    options := opts
    args := ←get
  }

def getArg (hint : String) (pos : Nat) : Violet.CmdM String := do
  let args := (← read).args
  match args[pos]? with
    | .none => throw $ Violet.Exception.arg hint
    | .some x => return x

def check_cmd : Violet.CmdM Unit := do
  let file ← getArg "<filename>" 1
  readFileTo Program.check file

unsafe def main (args : List String) : IO UInt32 := do
  enableInitializersExecution
  let (ctx, args) ← makeContext.run args

  match args[0]? with
    | .some "check" => check_cmd.liftIO ctx
    | .some file => (readFileTo Program.load file).liftIO ctx
    | .none => IO.eprintln "Expected command"; return 1

  return 0
