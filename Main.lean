import Violet
import Violet.CLI
import Violet.CliContext
open System
open Lean Meta Elab Command
open Violet.Parser (parseFile)
open Violet.Ast.Surface

register_option violet.verbose : Bool := {
  defValue := false
  descr := "provide verbose output to help debugging"
}

def readFileTo (f : Options → Program → System.FilePath → IO Unit) (src : FilePath) (opts : Options := default) : Violet.CmdM UInt32 := do
  let content ← IO.FS.readFile src
  match parseFile.runFilename src content with
    | .error ε => IO.eprintln ε; return 1
    | .ok prog => f opts prog src; return 0

def makeContext : StateT (List String) IO Violet.Context := do
  let mut opts := default

  if ←getOptionArg ["-v", "--verbose"] then opts := opts.setBool `violet.verbose true

  return {
    options := opts
    args := ←get
  }

def getArg (pos : Nat) : Violet.CmdM String := do
  let args := (← read).args
  match args[pos]? with
    | .none => return ""
    | .some x => return x

def check_cmd : Violet.CmdM UInt32 := do
  let file ← getArg 1
  readFileTo Program.check file (opts := ←getOptions)

unsafe def main (args : List String) : IO UInt32 := do
  enableInitializersExecution
  let (ctx, args) := ←makeContext.run args

  match args[0]? with
    | .some "check" =>
      return ←check_cmd.run ctx
    | .some file =>
      return ←(readFileTo Program.load file).run ctx
    | .none => IO.eprintln "Expected command"; return 1
