import Lean.Elab.Command

namespace Violet
open Lean

/-- exception for all violet produced errors -/
inductive Exception
  /-- errors from IO operations -/
  | io (e : IO.Error)
  /-- tmp error for now, should be replaced with `elab ElabError` -/
  | tmp (msg : String)
  /-- when getting some must existed argument, `hint` is what should be provided -/
  | arg (hint : String)

def exnToString : Exception → String
  | .io e => s!"{e}"
  | .tmp msg => msg
  | .arg hint => s!"Expected argument {hint}"
instance : ToString Exception where
  toString := exnToString
def Exception.toIO : Exception → IO.Error
  | .io e => e
  | e => IO.Error.userError <| toString e

structure Context where
  options : Options := default
  args : List String

abbrev CmdM := ReaderT Context (EIO Exception)

instance : MonadOptions CmdM where
  getOptions := return (← read).options

instance : MonadLiftT IO Violet.CmdM where
  monadLift k := λ _ => IO.toEIO Exception.io k

end Violet
