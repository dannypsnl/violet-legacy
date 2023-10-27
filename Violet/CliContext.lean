import Lean.Elab.Command

namespace Violet
open Lean

inductive Exception
  | io (e : IO.Error)
  | tmp (msg : String)

structure Context where
  options : Options := default
  args : List String

abbrev CmdM := ReaderT Context IO

instance : MonadOptions CmdM where
  getOptions := return (← read).options

end Violet
