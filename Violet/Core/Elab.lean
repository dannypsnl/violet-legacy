import Violet.Core.Eval

namespace Violet.Core
open Violet.Ast.Core

inductive MetaEntry
  | solved (v : Val)
  | unsolved

def MetaCtx := Array MetaEntry

structure ElabContext where
  env : Env
  lvl : Lvl
  typCtx : List (String × VTy)
  mctx : MetaCtx

end Violet.Core
