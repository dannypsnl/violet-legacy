import Violet.Core.Eval

namespace Violet.Core
open Violet.Ast.Core

@[reducible]
abbrev TypCtx := List (String × VTy)

structure ElabContext where
  env : Env
  lvl : Lvl
  typCtx : TypCtx
  mctx : MetaCtx

end Violet.Core
