import Violet.Core.Eval

namespace Violet.Core
open Violet.Ast.Core

structure ElabContext where
  env : Env
  lvl : Lvl
  typCtx : List (String × VTy)
  mctx : MetaCtx

end Violet.Core
