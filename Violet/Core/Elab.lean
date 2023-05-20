import Violet.Core.Eval
import Violet.Ast.Surface

namespace Violet.Core
open Violet.Ast

@[reducible]
abbrev TypCtx := List (String × VTy)

structure ElabContext where
  env : Env
  lvl : Lvl
  typCtx : TypCtx
  mctx : MetaCtx

def infer [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) : m (Core.Tm × VTy) := do
  match tm with
  | .var x =>
    match ctx.typCtx.lookup x with
    | .some a => return (.var x, a)
    | .none => throw s!"no variable named `{x}`"
  -- TODO: a good idea would be having two lambda forms
  -- 1. lam x => t
  -- 2. lam (x : T) => t
  --
  -- The first one cannot be inferred, but the second one can.
  | .lam x t => throw "cannot infer lambda without type annotation"
  | .app t u => sorry
  | .type => return (.type, .type)
  | _ => sorry

end Violet.Core
